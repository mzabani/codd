# How to best upgrade your DB's schema in Production?

Suppose we want to run SQL migrations when we Blue-Green Deploy our Apps in a way that neither the Old App or the New App can fail. How would we do that?

Let us start with the following SQL migration and draft some ideas:

```sql
ALTER TABLE employee ADD COLUMN birthday DATE;
ALTER TABLE employee DROP COLUMN middle_name;
```

There's a big problem with this migration: the old App won't work after it's applied due to the lack of an expected column. Let's break it into two parts, then:

```sql
-- non-destructive section
-- Anything in this section will not break the Old App after applied.
ALTER TABLE employee ADD COLUMN birthday DATE;

-- destructive section
-- SQL in this section can break the Old App
ALTER TABLE employee DROP COLUMN middle_name;
```

That looks like a reasonable start. I guess we can come up with an algorithm to apply these changes when an App is deployed now. How about this:

1. BEGIN; Run the non-destructive sections of all new migrations; COMMIT;
   1.1. If step 1 fails for any reason, abort deployment completely. The DB state is unchanged and the old App is still running.
2. Redirect every new Request to the new App now.
3. Wait for the Old App to stop serving its Requests. Shut down the old App.
4. After the old App shuts down, do BEGIN; Run destructive sections of all new migrations; COMMIT;

This looks reasonable, but there is a problem with it: Between steps 1 and 4, both the old App and the new App run on a partially modified DB State - one which they haven't been tested against thoroughly. This is not a major concern since this stage should be short-lived, and the separation of non-destructive and destructive sections adds a lot of safety, but it's aggravated by the fact that Step 4 may fail. If that happens, the situation may be a bit dire, because as developers and on CI we'll be testing our App only against a fully modified (i.e. after Step 4) DB state.

What can we do about this? If we could do everything in a single transaction, there would only be two DB States: the previous one and the new one. But destructive updates would then mean that for a while the Old App could break. Here's one idea, then:

1. BEGIN;
   **Run the destructive sections of all migrations whose non-destructive sections are already applied**
   Run the non-destructive sections of all new migrations;
   COMMIT;
   1.1. If step 1 fails for any reason, abort deployment completely. The DB state is unchanged and the old App is still running.
2. Redirect every new Request to the new App now.
3. Wait for the Old App to stop serving its Requests. Shut down the old App.
   
How about this?

- For our example SQL migration, the `middle_name` column will only be dropped in a second Deploy, when the App no longer relies on it.
- However, applying the same set of migrations is no longer idempotent. It's only idempotent from the second run on. This is a bit odd, but let's accept it for now.
- While we can make CI run tests against a DB State that is the same as the one the new App will use, there's still an interval of time when the old App will run against an untested DB State. This is a much smaller concern now that we know this should be temporary (unless starting the new App fails for non-DB related reasons).

## Setting up CI

**Important**: On CI, you should **ALWAYS**

1. Run the migration on a Database with the same set of already applied Migrations that you have in the Database which is currently used by the Apps you'll deploy.
2. Require PRs to be up-to-date with `master` before merging.
TODO: Why must we always do these?

**Extra**: On CI, you _might want_, if you want to be very careful, to run your old App's test-suite against the new DB State to test the old App's behaviour while the new one hasn't come up yet. This adds a lot of build time to CI only to test a very short-lasting state during Deploy. Due to that, you can probably only run this for Deployment builds instead of for every build.

## For development

Testing our Apps with automated tests on CI is important, but as developers we are also an important part of testing our changes. We run our Apps and use them ourselves during the process of development and before pushing, and we want to make sure we're testing the same DB State that will be running on Production.
To achieve that, we want to run only the non-destructive sections of our newly added migrations, but want to have the same set of destructive migrations that have run in Production run locally.

It is not trivial to assure this property, though. A perfect local development and CI setup would have to:

- Make it possible for Dev machines to query the last deployed Commit. One way to do this would be for CI to automatically push a commit with an on-disk (e.g. a file) indication with the names of the SQL migrations whose destructive sections have run in Production. This would be fed to `codd` so that it only applies destructive sections of the same migrations.

Because many setups will not include this, we provide ways to run the destructive sections of already-run SQL migrations, so that you can periodically run it (THIS SOUNDS TERRIBLE).

## Problematic SQL migrations

### Avoiding too much work by accepting short-lived possible breakage

Suppose we want to apply the following migration:
```sql
ALTER TABLE employee RENAME COLUMN birthday to birthdate;
```

Of course, this would break the old App. How do we rename a column, then?

```sql
-- codd: non-destructive
ALTER TABLE employee ADD COLUMN birthdate DATE;
UPDATE employee SET birthdate=birthday;

-- codd: destructive
ALTER TABLE employee DROP COLUMN birthday;
```

But there is still a problem. The old App might insert new rows with NULL `birthdate`, which might not break the App, but could lead to a problem where a User updates their birthday and in the next Request find their birthdate is gone!
To make this migration work well, we'd have to set up triggers to set `birthdate` when `birthday` is updated*, but that might just be too much work, and maybe we don't want to go to such lengths only to ensure the old App remains working for a short while. Maybe we're fine with breaking the old App for a very short while and just want to `RENAME COLUMN`. If that's the case, you can force run a migration with a bit more markup:

```sql
-- codd: force, non-destructive
ALTER TABLE employee RENAME COLUMN birthday to birthdate;
```

Codd considers renaming and dropping columns as destructive actions, so you have to force your hand.

*: Or you can do a two-stage deployment by changing all your queries to select `COALESCE(birthday, birthdate)`, setting both `birthday` and `birthdate` in your insertions and updates and having the `UPDATE employee` query in the destructive section of your migration. More about this strategy in the next section.

### No-txn SQL migrations 

Suppose we want to apply this migration when using Postgres version < 12:

```sql
ALTER TYPE employee_type ADD VALUE 'manager' AFTER 'supervisor';
UPDATE employee SET emptype='manager' WHERE emptype='supervisor';
```

Sadly, Postgres cannot run it inside a Transaction. You have to tell Codd about it, though:

```sql
-- codd: non-destructive, no-txn
ALTER TYPE employee_type ADD VALUE 'manager' AFTER 'supervisor';

-- codd: destructive
UPDATE employee SET emptype='manager' WHERE emptype='supervisor';
```

The migration above has other interesting complications. Have you wondered about both new and old Apps running at the same time? The old App can't parse `'manager'` values and the new one **must be able to parse** both `'manager'` and `'supervisor'` into the same internal representation. But if some Request in the new App inserts a row with `'manager'` instead of `'supervisor'`, the old App can break.  
The best way to handle these situations is to do a two-stage deployment: first deploy an App that can parse both `'manager'` and `'supervisor'` into the same internal representation, but still inserts `'supervisor'` into the DB. A second deploy would then contain an App that doesn't need to know how to parse `'supervisor'`.

**Important:** Codd runs all pending `no-txn` migrations intertwined with Transaction blocks for consecutive pending `in-txn` migrations. However, because migrations can fail, when using `no-txn` migrations, your old App could be left running with a DB State that's the result of applying a few of the migrations, but not all of them. Splitting migrations into destructive and non-destructive sections alleviates this problem, but it's on you to be _really_ careful about this.

## Anatomy of a SQL migration

Every migration must have the following structure:

```sql
-- codd: ['no-txn' | 'in-txn' | '' ], ['force' | ''], non-destructive
SQL commands of non-destructive section

-- The destructive section must come after the non-destructive section and is optional
-- codd: ['no-txn' | 'in-txn' | '' ], destructive
SQL commands of the destructive section
```