# Database equality implementation

The goal of checksumming the database is to have the same query yield exactly the same results in two databases whose checksums match and data is the same (except for non-determinism such as no guarantee of order unless `ORDER BY` is specified and other similar things).

This is not currently the case, may not be entirely possible and some decisions made already involve tradeoffs. The rationale behind some decisions isn't always obvious either, so this section discusses some of those.

<!-- vscode-markdown-toc -->
- [Database equality implementation](#database-equality-implementation)
  - [What does codd checksum?](#what-does-codd-checksum)
  - [Details and gotchas](#details-and-gotchas)
    - [Delayed effect in pg_catalog](#delayed-effect-in-pg_catalog)
    - [System-dependent collations in the System Catalog Schema](#system-dependent-collations-in-the-system-catalog-schema)
    - [System libraries and collations](#system-libraries-and-collations)

<!-- vscode-markdown-toc-config
	numbering=false
	autoSave=false
	/vscode-markdown-toc-config -->
<!-- /vscode-markdown-toc -->

## What does codd checksum?

 For a more thorough - but a bit drafty and _not necessarily up to date_ - description of what is checksummed, see [SCHEMA-MAPPINGS.md](SCHEMA-MAPPINGS.md). What follows is an incomplete list of what currently is checksummed, **but be aware** there may be caveats:

- Tables, columns, CHECK constraints, FKs, indexes, and other constraints
- Indexes
- Sequences (although their _RESTART_ value is not currently checked)
- Functions, operators, and VIEWs
- Triggers
- Row Level Security
- Collations (with _important caveats_)
- Roles, including their config attributes such as `search_path`, which other roles they belong to and database-related permissions
- Database encoding and its `default_transaction_*` settings

In contrast, an **incomplete** list of things that are **not currently checksummed:**

- Types
- Full-text dictionaries
- Extensions
- Partitioning
- Foreign Servers
- Others

**The final word** on what _codd_ checksums can be found in an automated test at _HashingSpec.hs_ and the implementation in the _Pg10.hs_, _Pg11.hs_, _Pg12.hs_, _..._ files.

Checksumming every possible object is a top priority of _codd_, so if something's missing or not behaving as intended, please file a bug report.  

## Details and gotchas

### Delayed effect in pg_catalog

Suppose you add the following migration:

````sql
ALTER DATABASE my_database SET default_transaction_isolation TO 'serializable';
````

_Codd_ will fail when this is added (TODO: only after <https://github.com/mzabani/codd/issues/40> gets fixed).

Database settings are not visible anywhere in `pg_catalog` (as far as the author of _codd_ knows) in the same connection that changed them. Because of this, _codd_ has no way to checksum the new value for things like *default_transaction_isolation* before `COMMIT`. In cases like this, we recommend adding `SET default_transaction_isolation TO 'serializable'` to the migration, which does have an immediate effect.

### System-dependent collations in the System Catalog Schema

The system catalog is a namespace called `pg_catalog` that comes with internal functions, operators, collations, and other database objects native to postgres.

It is always in the search path regardless of the `search_path` setting and its objects take precedence over homonomous objects in other schemas. When you write `SELECT 1+2`, this is effectively the same as using the `+` operator from `pg_catalog`, and could be written as `SELECT 1 OPERATOR(pg_catalog.+) 2`.

The problem is that while this is fine for most things, some objects in the System Catalog are system-dependent. Collations are one such example.

According to <https://www.postgresql.org/docs/13/collation.html>, "initdb populates the system catalog pg_collation with collations based on all the locales it finds in the operating system at the time".

For an example look at _psql's_ `\dOS+`, pick a collation from `pg_catalog` and run the following query:

```sql
-- Use a collation you have in your system if this one (Cherokee) doesn't work
SELECT 'abc' < 'abd' COLLATE "chr-x-icu";
```

The collation might exist in the development database but might not in the production server, which can make checksums match but queries fail in one server and work in another.

One could think including `pg_catalog` in the list of namespaces to be checksummed would fix this, but even if development databases contain a superset of production's collations, _codd_ only does equality checks at the moment.

### System libraries and collations

This section is more of the informative kind. This is handled by _codd_ in a way that is likely what you expect.

Collations are implemented by calling into system libraries such as _libc_ or _icu_. This means different binaries of the same PostgreSQL version can be linked to different versions of such libraries, which can lead to different collation behavior.
It is possible to detect when versioned _icu_ or _libc_ provided collations are running in a postgres server linked to different versions of these libraries than the one collations were created with, because postgres will emit warnings like this one:

`WARNING:  collation "xx-x-icu" has version mismatch`

This means that the version of the _icu_ or _libc_ library a collation was created with does not always determine a collation's behavior; what does is the system library the server is linked to. You can see both by querying:

```sql
-- Check the versions of system libraries collations were created with and the versions the postgres server is linked to
select oid, collname, collprovider, collversion AS created_with_version, pg_collation_actual_version(oid) AS system_library_version from pg_catalog.pg_collation;
```

The decision made in _codd_ is to checksum both the _created-with_ version and the _current-library_ version for each collation. Checksumming only _current-library_ versions could trigger version mismatch warnings on one server but not on the other even though checksums match, and checksumming only _created-with_ versions could even lead to different behavior although checksums match.
