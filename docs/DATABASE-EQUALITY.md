# Database equality implementation

<!-- vscode-markdown-toc -->
- [Database equality implementation](#database-equality-implementation)
  - [<a name='Whatdoescoddchecksum'></a>What does codd checksum?](#what-does-codd-checksum)
  - [<a name='Gotchas'></a>Gotchas](#gotchas)
    - [<a name='Delayedeffectinpg_catalog'></a>Delayed effect in pg_catalog](#delayed-effect-in-pg_catalog)

<!-- vscode-markdown-toc-config
	numbering=false
	autoSave=true
	/vscode-markdown-toc-config -->
<!-- /vscode-markdown-toc -->

## <a name='Whatdoescoddchecksum'></a>What does codd checksum?
 For a more thorough - but a bit drafty and _not necessarily up to date_ - description of what is checksummed, see [SCHEMA-MAPPINGS.md](SCHEMA-MAPPINGS.md). What follows is an incomplete list of what currently is checksummed, **but be aware** that not all pertinent attributes are necessarily included:

- Tables, columns, CHECK constraints, FKs, indexes and other constraints
- Indexes
- Sequences (although their _RESTART_ value are not currently checked)
- Functions, operators and VIEWs
- Triggers
- Row Level Security
- Roles, including their config attributes such as `search_path`, which other roles they belong to and database-related permissions
- Database encoding and its `default_transaction_*` settings

In contrast, an **incomplete** list of things that are **not currently checksummed:**

- Collations
- Extensions
- Partitioning
- Foreign Servers
- Others

**The final word** on what _codd_ checksums can be found in an automated test at _HashingSpec.hs_.

Checksumming every possible object is a top priority of *codd*, so if something's missing or not behaving as intended, please file a bug report.  

## <a name='Gotchas'></a>Gotchas

### <a name='Delayedeffectinpg_catalog'></a>Delayed effect in pg_catalog

Suppose you add the following migration:

````sql
ALTER DATABASE my_database SET default_transaction_isolation TO 'serializable';
````

_Codd_ will fail when this is added (TODO: only after https://github.com/mzabani/codd/issues/40 is fixed).

Database settings are not visible anywhere in `pg_catalog` (as far as the author of _codd_ knows) in the same connection that changed them. Because of this, _codd_ has no way to checksum the new value for things like *default_transaction_isolation* before `COMMIT`. In cases like this we recommend adding `SET default_transaction_isolation TO 'serializable';` to the migration, which do have an immediate effect.