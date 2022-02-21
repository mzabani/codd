# SQL Migrations and Codd

Most of the time, you'll be able to simply add migrations and things should work well. However, there are cases where things get tricky, so this guide should help if you're in such a situation.  

<!-- vscode-markdown-toc -->
- [SQL Migrations and Codd](#sql-migrations-and-codd)
	- [Using no-txn or custom-connection migrations](#using-no-txn-or-custom-connection-migrations)
	- [Unsupported SQL inside migrations](#unsupported-sql-inside-migrations)
	- [Retry Policies](#retry-policies)

<!-- vscode-markdown-toc-config
	numbering=false
	autoSave=true
	/vscode-markdown-toc-config -->
<!-- /vscode-markdown-toc -->

## Using no-txn or custom-connection migrations

By using `no-txn` migrations or migrations with a custom connection string, you're taking great risk with the possibility of a migration failing when deploying and leaving the database in an intermediary state that is not compatible with the previously deployed application nor the to-be-deployed one. It is recommended that you avoid these at great costs and plan carefully when adding even one of them.  

_Codd_ will always run each block of consecutive `in-txn` migrations with the same connection string in a single transaction. If there are `in-txn` migrations intertwined with `no-txn` migrations or migrations with custom connection strings, every block of consecutive `in-txn` and `same-connection-string` migrations runs in the same transaction, but other migrations run separately. Also, if even one `no-txn` migration or one migration with a custom connection string exists, _codd_ will apply and commit every pending migration and verify checksums only after that.  

## Unsupported SQL inside migrations

_Codd_ does not support all SQL statements inside a migration. This is an incomplete list of things that we know _codd_ does not support.  

1. `COPY FROM STDIN` is supported but other forms of `COPY` are not.  
2. psql's meta commands, including `\COPY`, are not supported.  

If you find a problem, please let us know. 

## Retry Policies

A migration can fail for a variety of reasons, including unhandled data and serializability errors when using Serializable transactions. For this reason, _codd_ comes with a default Retry Policy of 3 tries (at most 2 retries), the first retry attempt 1 second after the first fails, and the second retry attempt 2 seconds after the second one fails. This can be configured with the `CODD_RETRY_POLICY` environment variable as exemplified in [README.md](../README.md). Important observations are:

- When faced with a block of consecutive `in-txn`  migrations, _codd_ retries the blocks whole.
  - For these, the retry count and intervals are "reset" for each block.
- For `no-txn` migrations, _codd_ retries individual statements, not even entire migrations.
  - Otherwise retries would lead to possibly inconsistent data.
  - The retry count and intervals are also "reset" for each statement.
