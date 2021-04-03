# SQL Migrations and Codd

Most of the time, you'll be able to simply add migrations and things should work well. However, there are cases where things get tricky, so this guide should help if you're in such a situation.  

<!-- vscode-markdown-toc -->
- [SQL Migrations and Codd](#sql-migrations-and-codd)
  - [<a name='Usingno-txnmigrations'></a>Using no-txn migrations](#using-no-txn-migrations)
  - [<a name='UnsupportedSQLinsidemigrations'></a>Unsupported SQL inside migrations](#unsupported-sql-inside-migrations)

<!-- vscode-markdown-toc-config
	numbering=false
	autoSave=true
	/vscode-markdown-toc-config -->
<!-- /vscode-markdown-toc -->

## <a name='Usingno-txnmigrations'></a>Using no-txn migrations

By using `no-txn` migrations, you're taking great risk with the possibility of a migration failing when deploying and leaving the database in an intermediary state that is not compatible with the previously deployed application nor the to-be-deployed one. It is recommended that you avoid these at great costs and plan carefully when adding even one of them.  

_Codd_ will always run each block of consecutive `in-txn` migrations in a single transaction. If there are blocks of `in-txn` migrations intertwined with `no-txn` migrations, each consecutive block runs either in a transaction or outside a transaction, accordingly. Also, if even one `no-txn` migration exists, _codd_ will apply and commit every pending migration and will run a **soft schema check** at the end, meaning it iwll not fail if schemas mismatch, but will show differences between expected and actual schemas.  

## <a name='UnsupportedSQLinsidemigrations'></a>Unsupported SQL inside migrations

_Codd_ does not support any SQL statement inside a migration. This is an incomplete list of things that we know _codd_ does not support.  

1. `COPY FROM STDIN` is supported but other forms of `COPY` are not.  
2. psql's meta commands, including `\COPY`, are not supported.  

 If you find a problem, please let us know. 