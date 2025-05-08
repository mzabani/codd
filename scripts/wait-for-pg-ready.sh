#!/usr/bin/env bash

set -eo pipefail

pg_ctl status -D "$PGDATA" || PGCTLSTATUS=$?

if [ "$PGCTLSTATUS" = "0" ]; then
  echo Postgres already initialized.
else
  echo Initializing postgres.
  postgres -D "$PGDATA" -p $PGPORT &

  # Wait up to 10 seconds until postgres is initialized
  timeout=10
  for ((i=0; i<timeout; i++)); do
      set +e
      pg_ctl status -D "$PGDATA"
      PGCTLSTATUS=$?
      set -e
      if [ $PGCTLSTATUS -eq 0 ]; then
          break
      fi
      sleep 1
  done
fi
