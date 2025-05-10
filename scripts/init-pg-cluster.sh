#!/usr/bin/env bash

# $1 is the directory with config files to be copied into PGDATA

mkdir -p "$PGDATA"
if [ -d "$PGDATA" ] && [ "$(ls -A $PGDATA/*)" ]; then
  echo Postgres datadir "$PGDATA" not empty. Considering it initialized.
else
  # We use en_US.UTF-8 because MacOS has few locales and I don't want to learn how
  # to generate them or make glibcLocales available there
  initdb --locale=en_US.UTF-8 -E UTF8 -U postgres "$PGDATA"
fi

cp "$1"/* "$PGDATA"/
