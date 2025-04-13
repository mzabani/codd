{ postgres, pkgs, initializePostgres ? true, wipeCluster }:
# wipeCluster=true ensures this is a deterministic derivation (assuming initdb/postgres cluster initialisation is pure,
# which actually might not be true unless in a pure shell, since e.g. locales are imported from the system)
let
  utils = import ./utils.nix { inherit pkgs; };

  ls = "${pkgs.coreutils}/bin/ls";
  echo = "${pkgs.coreutils}/bin/echo";
  cat = "${pkgs.coreutils}/bin/cat";
in utils.writeShellScriptInBinFolder "init-postgres" ''
  ${if wipeCluster == true then "rm -rf $PGDATA" else ""}
  mkdir -p "$PGDATA"
  if [ -d "$PGDATA" ] && [ "$(${ls} -A $PGDATA/*)" ]; then
      ${echo} Postgres datadir not empty. Considering it initialized.
  else
      # We use en_US.UTF-8 because MacOS has few locales and I don't want to learn how
      # to generate them or make glibcLocales available there
      ${postgres}/bin/initdb --locale=en_US.UTF-8 -E UTF8 -U postgres $PGDATA
  fi

  ${if initializePostgres then ''
    set +e
    ${postgres}/bin/pg_ctl status -D $PGDATA -p ${postgres}/bin/postgres
    PGCTLSTATUS=$?
    set -e

    if [ "$PGCTLSTATUS" -eq "0" ]; then
        ${echo} Postgres already initialized.
    else
        echo All good, initializing postgres.
        ${cat} ${../conf/postgresql.conf} > $PGDATA/postgresql.conf
        ${cat} ${../conf/pg_hba.conf} > $PGDATA/pg_hba.conf
        ${postgres}/bin/postgres -D $PGDATA -p $PGPORT &

        # Wait up to 10 seconds until postgres is initialized
        timeout=10
        for ((i=0; i<timeout; i++)); do
            set +e
            ${postgres}/bin/pg_ctl status -D $PGDATA -p ${postgres}/bin/postgres
            PGCTLSTATUS=$?
            set -e
            if [ $PGCTLSTATUS -eq 0 ]; then
                break
            fi
            sleep 1
        done
    fi
  '' else ''
    ${cat} ${../conf/postgresql.conf} > $PGDATA/postgresql.conf
    ${cat} ${../conf/pg_hba.conf} > $PGDATA/pg_hba.conf
  ''}
''
