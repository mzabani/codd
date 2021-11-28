{ postgres, pkgs, initializePostgres ? true, wipeCluster }:
    # wipeCluster=true ensures this is a deterministic derivation (assuming initdb/postgres cluster initialisation is pure,
    # which actually might not be true unless in a pure shell, since e.g. locales are imported from the system)
    let
        utils = import ./utils.nix {};
        
        ls = "${pkgs.coreutils}/bin/ls";
        echo = "${pkgs.coreutils}/bin/echo";
        cat = "${pkgs.coreutils}/bin/cat";
        postgresql_conf = ../conf/postgresql.conf;
    in
        utils.writeShellScriptInBinFolder "init-postgres" ''
            ${if wipeCluster == true then "rm -rf $PGDATA" else ""}
            if [ -d "$PGDATA" ] && [ "$(${ls} -A $PGDATA/*)" ]; then
                ${echo} Postgres datadir not empty. Considering it initialized.
            else
                ${postgres}/bin/initdb --locale=C.UTF8 -E UTF8 -U postgres $PGDATA
            fi

            set +e
            ${postgres}/bin/pg_ctl status -D $PGDATA -p ${postgres}/bin/postgres
            PGCTLSTATUS=$?
            set -e

            ${if initializePostgres then ''
            if [ "$PGCTLSTATUS" -eq "0" ]; then
                ${echo} Postgres already initialized.
            else
                echo All good, initializing postgres.
                ${cat} ${postgresql_conf} > $PGDATA/postgresql.conf
                ${postgres}/bin/pg_ctl start
            fi
            '' else ""}
            ''