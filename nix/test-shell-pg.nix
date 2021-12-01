{ pkgs ? import ./nixpkgs.nix
, postgres
, pgDataDir }:
    let
        postgres-service = import ./postgres-service.nix { inherit postgres; inherit pkgs; initializePostgres = true; wipeCluster = true; };
    in
    pkgs.mkShell {
        buildInputs = [ postgres-service pkgs.glibcLocales ];
        shellHook = ''
            set -e
            trap "${postgres}/bin/pg_ctl stop" EXIT INT
            echo Going to initialize Postgres..
            export PGDATA="${pgDataDir}"
            export PGDATABASE="postgres"
            export PGPORT="5434"
            export PGHOST="127.0.0.1"
            export PGUSER="postgres"
            ${postgres-service}/bin/init-postgres
        '';
    }