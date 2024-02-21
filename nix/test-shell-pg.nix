{ pkgs, postgres, pgDataDir }:
let
  postgres-service = import ./postgres-service.nix {
    inherit postgres;
    inherit pkgs;
    initializePostgres = true;
    wipeCluster = true;
  };
in pkgs.mkShell {
  buildInputs = [ postgres postgres-service pkgs.glibcLocales pkgs.run ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ pkgs.strace ];
  description = "Test shell with postgres available and initializing";
  shellHook = ''
    set -e
    echo Going to initialize Postgres..
    export PGDATA="${pgDataDir}"
    export PGDATABASE="postgres"
    export PGPORT="5434"
    export PGHOST="127.0.0.1"
    export PGUSER="postgres"
    ${postgres-service}/bin/init-postgres
  '';
}
