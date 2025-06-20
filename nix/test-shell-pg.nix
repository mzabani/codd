{ pkgs, postgres }:
pkgs.mkShell {
  buildInputs = [ postgres pkgs.coreutils pkgs.bash pkgs.glibcLocales pkgs.run ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ pkgs.strace ];
  description = "Test shell with postgres available and initializing";
  shellHook = ''
    set -eo pipefail

    export PGDATA="$(mktemp -d)"
    export PGDATABASE="postgres"
    export PGPORT="5434"
    export PGHOST="/tmp"
    export PGUSER="postgres"
    trap "pg_ctl stop" EXIT ERR
    scripts/init-pg-cluster.sh ./conf/test-db
    pg_ctl start
    scripts/wait-for-pg-ready.sh
  '';
}
