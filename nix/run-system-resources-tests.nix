{ postgres, pkgs, coddtests }:
let fs = pkgs.lib.fileset;
in
 pkgs.stdenv.mkDerivation {
     name = "codd-test-system-resources-results";
     src = fs.toSource {
      root = ../.;
      fileset = fs.unions [ ../conf/test-db ../expected-schema ../test/migrations ../scripts/init-pg-cluster.sh ../scripts/wait-for-pg-ready.sh ];
     };
     nativeBuildInputs = [ postgres pkgs.strace pkgs.bash pkgs.coreutils pkgs.glibcLocales ];
     installPhase = ''
      patchShebangs scripts/*.sh
      mkdir $out
      mkdir -p local/temp-pg-data
      export LANG=en_US.UTF-8
      export PGDATA="local/temp-pg-data"
      export PGDATABASE="postgres"
      export PGPORT="5434"
      export PGHOST="/tmp"
      export PGUSER="postgres"
      scripts/init-pg-cluster.sh ./conf/test-db
      pg_ctl -l "pg_ctl_init.log" start
      scripts/wait-for-pg-ready.sh
      strace -f -e openat,open,close -o /tmp/strace-codd-system-resources-test.log \
       ${coddtests}/bin/codd-test --match "/SystemResourcesSpecs/RUNNING"
      pg_ctl stop
      ${coddtests}/bin/codd-test --match "/SystemResourcesSpecs/CHECKING"
    '';
  }
