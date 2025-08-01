{ postgres, pkgs, coddtests, hspecArgs }:
let fs = pkgs.lib.fileset;
in
 pkgs.stdenv.mkDerivation {
     name = "codd-test-with-db-results";
     src = fs.toSource {
      root = ../.;
      fileset = fs.unions [ ../conf/test-db ../test/migrations ../scripts/init-pg-cluster.sh ../scripts/wait-for-pg-ready.sh ];
     };
     nativeBuildInputs = [ postgres pkgs.bash pkgs.coreutils pkgs.glibcLocales ];
     installPhase = ''
      patchShebangs scripts/*.sh
      mkdir "$out"
      mkdir -p local/temp-pg-data
      export LANG=en_US.UTF-8
      export PGDATA="local/temp-pg-data"
      export PGDATABASE="postgres"
      export PGPORT="5434"
      export PGHOST="/tmp"
      export PGUSER="postgres"
      scripts/init-pg-cluster.sh ./conf/test-db
      trap "pg_ctl stop || true" EXIT ERR
      pg_ctl -l "$out/pg_ctl_init.log" start
      scripts/wait-for-pg-ready.sh
      # This isn't deterministic due to randomised testing and timing
      # information in the output, so we're really
      # abusing Nix's sandbox here, but it does makes life a lot easier.
      ${coddtests}/bin/codd-test ${hspecArgs} 2>&1 | tee "$out/haskell-tests.log"
      pg_ctl stop
      trap - EXIT ERR
      cp -R "$PGDATA/log" "$out/pglogs"
    '';
  }
