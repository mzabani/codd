{ postgres, pkgs, coddtests }:
let fs = pkgs.lib.fileset;
in
 pkgs.stdenv.mkDerivation {
     name = "codd-test-with-db-results";
     src = fs.toSource {
      root = ../.;
      fileset = fs.unions [ ../conf ../test/migrations ../scripts/init-pg-cluster.sh ../scripts/wait-for-pg-ready.sh ];
     };
     nativeBuildInputs = [ postgres pkgs.bash pkgs.coreutils pkgs.glibcLocales ];
     installPhase = ''
      patchShebangs scripts/*.sh
      mkdir "$out"
      mkdir -p local/temp-pg-data
      export PGDATA="local/temp-pg-data"
      export PGDATABASE="postgres"
      export PGPORT="5434"
      export PGHOST="/tmp"
      export PGUSER="postgres"
      scripts/init-pg-cluster.sh ./conf
      pg_ctl -l "$out/postgres.log" start
      echo After pg_ctl
      scripts/wait-for-pg-ready.sh
      # This isn't deterministic due to randomised testing, so we're really
      # abusing Nix's sandbox here, but it makes life a lot easier.
      ${coddtests}/bin/codd-test --match /DbDependentSpecs/ 2>&1 | tee "$out/haskell-tests.log"
      pg_ctl stop
    '';
  }
