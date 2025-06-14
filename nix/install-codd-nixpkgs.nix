# This is a "vanilla" nixpkgs cabal2nix derivation.
# The upside of having this is that it has a much smaller closure
# than a haskell.nix provided one, so it's useful as an installation option.
# The downside is that library versions are likely not the same as the ones from Stackage LTS.
let
  pkgs = import ./nixpkgs.nix {};
  coddFull = pkgs.haskell.lib.dontHaddock
    (pkgs.haskellPackages.callCabal2nixWithOptions "codd" ../. "" { });
in rec {
  inherit pkgs;
  # A derivation with tests so we're more comfortable that this release of codd
  # isn't completely broken just because it uses different library versions.
  coddWithCheck = coddFull.overrideAttrs (self: {
    checkPhase = ''
      patchShebangs scripts/*.sh
      export PGDATA="./cabal2nix-codd-datadir"
      export PGDATABASE="postgres"
      export PGPORT="5434"
      export PGHOST="/tmp"
      export PGUSER="postgres"
      export PATH="$PATH:${pkgs.postgresql_16.withPackages (ps: with ps; [ pg_cron ])}/bin" # Some tests require pg_dump in PATH
      export HSPEC_SKIP="/SystemResourcesSpecs/" # This test requires strace-wrapping, and I don't expect different libs would make it fail anyway
      scripts/init-pg-cluster.sh ./conf
      trap "pg_ctl stop || true" EXIT ERR
      pg_ctl start
      scripts/wait-for-pg-ready.sh
    '' + self.checkPhase + ''
      pg_ctl stop
      trap - EXIT ERR
    '';

  });
  codd = pkgs.haskell.lib.dontCheck coddFull;
}
