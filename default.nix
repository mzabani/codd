{ system ? builtins.currentSystem
, ghc ? "ghc9103"
, pkgs ? import ./nix/nixpkgs.nix { inherit system; }
}:
let
  addPgExtensions = postgres: postgres.withPackages (ps: [ ps.pg_cron ]);
  pkgsStaticLinking = if pkgs.stdenv.isDarwin
    then import ./nix/nixpkgs.nix { inherit system; staticLinking = true; }
    else import ./nix/nixpkgs.nix {
      inherit system;
      crossSystem = { config = "x86_64-unknown-linux-musl"; isStatic = true; };
      staticLinking = true;
    };
  pkgsDarwin = import ./nix/nixpkgs.nix { system = "aarch64-darwin"; };
  haskellPackages = builtins.getAttr ghc pkgs.haskell.packages;
  haskellPackagesStaticLinking = builtins.getAttr ghc pkgsStaticLinking.haskell.packages;

  justStatic = pkgsStaticLinking.haskell.lib.justStaticExecutables;

  coddexe = justStatic haskellPackagesStaticLinking.codd;
  coddtests = justStatic haskellPackagesStaticLinking.codd-tests;
  coddbenchmarks = haskellPackages.codd-benchmarks;
  coddhaddocks = haskellPackages.codd.doc;
in
{
  inherit coddexe coddtests coddbenchmarks coddhaddocks;
  inherit haskellPackages;

  dockerImage = import ./nix/docker/codd-exe.nix {
    inherit pkgs;
    inherit coddexe;
  };

  testsPg18 = { hspecArgs ? "--match /DbDependentSpecs/"}: import ./nix/run-db-tests.nix { inherit pkgs hspecArgs; codd-tests = coddtests; postgres = addPgExtensions pkgs.postgresql_18; };
  testsPg17 = { hspecArgs ? "--match /DbDependentSpecs/"}: import ./nix/run-db-tests.nix { inherit pkgs hspecArgs; codd-tests = coddtests; postgres = addPgExtensions pkgs.postgresql_17; };
  testsPg16 = { hspecArgs ? "--match /DbDependentSpecs/"}: import ./nix/run-db-tests.nix { inherit pkgs hspecArgs; codd-tests = coddtests; postgres = addPgExtensions pkgs.postgresql_16; };
  testsPg15 = { hspecArgs ? "--match /DbDependentSpecs/"}: import ./nix/run-db-tests.nix { inherit pkgs hspecArgs; codd-tests = coddtests; postgres = addPgExtensions pkgs.postgresql_15; };
  testsPg14 = { hspecArgs ? "--match /DbDependentSpecs/"}: import ./nix/run-db-tests.nix { inherit pkgs hspecArgs; codd-tests = coddtests; postgres = addPgExtensions pkgs.postgresql_14; };
  testsNoDb = { hspecArgs ? "--skip /DbDependentSpecs/ --skip /SystemResourcesSpecs/" }: import ./nix/run-no-db-tests.nix { inherit pkgs hspecArgs; codd-tests = coddtests; };
  testsSystemResources = import ./nix/run-system-resources-tests.nix { inherit pkgs; codd-tests = coddtests; postgres = addPgExtensions pkgs.postgresql_18; };

  # Shells with specific-versioned postgres servers to run tests locally
  shellPg18 = import ./nix/test-shell-pg.nix { inherit pkgs; postgres = addPgExtensions pkgs.postgresql_18; };
  shellPg17 = import ./nix/test-shell-pg.nix { inherit pkgs; postgres = addPgExtensions pkgs.postgresql_17; };
  shellPg16 = import ./nix/test-shell-pg.nix { inherit pkgs; postgres = addPgExtensions pkgs.postgresql_16; };
  shellPg15 = import ./nix/test-shell-pg.nix { inherit pkgs; postgres = addPgExtensions pkgs.postgresql_15; };
  shellPg14 = import ./nix/test-shell-pg.nix { inherit pkgs; postgres = addPgExtensions pkgs.postgresql_14; };

  shellForCITests = import ./nix/test-shell-ci.nix { inherit pkgs; };

  # Our Darwin app bundle. This only builds on aarch64-darwin
  darwinAppBundle = import ./nix/codd-darwin-bundle.nix { inherit coddexe; pkgs = pkgsDarwin; };
}
