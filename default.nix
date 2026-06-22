{ system ? builtins.currentSystem, pkgs ? import ./nix/nixpkgs.nix { inherit system; }, useMuslIfPossible ? true }:
let
  pkgsMusl = if pkgs.stdenv.isDarwin || !useMuslIfPossible then pkgs else pkgs.pkgsCross.musl64;
  pkgsDarwin = import ./nix/nixpkgs.nix { system = "aarch64-darwin"; };
  addPgExtensions = postgres: postgres.withPackages (ps: [ ps.pg_cron ]);
  fs = pkgsMusl.lib.fileset;
  project = pkgsMusl.haskell-nix.stackProject' {
    src = fs.toSource {
      root = ./.;
      fileset = fs.gitTracked ./.;
    };
    stackYaml = "stack.yaml";
    compiler-nix-name = "ghc984";

    modules =
      [
        {
          # Set to true to be able to run `cabal --enable-profiling`
          enableLibraryProfiling = false;

          # Work around https://github.com/input-output-hk/haskell.nix/issues/231. More info
          # in codd.cabal
          packages.codd.components.tests.codd-test.build-tools = [ project.hsPkgs.hspec-discover ];
        }
      ]
      ++ (
        if pkgs.stdenv.isDarwin then
          [ ]
        else
          let
            # I'm not sure how linking works. HMAC_Update and HMAC_Final are two symbols present both in
            # libssl.a and libcrypto.a, but without including both linking will fail! It is also present
            # in pgcommon_shlib (from postgres) but it doesn't work if it comes from there either.
            # Also, the order of -lssl and -lcrypto is important here, and this doesn't seem to affect
            # dynamically linked glibc builds.
            # The musl overlay maps `postgresql` to the standalone `libpq` package
            # (see nix/nixpkgs.nix) to avoid the full server's clang/LTO build.
            # Static archives (.a) are in the dev output.
            muslConfigureFlags = [
              "--ghc-option=-optl=-L${pkgsMusl.openssl.out}/lib"
              "--ghc-option=-optl=-lssl"
              "--ghc-option=-optl=-lcrypto"

              "--ghc-option=-optl=-L${pkgsMusl.postgresql.dev}/lib"
              "--ghc-option=-optl=-lpgcommon"
              "--ghc-option=-optl=-lpgport"
            ];
          in
          [
            {
              # Ensure pg_config is in PATH for the postgresql-libpq-configure
              # autoconf script during cross-compilation (it's in buildInputs
              # via haskell.nix's configuration-nix.nix, but for cross builds
              # it needs to be in nativeBuildInputs to appear in PATH).
              packages.postgresql-libpq-configure.components.library.build-tools = [ pkgsMusl.postgresql.pg_config ];

              # Apply the same configureFlags to all components so that
              # haskell.nix doesn't recompile the library inside exe/test
              # derivations due to a configuration mismatch.
              packages.codd.components.library.configureFlags = muslConfigureFlags;
              packages.codd.components.exes.codd = {
                dontStrip = false;
                configureFlags = muslConfigureFlags;
              };

              packages.codd.components.tests.codd-test = {
                dontStrip = false;
                configureFlags = muslConfigureFlags;
              };
            }
          ]
      );
  };
  coddexe = project.hsPkgs.codd.components.exes.codd;
  coddtests = project.hsPkgs.codd.components.tests.codd-test;
  coddbenchmarks = project.hsPkgs.codd.components.benchmarks.codd-bench;
  coddhaddocks = project.hsPkgs.codd.components.library.doc;
in
{
  inherit project;
  inherit coddexe coddtests coddbenchmarks coddhaddocks;
  dockerImage = import ./nix/docker/codd-exe.nix {
          inherit pkgs;
          inherit coddexe;
        };

  testsPg18 = { hspecArgs ? "--match /DbDependentSpecs/"}: import ./nix/run-db-tests.nix { inherit pkgs coddtests hspecArgs; postgres = addPgExtensions pkgs.postgresql_18; };
  testsPg17 = { hspecArgs ? "--match /DbDependentSpecs/"}: import ./nix/run-db-tests.nix { inherit pkgs coddtests hspecArgs; postgres = addPgExtensions pkgs.postgresql_17; };
  testsPg16 = { hspecArgs ? "--match /DbDependentSpecs/"}: import ./nix/run-db-tests.nix { inherit pkgs coddtests hspecArgs; postgres = addPgExtensions pkgs.postgresql_16; };
  testsPg15 = { hspecArgs ? "--match /DbDependentSpecs/"}: import ./nix/run-db-tests.nix { inherit pkgs coddtests hspecArgs; postgres = addPgExtensions pkgs.postgresql_15; };
  testsPg14 = { hspecArgs ? "--match /DbDependentSpecs/"}: import ./nix/run-db-tests.nix { inherit pkgs coddtests hspecArgs; postgres = addPgExtensions pkgs.postgresql_14; };
  testsNoDb = { hspecArgs ? "--skip /DbDependentSpecs/ --skip /SystemResourcesSpecs/" }: import ./nix/run-no-db-tests.nix { inherit pkgs coddtests hspecArgs; };
  testsSystemResources = import ./nix/run-system-resources-tests.nix { inherit pkgs coddtests; postgres = addPgExtensions pkgs.postgresql_16; };

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
