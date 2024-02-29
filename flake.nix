{
  description = "Codd's flake";
  inputs.haskellNix.url =
    "github:input-output-hk/haskell.nix/2a1000b835ea4f8186b79e5926c99a80f9e354fc";
  # When switching away from nixpkgs-unstable, make sure to change
  # install-codd-nixpkgs.nix accordingly!
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  # We only have flake-compat here while we support nix-shell and
  # nix-build, i.e. while we support non-flakes Nix usage.
  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix, flake-compat }:
    flake-utils.lib.eachSystem [
      "x86_64-linux"
      "aarch64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ] (system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };

        postgres-service = import ./nix/postgres-service.nix {
          postgres = pkgs.postgresql_16;
          inherit pkgs;
          initializePostgres = false;
          wipeCluster = false;
        };
        overlays = [
          haskellNix.overlay
          (final: prev:
            let
              mkProject = stackYaml: compiler-nix-name:
                let
                  proj = final.haskell-nix.stackProject' {
                    src = ./.;
                    inherit compiler-nix-name stackYaml;

                    modules = [{
                      # Set to true to be able to run `cabal --enable-profiling`
                      enableLibraryProfiling = false;

                      # Work around https://github.com/input-output-hk/haskell.nix/issues/231. More info
                      # in codd.cabal
                      packages.codd.components.tests.codd-test.build-tools =
                        [ proj.hsPkgs.hspec-discover ];
                      }] ++ ([
                        {
                          packages.codd.components.exes.codd = {
                            dontStrip = false;
                            configureFlags = [
                              # I'm not sure how linking works. HMAC_Update and HMAC_Final are two symbols present both in
                              # libssl.a and libcrypto.a, but without including both linking will fail! It is also present
                              # in pgcommon_shlib (from postgres) but it doesn't work if it comes from there either.
                              # Also, the order of -lssl and -lcrypto is important here, and this doesn't seem to affect
                              # dynamically linked glibc builds.
                              # IMPORTANT: `postgresql` is postgresql 15, not 16. pg16 static builds are failing, see
                              # https://github.com/NixOS/nixpkgs/issues/191920
                              # This doesn't seem like a big issue since we only need libpq and we do run tests against
                              # postgresql-16-the-server.
                              "--ghc-option=-optl=-L${final.pkgsCross.musl64.openssl.out}/lib"
                              "--ghc-option=-optl=-lssl"
                              "--ghc-option=-optl=-lcrypto"

                              "--ghc-option=-optl=-L${final.pkgsCross.musl64.postgresql.out}/lib"
                              "--ghc-option=-optl=-lpgcommon"
                              "--ghc-option=-optl=-lpgport"
                            ];
                          };

                          packages.codd.components.tests.codd-test = {
                            dontStrip = false;
                            configureFlags = [
                              # Same as for the executable here
                              "--ghc-option=-optl=-L${final.pkgsCross.musl64.openssl.out}/lib"
                              "--ghc-option=-optl=-lssl"
                              "--ghc-option=-optl=-lcrypto"

                              "--ghc-option=-optl=-L${final.pkgsCross.musl64.postgresql.out}/lib"
                              "--ghc-option=-optl=-lpgcommon"
                              "--ghc-option=-optl=-lpgport"
                            ];
                          };
                        }
                      ]);


                    # This is used by `nix develop .` to open a shell for use with
                    # `cabal`, `hlint` and `haskell-language-server`
                    shell.tools = {
                      cabal = "latest";
                      hlint = "3.4.1"; # latest was failing cabal deps bounds
                      haskell-language-server = "2.4.0.0";
                    };
                    # Non-Haskell shell tools go here
                    shell.buildInputs = with pkgs; [
                      cacert
                      ghcid
                      glibcLocales
                      # haskellPackages.brittany # Brittany from the LTS is older than this
                      proj.hsPkgs.brittany.components.exes.brittany
                      postgres-service
                      postgresql_16
                      run
                      shellcheck
                    ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ strace ];
                    shell.shellHook = ''
                      source scripts/source-env.sh .env

                      # init-postgres doesn't actually work with direnv. I tried to daemonize starting postgres but was not able
                      # to make it work. See https://github.com/direnv/direnv/issues/755
                      init-postgres

                      echo You should be able to start postgres with 'pg_ctl start' and use 'psql' to connect to it, and it will be independent from any your own system might have provided.
                      echo If 'psql' fails to connect, check logs at $PGDATA/log/

                      # Postgres 15 insists in appearing in PATH before postgres 16.
                      # So we add postgres 16 _again_ to the beginning of the PATH, and also some useful scripts
                      export PATH="${pkgs.postgresql_16}/bin:$PATH:scripts/path"
                    '';
                    # This adds `js-unknown-linux-musl` to the shell.
                    # shell.crossPlatforms = p: [ p.musl64 ];
                  };
                in proj;
            in {
              # This overlay adds our project to pkgs
              coddProjectAeson1 = mkProject "stack.yaml" "ghc8107";
              coddProjectAeson2 = mkProject "stack-aeson-2.yaml" "ghc902";
            })
        ];

        flakeAeson2 = pkgs.coddProjectAeson2.flake {
          # This adds support for `nix build .#x86_64-unknown-linux-musl:codd:exe:codd`
          # and `nix build .#x86_64-w64-mingw32:codd:exe:codd`
          # Check nixpkgs.lib.systems.examples for more.
          # The mingwW64 build still fails, IIRC.
          crossPlatforms = p: [ p.musl64 p.mingwW64 p.aarch64-multiplatform-musl p.aarch64-darwin];
        };
        flakeAeson1 =
          pkgs.coddProjectAeson1.flake { crossPlatforms = p: [ p.musl64 ]; };
      in flakeAeson2 // {
        # Built by `nix build .`
        defaultPackage = flakeAeson2.packages."codd:exe:codd";

        # Aeson 1 is supported but only tested to compile without errors,
        # not actively tested.
        # To enter dev shell, run `nix develop .#flakeAeson1.x86_64-linux.devShell`
        # To build run `nix build .#flakeAeson1.x86_64-linux.codd-musl`
        flakeAeson1 = flakeAeson1 // {
          codd-musl =
            flakeAeson1.packages."x86_64-unknown-linux-musl:codd:exe:codd";
        };

        testShells = {
          pg12 = import ./nix/test-shell-pg12.nix { inherit pkgs; };
          pg13 = import ./nix/test-shell-pg13.nix { inherit pkgs; };
          pg14 = import ./nix/test-shell-pg14.nix { inherit pkgs; };
          pg15 = import ./nix/test-shell-pg15.nix { inherit pkgs; };
          pg16 = import ./nix/test-shell-pg16.nix { inherit pkgs; };
        };

        shellWithRunfile = pkgs.mkShell { buildInputs = [ pkgs.run pkgs.shellcheck ]; };

        # Having pkgs helps debug musl builds with `nix repl`. We can e.g.
        # build musl packages statically to see if their "normal" builds pass
        inherit pkgs;

        # Built with `nix build .#dockerImage.x86_64-linux`.
        dockerImage = import ./nix/docker/codd-exe.nix {
          inherit pkgs;
          codd-exe =
            flakeAeson2.packages."x86_64-unknown-linux-musl:codd:exe:codd";
        };
      });
}
