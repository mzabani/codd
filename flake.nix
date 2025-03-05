{
  description = "Codd's flake";
  inputs.haskellNix.url =
    "github:input-output-hk/haskell.nix/4b723bfac41d8ac61dbc9a4ca47b5507c67b6911";
  # When switching away from nixpkgs-unstable, make sure to change
  # nixpkgs.nix accordingly!
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-2411";
  # Some green staging build taken from Hydra: https://hydra.nixos.org/eval/1812308#tabs-inputs
  # inputs.nixpkgs.url = "github:NixOS/nixpkgs/51b93f39abfb9e566dba11b5e57e00d3e18357e8";
  # inputs.haskellNix.inputs.nixpkgs.follows = "nixpkgs";
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
          postgres = pkgs.postgresql_17;
          inherit pkgs;
          initializePostgres = false;
          wipeCluster = false;
        };
        overlays = [
          haskellNix.overlay
          # (self: super: {
          #   pkgsCross = super.pkgsCross // {
          #     musl64 = super.pkgsCross.musl64 // {
          #       postgresql = self.pkgsStatic.postgresql;
          #     };
          #   };
          # })
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
                    }] ++ (if final.stdenv.isDarwin then
                      [ ]
                    else [{
                      packages.codd.components.exes.codd = {
                        dontStrip = false;
                        configureFlags = [
                          # I'm not sure how linking works. HMAC_Update and HMAC_Final are two symbols present both in
                          # libssl.a and libcrypto.a, but without including both linking will fail! It is also present
                          # in pgcommon_shlib (from postgres) but it doesn't work if it comes from there either.
                          # Also, the order of -lssl and -lcrypto is important here, and this doesn't seem to affect
                          # dynamically linked glibc builds.
                          "--ghc-option=-optl=-L${final.pkgsCross.musl64.openssl.out}/lib"
                          "--ghc-option=-optl=-lssl"
                          "--ghc-option=-optl=-lcrypto"

                          "--ghc-option=-optl=-L${pkgs.pkgsStatic.postgresql.dev}/lib"
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

                          "--ghc-option=-optl=-L${pkgs.pkgsStatic.postgresql.dev}/lib"
                          "--ghc-option=-optl=-lpgcommon"
                          "--ghc-option=-optl=-lpgport"
                        ];
                      };
                    }]);

                    # This is used by `nix develop .` to open a shell for use with
                    # `cabal`, `hlint` and `haskell-language-server`
                    shell.tools = {
                      cabal = "latest";
                      hlint = "latest";
                      haskell-language-server = "latest";
                      fourmolu = "latest";
                    };
                    # Non-Haskell shell tools go here
                    shell.buildInputs = with pkgs;
                      [
                        cacert
                        ghcid
                        glibcLocales
                        hyperfine
                        postgres-service
                        postgresql_17
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

                      # Add some useful scripts to PATH
                      export PATH="${pkgs.postgresql_17}/bin:$PATH:scripts/path"
                    '';
                    # This adds `js-unknown-linux-musl` to the shell.
                    # shell.crossPlatforms = p: [ p.musl64 ];
                  };
                in proj;
            in {
              # This overlay adds our project to pkgs
              coddProject = mkProject "stack.yaml" "ghc966";
            })
        ];

        flakeDefault = pkgs.coddProject.flake {
          # This adds support for `nix build .#x86_64-unknown-linux-musl:codd:exe:codd`
          # and `nix build .#x86_64-w64-mingw32:codd:exe:codd`
          # Check nixpkgs.lib.systems for more.
          # The mingwW64 build still fails, IIRC.
          crossPlatforms = p: [ p.musl64 p.mingwW64 ];
        };
      in flakeDefault // {
        # Built by `nix build .`
        defaultPackage = flakeDefault.packages."codd:exe:codd";

        devShells = flakeDefault.devShells // {
          pg12 = import ./nix/test-shell-pg12.nix { inherit pkgs; };
          pg13 = import ./nix/test-shell-pg13.nix { inherit pkgs; };
          pg14 = import ./nix/test-shell-pg14.nix { inherit pkgs; };
          pg15 = import ./nix/test-shell-pg15.nix { inherit pkgs; };
          pg16 = import ./nix/test-shell-pg16.nix { inherit pkgs; };
          shellWithRunfile =
            pkgs.mkShell { buildInputs = [ pkgs.run pkgs.shellcheck ]; };
        };

        # Having pkgs helps debug musl builds with `nix repl`. We can e.g.
        # build musl packages statically to see if their "normal" builds pass
        inherit pkgs;

        # Built with `nix build .#dockerImage.x86_64-linux`.
        dockerImage = import ./nix/docker/codd-exe.nix {
          inherit pkgs;
          codd-exe =
            flakeDefault.packages."x86_64-unknown-linux-musl:codd:exe:codd";
        };

        coddDarwinAppBundle = with pkgs;
          import ./nix/codd-darwin-bundle.nix {
            codd-exe = flakeDefault.packages."codd:exe:codd";
            inherit macdylibbundler stdenv zip;
          };
      });
}
