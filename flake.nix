{
  description = "Codd's flake";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  # We only have flake-compat here while we support nix-shell and
  # nix-build, i.e. while we support non-flakes Nix usage.
  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix, flake-compat }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        postgres = pkgs.postgresql_14;
        postgres-service = import ./nix/postgres-service.nix {
          postgres = postgres;
          inherit pkgs;
          initializePostgres = false;
          wipeCluster = false;
        };
        overlays = [
          haskellNix.overlay
          (final: prev:
            let finalIohkPkgs = final.haskell-nix.haskellPackages;
            in {
              # This overlay adds our project to pkgs
              coddProject = final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc8107";

                modules = [{
                  # Musl builds fail because postgresql-libpq requires pg_config in the path for its configure phase.
                  # See https://github.com/haskellari/postgresql-libpq/blob/master/Setup.hs#L65-L66
                  packages.postgresql-libpq.components.library.build-tools =
                    [ pkgs.postgresql ];

                  # Work around https://github.com/input-output-hk/haskell.nix/issues/231. More info
                  # in codd.cabal
                  packages.codd.components.tests.codd-test.build-tools = [
                    finalIohkPkgs.hspec-discover.components.exes.hspec-discover
                  ];
                }];

                # This is used by `nix develop .` to open a shell for use with
                # `cabal`, `hlint` and `haskell-language-server`
                shell.tools = {
                  cabal = "latest";
                  hlint = "latest";
                  haskell-language-server = "latest";
                };
                # Non-Haskell shell tools go here
                shell.buildInputs = with pkgs; [
                  ghcid
                  haskellPackages.brittany # Brittany from the LTS is older than this
                  # finalIohkPkgs.brittany.components.exes.brittany
                  postgres
                  glibcLocales
                  cacert
                  postgres-service
                ];
                shell.shellHook = ''
                  source scripts/source-env.sh .env

                  # init-postgres doesn't actually work with direnv. I tried to daemonize starting postgres but was not able
                  # to make it work. See https://github.com/direnv/direnv/issues/755
                  init-postgres

                  echo You should be able to start postgres with 'pg_ctl start' and use 'psql' to connect to it, and it will be independent from any your own system might have provided.
                  echo You just might have to run ./scripts/create-dev-db.sh and then 'codd.sh up' first to create database $PGDATABASE.
                  echo If 'psql' fails to connect, check logs at $PGDATA/log/

                  export PATH="$PATH:scripts/path"
                '';
                # This adds `js-unknown-ghcjs-cabal` to the shell.
                # shell.crossPlatforms = p: [p.ghcjs];
              };
            })
        ];

        flake = pkgs.coddProject.flake {
          # This adds support for `nix build .#x86_64-unknown-linux-musl:codd:exe:codd`
          # and `nix build .#x86_64-w64-mingw32:codd:exe:codd`
          # Check nixpkgs.lib.systems for more.
          # Sadly, musl64 builds fail when building postgresql-libpq. https://github.com/input-output-hk/haskell.nix/issues/782 might be related.
          # The mingw build fails with infinite recursion right at the start too..
          crossPlatforms = p: [ p.musl64 p.mingwW64 ];
        };
      in flake // {
        # Built by `nix build .`
        defaultPackage = flake.packages."codd:exe:codd";

        # Built with `nix build .#dockerImage.x86_64-linux`.
        # Ideally this would be statically linked, and preferably
        # with musl as libc for smaller size, but that's hard to do.  
        dockerImage = import ./nix/docker/codd-exe.nix {
          inherit pkgs;
          codd-exe = flake.packages."codd:exe:codd";
        };
      });
}
