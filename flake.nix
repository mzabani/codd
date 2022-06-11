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
                  # in package.yaml
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
                  hpack
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
          # This adds support for `nix build .#js-unknown-ghcjs-cabal:hello:exe:hello`
          # crossPlatforms = p: [p.ghcjs];
        };
      in flake // {
        # Built by `nix build .`
        defaultPackage = flake.packages."codd:exe:codd";
      });
}
