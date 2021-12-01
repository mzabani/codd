let
    pkgs = import ./nix/nixpkgs.nix;
    hsPkgs = import ./default.nix { inherit pkgs; };

    postgres = pkgs.postgresql_12;
    postgres-service = import ./nix/postgres-service.nix { postgres = postgres; inherit pkgs; initializePostgres = false; wipeCluster = false; };

in
    hsPkgs.shellFor {
        # Include only the *local* packages of your project.
        packages = ps: with ps; [
            codd
        ];

        # Builds a Hoogle documentation index of all dependencies,
        # and provides a "hoogle" command to search the index.
        withHoogle = true;

        # You might want some extra tools in the shell (optional).

        # Some common tools can be added with the `tools` argument
        tools = { cabal = "3.2.0.0"; hlint = "2.2.11"; };
        # See overlays/tools.nix for more details

        # Some you may need to get some other way.
        buildInputs = with pkgs.haskellPackages;
            [ ghcid hpack brittany hsPkgs.hspec-discover.components.exes.hspec-discover postgres pkgs.glibcLocales pkgs.cacert postgres-service ];

        # Prevents cabal from choosing alternate plans, so that
        # *all* dependencies are provided by Nix.
        exactDeps = true;

        shellHook = ''
            source scripts/source-env.sh .env

            # init-postgres doesn't actually work with direnv. I tried to daemonize starting postgres but was not able
            # to make it work. See https://github.com/direnv/direnv/issues/755
            init-postgres

            echo You should be able to start postgres with 'pg_ctl start' and use 'psql' to connect to it, and it will be independent from any your own system might have provided.
            echo You just might have to run ./scripts/create-dev-db.sh and then 'codd.sh up' first to create database $PGDATABASE.
            echo If 'psql' fails to connect, check logs at $PGDATA/log/

            export PATH="$PATH:scripts/path"
        '';
    }
