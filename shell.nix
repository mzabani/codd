let
    pkgs = import ./nix/nixpkgs.nix;
    hsPkgs = import ./default.nix { inherit pkgs; };

    postgres-init = import ./nix/postgres-service.nix { postgres = pkgs.postgresql_12; inherit pkgs; };
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
            [ ghcid hpack hspec-discover ];

        # Prevents cabal from choosing alternate plans, so that
        # *all* dependencies are provided by Nix.
        # Set to false until https://github.com/input-output-hk/haskell.nix/issues/231 is resolved
        exactDeps = false;

        shellHook = ''
            source scripts/source-env.sh .env
            ${postgres-init}/bin/init-postgres
            echo You should be able to use 'psql' now to connect to a postgres database, independent from any your own system might have provided.
            echo You just might have to run 'cabal run codd-exe -- up' first to create the $PGDATABASE database.
            echo If 'psql' fails to connect, check logs at $PGDATA/log/
        '';
    }