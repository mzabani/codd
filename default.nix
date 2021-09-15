{ pkgs ? import ./nix/nixpkgs.nix }:
  with pkgs.haskell-nix;
  stackProject {
      # 'cleanGit' cleans a source directory based on the files known by git
      src = pkgs.haskell-nix.haskellLib.cleanGit {
        name = "codd";
        src = ./.;
      };

      modules = [ 
        {
          # Musl builds fail because postgresql-libpq requires pg_config in the path for its configure phase.
          # See https://github.com/haskellari/postgresql-libpq/blob/master/Setup.hs#L65-L66
          packages.postgresql-libpq.components.library.build-tools = [ pkgs.postgresql ];
        }
      ];
  }