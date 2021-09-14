{ pkgs ? import ./nix/nixpkgs.nix }:
  with pkgs.haskell-nix;
  let
    # testModule = r: builtins.trace (builtins.attrNames r) {
    #   config.packages.postgresql-libpq = r.config.packages.postgresql-libpq // {
    #     components = iohkHaskellPkgs.postgresql-libpq.components // {
    #       library = iohkHaskellPkgs.postgresql-libpq.components.library.overrideAttrs (_: {
    #         nativeBuildInputs = iohkHaskellPkgs.postgresql-libpq.components.library.nativeBuildInputs ++ [ pkgs.postgresql ];
    #       });
    #     };
    #   };
    # };
    iohkHaskellPkgs = stackProject {
        # 'cleanGit' cleans a source directory based on the files known by git
        src = pkgs.haskell-nix.haskellLib.cleanGit {
          name = "codd";
          src = ./.;
        };

        modules = [ 
          {
            packages.postgresql-libpq.components.library.build-tools = [ pkgs.pkgsCross.musl64.postgresql ];
          }
         ];
      };

    # postgresql-libpq = iohkHaskellPkgs.postgresql-libpq // {
    #   components = iohkHaskellPkgs.postgresql-libpq.components // {
    #     library = iohkHaskellPkgs.postgresql-libpq.components.library.overrideAttrs (_: {
    #       nativeBuildInputs = iohkHaskellPkgs.postgresql-libpq.components.library.nativeBuildInputs ++ [ pkgs.pkgsCross.musl64.postgresql ];
    #     });
    #   };
    # };

    # muslIohkHaskellPkgs = stackProject {
    #     # 'cleanGit' cleans a source directory based on the files known by git
    #     src = pkgs.haskell-nix.haskellLib.cleanGit {
    #       name = "codd";
    #       src = ./.;
    #     };

    #     pkg-def-extras = [ (_: _: { inherit postgresql-libpq; }) ];
    #   };
  in
  # Musl builds fail because postgresql-libpq requires pg_config in the path for its configure phase
  # (seems it only needs that depending on the existence of the "use-pkg-config" flag).
  # See https://github.com/haskellari/postgresql-libpq/blob/master/Setup.hs#L65-L66
  # TODO: this needs to override every postgresql-libpq as a dependency for every other package.
  # To achieve that, https://input-output-hk.github.io/haskell.nix/reference/library/ might help
  iohkHaskellPkgs
  # iohkHaskellPkgs // {
  #   postgresql-libpq = iohkHaskellPkgs.postgresql-libpq // {
  #     components = iohkHaskellPkgs.postgresql-libpq.components // {
  #       library = iohkHaskellPkgs.postgresql-libpq.components.library.overrideAttrs (_: {
  #         nativeBuildInputs = iohkHaskellPkgs.postgresql-libpq.components.library.nativeBuildInputs ++ [ pkgs.postgresql ];
  #       });
  #     };
  #   };
  # }