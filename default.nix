# IMPORTANT
# This file is used by the installer. It must not be moved or
# have its interface changed without changes to "install-codd.sh"

{ pkgs ? import ./nix/nixpkgs.nix }:
with pkgs.haskell-nix;
let
  pkgSet = stackProject {
    # 'cleanGit' cleans a source directory based on the files known by git
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "codd";
      src = ./.;
    };

    modules = [{
      # Musl builds fail because postgresql-libpq requires pg_config in the path for its configure phase.
      # See https://github.com/haskellari/postgresql-libpq/blob/master/Setup.hs#L65-L66
      packages.postgresql-libpq.components.library.build-tools =
        [ pkgs.postgresql ];
    }];
  };
  # Work around https://github.com/input-output-hk/haskell.nix/issues/231. More info
  # in package.yaml
in pkgSet // {
  codd = pkgSet.codd // {
    components = pkgSet.codd.components // {
      tests = pkgSet.codd.components.tests // {
        codd-test = pkgSet.codd.components.tests.codd-test.overrideAttrs (_: {
          depsBuildBuildPropagated =
            [ pkgSet.hspec-discover.components.exes.hspec-discover ];
        });
      };
    };
  };
}
