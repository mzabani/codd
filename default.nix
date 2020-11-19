{ pkgs ? import ./nix/nixpkgs.nix }:
  pkgs.haskell-nix.project {
    # 'cleanGit' cleans a source directory based on the files known by git
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "codd";
      src = ./.;
    };
  }