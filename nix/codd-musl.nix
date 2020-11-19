let
  pkgs = import ./nixpkgsMusl.nix;
  projectPkgs = import ../default.nix { inherit pkgs; };
in
  projectPkgs.codd.components.exes.codd