let
  pkgs = import ../nixpkgsMusl.nix;
  codd-exe = (import ../../default.nix { inherit pkgs; }).codd.components.exes.codd;

in pkgs.dockerTools.buildImage {
  name = "codd";
  tag = "latest";

  contents = [ codd-exe ];
}
