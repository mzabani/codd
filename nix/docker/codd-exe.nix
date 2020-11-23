let
  pkgs = import ../nixpkgsMusl.nix;
  codd-exe = import ../codd-exe-musl.nix;

in pkgs.dockerTools.buildImage {
  name = "codd";
  tag = "latest";

  contents = [ codd-exe ];
}
