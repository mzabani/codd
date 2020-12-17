let
  pkgs = import ../nixpkgsMusl.nix;
  codd-exe = import ../codd-exe-musl.nix;

  useradd = "${pkgs.shadow}/bin/useradd";
  # mkdir = "${pkgs.coreutils}/bin/mkdir";

in pkgs.dockerTools.buildImage {
  name = "codd";
  tag = "latest";

  contents = [ codd-exe pkgs.coreutils ];
  runAsRoot = ''
    #!${pkgs.runtimeShell}
    export PATH="/bin/"
    ${pkgs.dockerTools.shadowSetup}
    ${useradd} -m -U codd
    mkdir /tmp
    chmod a+rwx /tmp
  '';

  config = {
    Cmd = "/bin/codd";
    User = "codd:codd";
  };
}
