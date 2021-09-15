let
  pkgs = import ../nixpkgs.nix;

  # the musl build doesn't work yet, so we use glibc
  codd-exe = (import ../codd-exe-musl.nix).codd-glibc;

  useradd = "${pkgs.shadow}/bin/useradd";
  groupadd = "${pkgs.shadow}/bin/groupadd";
  mkdir = "${pkgs.coreutils}/bin/mkdir";

# Currently, there seems to be no way to map the host's UID and GID to bind-mounts,
# so there's no way to modify files and folders while giving them the host's user and group
# permissions..
# See https://github.com/moby/moby/issues/7198
#     https://github.com/boxboat/fixuid
# So we do something terrible: create large ranges of uids and gids
# which should hopefully cover most use cases

in pkgs.dockerTools.buildImage {
  name = "codd";
  tag = "latest";

  contents = [ codd-exe pkgs.coreutils ];
  runAsRoot = ''
    #!${pkgs.runtimeShell}
    export PATH="/bin/"
    ${pkgs.dockerTools.shadowSetup}
    ${mkdir} /tmp /working-dir
    chmod a+rwx /tmp /working-dir

    for i in {1..2001}
    do
        ${groupadd} -f --gid "$i" "g$i"
        ${useradd} --uid "$i" --gid "$i" -M "u$i"
    done
  '';

  config = {
    Entrypoint = "/bin/codd";
    WorkingDir = "/working-dir";
  };
}
