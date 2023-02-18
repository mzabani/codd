{ pkgs, codd-exe }:
let
  useradd = "${pkgs.shadow}/bin/useradd";
  groupadd = "${pkgs.shadow}/bin/groupadd";

  # Currently, there seems to be no way to map the host's UID and GID to bind-mounts,
  # so there's no way to modify files and folders while giving them the host's user and group
  # permissions..
  # See https://github.com/moby/moby/issues/7198
  #     https://github.com/boxboat/fixuid
  # So we do something terrible: create large ranges of uids and gids
  # which should hopefully cover most use cases

  # Using dockerTools from musl64 throws linking errors
in pkgs.dockerTools.buildImage {
  name = "codd";
  tag = "latest";

  copyToRoot = with pkgs; [
    codd-exe
    # pkgsCross.musl64.dash # We might as well not use dash if we can't get rid of bash inside the final image..
    pkgsCross.musl64.busybox
  ];
  runAsRoot = ''
    #!${pkgs.runtimeShell}
    export PATH="/bin/"
    ${pkgs.pkgsCross.musl64.dockerTools.shadowSetup}
    mkdir /tmp /working-dir
    chmod a+rwx /tmp /working-dir

    for i in {1000..2001}
    do
        addgroup -g "$i" "g$i"
        adduser -u "$i" -G "g$i" -H -D "u$i"
    done

    # shadowSetup adds a dependency on `shadow`, which I guess pulls in glibc?
    # Anyway, we should find some way to do this in the future.
    # The following throws "Permission denied", sadly.
    # chmod o+w /nix/store/* -R
    # rm -rf /nix/store/*-glibc-* /nix/store/*-bash-* /nix/store/*-ncurses-*
  '';

  config = {
    Entrypoint = "/bin/codd";
    WorkingDir = "/working-dir";
  };
}
