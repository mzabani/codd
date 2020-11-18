let
  pkgs = import ../nixpkgsMusl.nix;
  codd = pkgs.haskell-nix.project {
    # 'cleanGit' cleans a source directory based on the files known by git
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "codd";
      src = ../../.;
    };
  };
  codd-exe = codd.components.exes.codd-exe;

in pkgs.dockerTools.buildImage {
  name = "codd";
  tag = "latest";

  contents = [ codd-exe ];

  # config = {
  #   Cmd = "/bin/codd-exe";
  # };
}
