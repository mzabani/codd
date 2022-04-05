# IMPORTANT
# 1. This file is used by the installer. It must not be moved or
# have its interface changed without changes to "install-codd.sh"
# 2. This file is the only file downloaded from github to install codd.
# It cannot reference any other files in the repository.

let
  # Fetch the latest haskell.nix and import its default.nix
  haskellNix = import (builtins.fetchTarball {
    url =
      "https://github.com/input-output-hk/haskell.nix/archive/b2479523c0bafc9a6ce63cafefd38a110dd01331.tar.gz";
    sha256 = "0i77m8348klzqh2hry7gmykmkk3ngdnabnfjja2mwpynazpgvvzh";
  }) { };

  # haskell.nix provides access to the nixpkgs pins which are used by our CI,
  # hence you will be more likely to get cache hits when using these.
  # But you can also just use your own, e.g. '<nixpkgs>'.
  nixpkgsSrc = haskellNix.sources.nixpkgs-unstable;

  # haskell.nix provides some arguments to be passed to nixpkgs, including some
  # patches and also the haskell.nix functionality itself as an overlay.
  nixpkgsArgs = haskellNix.nixpkgsArgs;
  pkgs = import nixpkgsSrc nixpkgsArgs;
in {
  inherit nixpkgsSrc;
  inherit nixpkgsArgs;
  installShell = with pkgs;
    mkShell { buildInputs = with pkgs; [ jq nix-prefetch-git ]; };
}
