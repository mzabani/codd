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

  noKvmOverlay = import ./docker/no-kvm-overlay.nix;
  customArgs = nixpkgsArgs // {
    overlays = nixpkgsArgs.overlays ++ [ noKvmOverlay ];
  };
in import nixpkgsSrc customArgs
