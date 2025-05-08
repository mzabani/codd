let haskellPatchesOverlay = final: prev: {
    haskellPackages = prev.haskellPackages.override {
      overrides = hsSelf: hsSuper: {
        haxl = final.haskell.lib.doJailbreak
          (final.haskell.lib.markUnbroken hsSuper.haxl);
      };
    };
  };
  haskellNix = import (fetchTarball {
    url = "https://github.com/input-output-hk/haskell.nix/archive/6aa8046087d4e6fd70f3b6b99628f77e398e9fd2.tar.gz";
    sha256 = "sha256:1wrzkmqkhansgic6x55jjkssq5viis41lwnn3vkyn19818xjylw0";
  }) {};
  nixpkgsImportArgs = haskellNix.nixpkgsArgs // {overlays = haskellNix.nixpkgsArgs.overlays ++ [haskellPatchesOverlay]; };
in
  import haskellNix.sources.nixpkgs-2311 nixpkgsImportArgs
