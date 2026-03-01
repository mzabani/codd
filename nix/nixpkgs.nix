{ system ? builtins.currentSystem }:
let haskellPatchesOverlay = final: prev: {
      haskellPackages = prev.haskellPackages.override {
        overrides = hsSelf: hsSuper: {
          haxl = final.haskell.lib.doJailbreak
            (final.haskell.lib.markUnbroken hsSuper.haxl);
          postgresql-query = final.haskell.lib.dontCheck (final.haskell.lib.markUnbroken hsSuper.postgresql-query);
        };
      };
    };
    haskellNix = import (fetchTarball {
      url = "https://github.com/input-output-hk/haskell.nix/archive/e35f00f2aaee653ce8b504c512a3eb4030517c14.tar.gz";
      sha256 = "sha256:ceWHb8+EZJKi+o4dr6/5vp2X+QIdo35I7ZjPc9nfFGU=";
    }) {};
    nixpkgsImportArgs = haskellNix.nixpkgsArgs // { inherit system; overlays = haskellNix.nixpkgsArgs.overlays ++ [haskellPatchesOverlay ]; };
in
  import haskellNix.sources.nixpkgs-2405 nixpkgsImportArgs
