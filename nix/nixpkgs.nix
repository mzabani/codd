let haskellPatchesOverlay = final: prev: {
    haskellPackages = prev.haskellPackages.override {
      overrides = hsSelf: hsSuper: {
        haxl = final.haskell.lib.doJailbreak
          (final.haskell.lib.markUnbroken hsSuper.haxl);
      };
    };
  };
in
  import
    (let lock = builtins.fromJSON (builtins.readFile ../flake.lock);
    in fetchTarball {
      url =
        "https://github.com/NixOS/nixpkgs/archive/${lock.nodes.nixpkgs-2311.locked.rev}.tar.gz";
      sha256 = lock.nodes.nixpkgs-2311.locked.narHash;
    }) { overlays = [ haskellPatchesOverlay ]; }
