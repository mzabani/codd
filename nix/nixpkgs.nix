let
    # Fetch the latest haskell.nix and import its default.nix
    haskellNix = import (builtins.fetchTarball {
        url = "https://github.com/input-output-hk/haskell.nix/archive/c810f3af58d6e93d1459315ae26b8d8e4b5c1f3d.tar.gz";
        sha256 = "1dbdh8kmxargb9z09lrarbks1fngxc0lhrb13h5rg8xrqaxq9h5q";
    }) {};

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
in
    import nixpkgsSrc customArgs
