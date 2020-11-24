let
    # Fetch the latest haskell.nix and import its default.nix
    haskellNix = import (builtins.fetchTarball {
        url = "https://github.com/input-output-hk/haskell.nix/archive/15b45578a9f0c2a31cae3d0e8772d5e8cc37212b.tar.gz";
        sha256 = "0jichn6qjmph31c7bv09wc76vrzjknkxih0w1qp4agip5164mcdz";
    }) {};

    # haskell.nix provides access to the nixpkgs pins which are used by our CI,
    # hence you will be more likely to get cache hits when using these.
    # But you can also just use your own, e.g. '<nixpkgs>'.
    nixpkgsSrc = haskellNix.sources.nixpkgs-2009;

    # haskell.nix provides some arguments to be passed to nixpkgs, including some
    # patches and also the haskell.nix functionality itself as an overlay.
    nixpkgsArgs = haskellNix.nixpkgsArgs;
in
    # import nixpkgs with overlays
    import nixpkgsSrc nixpkgsArgs
