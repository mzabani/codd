let
    # Fetch the latest haskell.nix and import its default.nix
    haskellNix = import (builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/cd429f6ea6946c7a2fc78fc55a1b11cfd73acc86.tar.gz") {};

    # haskell.nix provides access to the nixpkgs pins which are used by our CI,
    # hence you will be more likely to get cache hits when using these.
    # But you can also just use your own, e.g. '<nixpkgs>'.
    nixpkgsSrc = haskellNix.sources.nixpkgs-2003;

    # haskell.nix provides some arguments to be passed to nixpkgs, including some
    # patches and also the haskell.nix functionality itself as an overlay.
    nixpkgsArgs = haskellNix.nixpkgsArgs;
in
    # import nixpkgs with overlays
    import nixpkgsSrc nixpkgsArgs