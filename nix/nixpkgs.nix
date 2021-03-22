let
    # Fetch the latest haskell.nix and import its default.nix
    haskellNix = import (builtins.fetchTarball {
        url = "https://github.com/input-output-hk/haskell.nix/archive/72beef11fc6ec32a98f1dd0d4dcd072c89595b43.tar.gz";
        sha256 = "0ckgssxp43iayp2qn4wf0f06hlfmqwwqvfmak836a7niznj8xz0s";
    }) {};

    # haskell.nix provides access to the nixpkgs pins which are used by our CI,
    # hence you will be more likely to get cache hits when using these.
    # But you can also just use your own, e.g. '<nixpkgs>'.
    nixpkgsSrc = haskellNix.sources.nixpkgs-unstable;

    # haskell.nix provides some arguments to be passed to nixpkgs, including some
    # patches and also the haskell.nix functionality itself as an overlay.
    nixpkgsArgs = haskellNix.nixpkgsArgs;
in
    # import nixpkgs with overlays
    import nixpkgsSrc nixpkgsArgs
