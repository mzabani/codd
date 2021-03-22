let
    pkgs = import ./nixpkgs.nix;

    # Fetch the latest haskell.nix and import its default.nix
    # The whole pkgsCross thing, which can be read about at https://input-output-hk.github.io/haskell.nix/tutorials/cross-compilation/#static-executables-with-musl-libc
    # is not working
    haskellNix = import (builtins.fetchTarball {
        url = "https://github.com/input-output-hk/haskell.nix/archive/72beef11fc6ec32a98f1dd0d4dcd072c89595b43.tar.gz";
        sha256 = "0ckgssxp43iayp2qn4wf0f06hlfmqwwqvfmak836a7niznj8xz0s";
    }) { pkgs = pkgs.pkgsCross.musl64; };

    # haskell.nix provides access to the nixpkgs pins which are used by our CI,
    # hence you will be more likely to get cache hits when using these.
    # But you can also just use your own, e.g. '<nixpkgs>'.
    nixpkgsSrc = haskellNix.sources.nixpkgs-unstable;

    # haskell.nix provides some arguments to be passed to nixpkgs, including some
    # patches and also the haskell.nix functionality itself as an overlay.
    musl64overlay = self: super: {
        pkgs = super.pkgs.pkgsCross.musl64;
    };
    nixpkgsArgs = haskellNix.nixpkgsArgs;
in
    # import nixpkgs with overlays
    import nixpkgsSrc nixpkgsArgs
