let
    pkgs = import ./nixpkgs.nix;

    # Fetch the latest haskell.nix and import its default.nix
    # The whole pkgsCross thing, which can be read about at https://input-output-hk.github.io/haskell.nix/tutorials/cross-compilation/#static-executables-with-musl-libc
    # is not working
    haskellNix = import (builtins.fetchTarball {
        url = "https://github.com/input-output-hk/haskell.nix/archive/0178d9da9703757acafac565027145dc527ebb3c.tar.gz";
        sha256 = "0l57gnwbfxhskxpd0ndxhif7w3asv9w5zsnfa9b0ng2kvlra1qm0";
    }) { pkgs = pkgs.pkgsCross.musl64; };

    # haskell.nix provides access to the nixpkgs pins which are used by our CI,
    # hence you will be more likely to get cache hits when using these.
    # But you can also just use your own, e.g. '<nixpkgs>'.
    nixpkgsSrc = haskellNix.sources.nixpkgs-2009;

    # haskell.nix provides some arguments to be passed to nixpkgs, including some
    # patches and also the haskell.nix functionality itself as an overlay.
    musl64overlay = self: super: {
        pkgs = super.pkgs.pkgsCross.musl64;
    };
    nixpkgsArgs = haskellNix.nixpkgsArgs;
in
    # import nixpkgs with overlays
    import nixpkgsSrc nixpkgsArgs
