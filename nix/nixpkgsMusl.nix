let
    nixpkgsSrc = builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/20.09.tar.gz";
        sha256 = "1wg61h4gndm3vcprdcg7rc4s1v3jkm5xd7lw8r2f67w502y94gcy";
    };

    nixpkgsRaw = import nixpkgsSrc {};

    # Fetch the latest haskell.nix and import its default.nix
    haskellNix = import (builtins.fetchTarball {
        url = "https://github.com/input-output-hk/haskell.nix/archive/01003480bdee1717b95ca99049d283168b405925.tar.gz";
        sha256 = "06j381kix405k3x8yqlccwqckjrc3qrdqchbbd80cd6slf6ybg9c";
    }) { pkgs = nixpkgsRaw.pkgsCross.musl64; };

    # haskell.nix provides some arguments to be passed to nixpkgs, including some
    # patches and also the haskell.nix functionality itself as an overlay.
    nixpkgsArgs = haskellNix.nixpkgsArgs;
in
    # import nixpkgs with overlays
    import nixpkgsSrc nixpkgsArgs