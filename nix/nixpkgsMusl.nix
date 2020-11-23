let
    nixpkgsSrc = builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/20.09.tar.gz";
        sha256 = "1wg61h4gndm3vcprdcg7rc4s1v3jkm5xd7lw8r2f67w502y94gcy";
    };

    # Fetch the latest haskell.nix and import its default.nix
    haskellNix = import (builtins.fetchTarball {
        url = "https://github.com/input-output-hk/haskell.nix/archive/15b45578a9f0c2a31cae3d0e8772d5e8cc37212b.tar.gz";
        sha256 = "0jichn6qjmph31c7bv09wc76vrzjknkxih0w1qp4agip5164mcdz";
    }) {};

    # haskell.nix provides some arguments to be passed to nixpkgs, including some
    # patches and also the haskell.nix functionality itself as an overlay.
    nixpkgsArgs = haskellNix.nixpkgsArgs;
in
    # import nixpkgs with overlays
    import nixpkgsSrc nixpkgsArgs
