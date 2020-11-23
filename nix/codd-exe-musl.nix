let
  pkgs = import ./nixpkgsMusl.nix;
  stdenv = pkgs.stdenv;
  zlib = pkgs.zlib;
  gmp6 = pkgs.gmp6;
  projectPkgs = import ../default.nix { inherit pkgs; };
in
  projectPkgs.codd.components.exes.codd.overrideAttrs (oldAttrs: {
    configureFlags = [
        "--disable-executable-dynamic"
        "--disable-shared"
        "--ghc-option=-optl=-pthread"
        "--ghc-option=-optl=-static"
        "--ghc-option=-optl=-L${gmp6.override { withStatic = true; }}/lib"
        "--ghc-option=-optl=-L${zlib.static}/lib"
      ];
  })