let
  pkgs = import ./nixpkgsMusl.nix;
  stdenv = pkgs.stdenv;
  zlib = pkgs.zlib;
  gmp6 = pkgs.gmp6;
  projectPkgs = import ../default.nix { inherit pkgs; };
in
  # The static build thing, which can be read about at https://input-output-hk.github.io/haskell.nix/tutorials/cross-compilation/#static-executables-with-musl-libc
  # is not working
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