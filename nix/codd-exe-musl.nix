let
  pkgs = import ./nixpkgs.nix;
  muslPkgs = import ../default.nix { pkgs = pkgs.pkgsCross.musl64; };
  glibcPkgs = import ../default.nix { inherit pkgs; };
in
  {
    # Sadly postgresql doesn't build with musl yet
    # See https://github.com/input-output-hk/haskell.nix/issues/782
    codd-musl = muslPkgs.codd.components.exes.codd.overrideAttrs (oldAttrs: {
      configureFlags = [
          "--disable-executable-dynamic"
          "--disable-shared"
          "--ghc-option=-optl=-pthread"
          "--ghc-option=-optl=-static"

          # The two below don't use musl gmp6 and zlib. Is this right?
          "--ghc-option=-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib"
          "--ghc-option=-optl=-L${pkgs.zlib.static}/lib"
        ];
    });

    codd-glibc = glibcPkgs.codd.components.exes.codd;
  }
