let
  pkgs = import ./nix/nixpkgs.nix;

  proj = pkgs.haskell-nix.stackProject' {
    src = ./.;
    stackYaml = "stack.yaml";
    compiler-nix-name = "ghc965";

    modules =
      [
        {
          # Set to true to be able to run `cabal --enable-profiling`
          enableLibraryProfiling = false;

          # Work around https://github.com/input-output-hk/haskell.nix/issues/231. More info
          # in codd.cabal
          packages.codd.components.tests.codd-test.build-tools = [ proj.hsPkgs.hspec-discover ];
        }
      ]
      ++ (
        if pkgs.stdenv.isDarwin then
          [ ]
        else
          [
            {
              packages.codd.components.exes.codd = {
                dontStrip = false;
                configureFlags = [
                  # I'm not sure how linking works. HMAC_Update and HMAC_Final are two symbols present both in
                  # libssl.a and libcrypto.a, but without including both linking will fail! It is also present
                  # in pgcommon_shlib (from postgres) but it doesn't work if it comes from there either.
                  # Also, the order of -lssl and -lcrypto is important here, and this doesn't seem to affect
                  # dynamically linked glibc builds.
                  # IMPORTANT: `postgresql` is postgresql 15, not 16. pg16 static builds are failing, see
                  # https://github.com/NixOS/nixpkgs/issues/191920
                  # This doesn't seem like a big issue since we only need libpq and we do run tests against
                  # postgresql-16-the-server.
                  "--ghc-option=-optl=-L${pkgs.pkgsCross.musl64.openssl.out}/lib"
                  "--ghc-option=-optl=-lssl"
                  "--ghc-option=-optl=-lcrypto"

                  "--ghc-option=-optl=-L${pkgs.pkgsCross.musl64.postgresql.out}/lib"
                  "--ghc-option=-optl=-lpgcommon"
                  "--ghc-option=-optl=-lpgport"
                ];
              };

              packages.codd.components.tests.codd-test = {
                dontStrip = false;
                configureFlags = [
                  # Same as for the executable here
                  "--ghc-option=-optl=-L${pkgs.pkgsCross.musl64.openssl.out}/lib"
                  "--ghc-option=-optl=-lssl"
                  "--ghc-option=-optl=-lcrypto"

                  "--ghc-option=-optl=-L${pkgs.pkgsCross.musl64.postgresql.out}/lib"
                  "--ghc-option=-optl=-lpgcommon"
                  "--ghc-option=-optl=-lpgport"
                ];
              };
            }
          ]
      );
  };
in
proj
