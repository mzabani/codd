{ system ? builtins.currentSystem, crossSystem ? null }:
let
    haskellPatchesOverlay = final: prev:
      {
        haskell = prev.haskell // {
          packages = builtins.mapAttrs (ghcVer: hpkgs:
            let canOverride = (builtins.tryEval (hpkgs ? override)).value or false;
            in if canOverride then
              hpkgs.override {
                overrides = hsSelf: hsSuper: {
                  haxl = final.haskell.lib.dontCheck
                    (final.haskell.lib.doJailbreak
                      (final.haskell.lib.markUnbroken
                        (final.haskell.lib.overrideSrc hsSuper.haxl {
                          src = final.fetchgit {
                            url = "https://github.com/facebook/Haxl.git";
                            rev = "6cd30c084debcde9bb83e0f517bbe98cdd80e945";
                            sha256 = "sha256-/P44enZEdgbhULD/3eFNnSinSFlwqXfmvLHDISHBceM=";
                          };
                          version = "2.5.2.0";
                        })));
                  postgresql-query = final.haskell.lib.dontCheck
                    (final.haskell.lib.markUnbroken hsSuper.postgresql-query);
                };
              }
            else hpkgs
          ) prev.haskell.packages;
        };
      };

    # The full postgresql server derivation (generic.nix) forces clang with
    # -flto, producing LLVM bitcode .a files that GNU ld can't link. Instead,
    # use the standalone libpq package which builds with GCC and produces
    # proper ELF static archives. We map `postgresql` to `libpq` only in the
    # musl cross set; the native pkgs (used for test shells etc.) are unaffected.
    muslPostgresFixOverlay = final: prev:
      let
        # libpq.nix inherits `teams` from postgresql.meta, but in the musl
        # cross set postgresql.meta lacks `teams`. Provide it so the
        # inherit doesn't fail.
        pgWithFixedMeta = prev.postgresql // {
          meta = prev.postgresql.meta // { teams = prev.postgresql.meta.teams or {}; };
        };
      in
      prev.lib.optionalAttrs prev.stdenv.hostPlatform.isMusl {
        # Also fix `libpq` itself so that packages depending on it directly
        # (e.g. Haskell's postgresql-libpq-pkgconfig) don't hit the teams error.
        libpq = prev.libpq.override {
          gssSupport = false;
          postgresql = pgWithFixedMeta;
        };
        postgresql = final.libpq.overrideAttrs (old: {
          # Keep .a files for static linking (normally removed for non-static builds)
          postInstall = "";
          # In static builds all library files end up in the dev output,
          # leaving the out output empty.  The fixup phase removes empty
          # directories, which deletes $out and makes nix reject the
          # derivation.  Ensure $out exists after fixup.
          postFixup = (old.postFixup or "") + "\nmkdir -p $out";
          # Break the meta inheritance cycle: libpq.meta inherits from
          # postgresql.meta, but we are replacing postgresql with libpq.
          meta = {
            description = "C application programmer's interface to PostgreSQL";
            homepage = "https://www.postgresql.org";
            license = prev.lib.licenses.postgresql;
            platforms = prev.lib.platforms.all;
          };
        });
      };

    ourOwnHaskellPkgsOverlay = final: prev:
      let
        isMusl = prev.stdenv.hostPlatform.isMusl;
        sourceOverrides = prev.haskell.lib.compose.packageSourceOverrides {
          codd = ../.;
          codd-tests = ../codd-tests;
          codd-benchmarks = ../codd-benchmarks;
        };
        muslConfigureFlags = [
          "--ghc-option=-optl=-L${final.openssl.out}/lib"
          "--ghc-option=-optl=-lssl"
          "--ghc-option=-optl=-lcrypto"

          "--ghc-option=-optl=-L${final.postgresql.dev}/lib"
          "--ghc-option=-optl=-lpgcommon"
          "--ghc-option=-optl=-lpgport"
        ];
        dontCheckAndMuslOverrides = hsSelf: hsSuper:
          let noProfiling = final.haskell.lib.disableLibraryProfiling;
          in {
          codd = noProfiling (if isMusl
            then final.haskell.lib.appendConfigureFlags (final.haskell.lib.dontCheck hsSuper.codd) muslConfigureFlags
            else final.haskell.lib.dontCheck hsSuper.codd);
          codd-tests = noProfiling (final.haskell.lib.addBuildTool
            (if isMusl
              then final.haskell.lib.appendConfigureFlags (final.haskell.lib.dontCheck hsSuper.codd-tests) muslConfigureFlags
              else final.haskell.lib.dontCheck hsSuper.codd-tests)
            hsSelf.hspec-discover);
          codd-benchmarks = noProfiling (final.haskell.lib.dontCheck hsSuper.codd-benchmarks);
        };
      in {
        haskell = prev.haskell // {
          packages = builtins.mapAttrs (ghcVer: hpkgs:
            let canExtend = (builtins.tryEval (hpkgs ? extend)).value or false;
            in if canExtend then
              hpkgs.extend (prev.lib.composeExtensions
                sourceOverrides
                dontCheckAndMuslOverrides)
            else hpkgs
          ) prev.haskell.packages;
        };
      };
in
    import (fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/22fa6f7b5510a5492e46232efcb0a07f68d8be03.tar.gz";
      sha256 = "sha256:1md1mh2h6xz9cd80lfjnwrjyi575py02s8dm2naks6wd6n3ay3rr";
    }) ({
      inherit system;
      overlays = [ haskellPatchesOverlay muslPostgresFixOverlay ourOwnHaskellPkgsOverlay ];
    } // (if crossSystem != null then { inherit crossSystem; } else {}))
