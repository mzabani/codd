{ system ? builtins.currentSystem }:
let haskellPatchesOverlay = final: prev: {
      haskellPackages = prev.haskellPackages.override {
        overrides = hsSelf: hsSuper: {
          haxl = final.haskell.lib.doJailbreak
            (final.haskell.lib.markUnbroken hsSuper.haxl);
          postgresql-query = final.haskell.lib.dontCheck (final.haskell.lib.markUnbroken hsSuper.postgresql-query);
        };
      };
    };
    # The full postgresql server derivation (generic.nix) forces clang with
    # -flto, producing LLVM bitcode .a files that GNU ld can't link. Instead,
    # use the standalone libpq package which builds with GCC and produces
    # proper ELF static archives. We map `postgresql` to `libpq` only in the
    # musl cross set; the native pkgs (used for test shells etc.) are unaffected.
    muslPostgresFixOverlay = final: prev:
      prev.lib.optionalAttrs prev.stdenv.hostPlatform.isMusl {
        postgresql = (prev.libpq.override { gssSupport = false; curlSupport = false; }).overrideAttrs (old: {
          # Keep .a files for static linking (normally removed for non-static builds)
          postInstall = "";
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
    haskellNix = import (fetchTarball {
      url = "https://github.com/input-output-hk/haskell.nix/archive/439046aebd36d8ba65948407aed9bd6c5a494dc6.tar.gz";
      sha256 = "sha256-dElIcx6PM0s5ffopDaRPeWt9jKuf6PfsViqq/Bj/aSU=";
    }) {};
    nixpkgsImportArgs = haskellNix.nixpkgsArgs // { inherit system; overlays = haskellNix.nixpkgsArgs.overlays ++ [haskellPatchesOverlay muslPostgresFixOverlay]; };
in
  import haskellNix.sources.nixpkgs-2511 nixpkgsImportArgs
