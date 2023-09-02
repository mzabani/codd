# This is a "vanilla" nixpkgs cabal2nix derivation.
# The upside of having this is that it has a much smaller closure
# than a haskell.nix provided one, so it's useful as an installation option.
# The downside is that library versions are likely not the same as the ones from Stackage LTS.
let
  postgres15Overlay = import ./postgres15Overlay.nix;
  haskellPatchesOverlay = final: prev: {
    haskellPackages = prev.haskellPackages.override {
      overrides = hsSelf: hsSuper: {
        haxl = final.haskell.lib.doJailbreak
          (final.haskell.lib.markUnbroken hsSuper.haxl);
      };
    };
  };
  nixpkgs = import
    (let lock = builtins.fromJSON (builtins.readFile ../flake.lock);
    in fetchTarball {
      url =
        "https://github.com/NixOS/nixpkgs/archive/${lock.nodes.nixpkgs-unstable.locked.rev}.tar.gz";
      sha256 = lock.nodes.nixpkgs-unstable.locked.narHash;
    }) { overlays = [ postgres15Overlay haskellPatchesOverlay ]; };
  pgService = import ./postgres-service.nix {
    pkgs = nixpkgs;
    postgres = nixpkgs.postgresql;
    initializePostgres = true;
    wipeCluster = true;
  };
  coddFull = nixpkgs.haskell.lib.dontHaddock
    (nixpkgs.haskellPackages.callCabal2nixWithOptions "codd" ../. "" { });
in rec {
  inherit nixpkgs;
  # A derivation with tests so we're more comfortable that this release of codd
  # isn't completely broken just because it uses different library versions.
  coddWithCheck = coddFull.overrideAttrs (self: {
    checkPhase = ''
      export PGDATA="./cabal2nix-codd-datadir"
      export PGDATABASE="postgres"
      export PGPORT="5434"
      export PGHOST="127.0.0.1"
      export PGUSER="postgres"
      export PATH="$PATH:${nixpkgs.postgresql}/bin" # Some tests require pg_dump in PATH
      ${pgService}/bin/init-postgres
    '' + self.checkPhase;
  });
  codd = nixpkgs.haskell.lib.dontCheck coddFull;
}
