# This is a "vanilla" nixpkgs cabal2nix derivation.
# The upside of having this is that it has a much smaller closure
# than a haskell.nix provided one. The downside is that library versions
# are likely not the same as the ones from Stackage LTS.
let
  nixpkgs = import
    (let lock = builtins.fromJSON (builtins.readFile ../flake.lock);
    in fetchTarball {
      url =
        "https://github.com/NixOS/nixpkgs/archive/${lock.nodes.nixpkgs.locked.rev}.tar.gz";
      sha256 = lock.nodes.nixpkgs.locked.narHash;
    }) {
      overlays = [
        (final: prev:
          prev // {
            haskellPackages = prev.haskellPackages.override {
              overrides = self: super: {
                haxl = final.haskell.lib.markUnbroken prev.haskellPackages.haxl;
                unliftio =
                  prev.haskellPackages.callPackage ./haskell-deps/unliftio.nix
                  { };
              };
            };
          })
      ];
    };
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
  coddWithCheck = coddFull.overrideAttrs (self: {
    preConfigure = self.preConfigure + ''
      export PGDATA="./cabal2nix-codd-datadir"
      export PGDATABASE="postgres"
      export PGPORT="5434"
      export PGHOST="127.0.0.1"
      export PGUSER="postgres"
      ${pgService}/bin/init-postgres
    '';
  });
  codd = nixpkgs.haskell.lib.dontCheck coddFull;
}
