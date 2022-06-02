{ pkgs ? import ./nixpkgs.nix }:
import ./test-shell-pg.nix {
  inherit pkgs;
  postgres = pkgs.postgresql_14;
  pgDataDir = "./local/pg14datadir";
}
