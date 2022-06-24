{ pkgs }:
import ./test-shell-pg.nix {
  inherit pkgs;
  postgres = pkgs.postgresql_10;
  pgDataDir = "./local/pg10datadir";
}
