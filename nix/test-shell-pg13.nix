{ pkgs }:
import ./test-shell-pg.nix {
  inherit pkgs;
  postgres = pkgs.postgresql_13;
  pgDataDir = "./local/pg13datadir";
}
