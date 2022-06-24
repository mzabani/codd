{ pkgs }:
import ./test-shell-pg.nix {
  inherit pkgs;
  postgres = pkgs.postgresql_12;
  pgDataDir = "./local/pg12datadir";
}
