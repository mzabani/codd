{ pkgs }:
import ./test-shell-pg.nix {
  inherit pkgs;
  postgres = pkgs.postgresql_15;
  pgDataDir = "./local/pg15datadir";
}
