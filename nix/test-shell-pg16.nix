{ pkgs }:
import ./test-shell-pg.nix {
  inherit pkgs;
  postgres = pkgs.postgresql_16;
  pgDataDir = "./local/pg16datadir";
}
