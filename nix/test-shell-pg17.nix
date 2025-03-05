{ pkgs }:
import ./test-shell-pg.nix {
  inherit pkgs;
  postgres = pkgs.postgresql_17;
  pgDataDir = "./local/pg17datadir";
}
