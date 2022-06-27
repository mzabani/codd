{ pkgs }:
import ./test-shell-pg.nix {
  inherit pkgs;
  postgres = pkgs.postgresql_11;
  pgDataDir = "./local/pg11datadir";
}
