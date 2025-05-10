{ pkgs }:
pkgs.mkShell {
  buildInputs = [ pkgs.coreutils pkgs.bash pkgs.glibcLocales pkgs.run pkgs.shellcheck ];
  description = "Test shell with what is necessary for 'run ci-tests'";
}

