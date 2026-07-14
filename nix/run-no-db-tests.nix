{ pkgs, codd-tests, hspecArgs }:
let fs = pkgs.lib.fileset;
in
 pkgs.stdenv.mkDerivation {
     name = "codd-test-without-db-results";
     src = fs.toSource {
      root = ../.;
      fileset = fs.unions [ ../codd-tests/migrations ];
     };
     nativeBuildInputs = [ pkgs.glibcLocales ];
     installPhase = ''
      export LANG=en_US.UTF-8
      mkdir $out
      ${codd-tests}/bin/codd-tests ${hspecArgs}
    '';
    }
