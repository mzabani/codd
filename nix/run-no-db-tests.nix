{ pkgs, coddtests, hspecArgs }:
let fs = pkgs.lib.fileset;
in
 pkgs.stdenv.mkDerivation {
     name = "codd-test-without-db-results";
     src = fs.toSource {
      root = ../.;
      fileset = fs.unions [ ../test/migrations ];
     };
     nativeBuildInputs = [ pkgs.glibcLocales ];
     installPhase = ''
      export LANG=en_US.UTF-8
      mkdir $out
      ${coddtests}/bin/codd-test ${hspecArgs}
    '';
    }
