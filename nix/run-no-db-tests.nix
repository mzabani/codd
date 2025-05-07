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
      mkdir "$out"
      export LANG=en_US.UTF-8
      # This isn't deterministic due to randomised testing and timing
      # information in the output, so we're really
      # abusing Nix's sandbox here, but it does makes life a lot easier.
      ${coddtests}/bin/codd-test ${hspecArgs} 2>&1 | tee "$out/haskell-tests.log"
    '';
    }
