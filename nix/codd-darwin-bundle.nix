{ coddexe, pkgs }:
pkgs.stdenv.mkDerivation {
  name = "codd.app";
  buildInputs = with pkgs; [ macdylibbundler zip ];
  src = coddexe;
  installPhase = ''
    mkdir -p "$out"
    mkdir -p codd.app/Contents/MacOS/
    cp "$src/bin/codd" "codd.app/Contents/MacOS/codd"
    dylibbundler -od -b -x ./codd.app/Contents/MacOS/codd -d ./codd.app/Contents/libs/ -p @executable_path/../libs/
    ls -lh -R codd.app
    codd.app/Contents/MacOS/codd --help
    zip -r "$out/codd.zip" codd.app
  '';
}
