{ mkDerivation, async, base, bytestring, containers, deepseq, directory
, fetchgit, filepath, gauge, hpack, hspec, lib, process, QuickCheck
, safe-exceptions, stm, time, transformers, unix, unliftio-core }:
mkDerivation {
  pname = "unliftio";
  version = "0.2.21.0";
  src = fetchgit {
    url = "https://github.com/fpco/unliftio.git";
    sha256 = "sha256-mANtDqhwDaHO1fx5SIg58cXXCVvy6nmBLm4aBlfqNHU=";
    rev = "unliftio-0.2.21.0";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/unliftio; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    async
    base
    bytestring
    deepseq
    directory
    filepath
    process
    safe-exceptions
    stm
    time
    transformers
    unix
    unliftio-core
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    async
    base
    bytestring
    containers
    deepseq
    directory
    filepath
    hspec
    process
    QuickCheck
    safe-exceptions
    stm
    time
    transformers
    unix
    unliftio-core
  ];
  benchmarkHaskellDepends = [
    async
    base
    bytestring
    deepseq
    directory
    filepath
    gauge
    process
    safe-exceptions
    stm
    time
    transformers
    unix
    unliftio-core
  ];
  prePatch = "hpack";
  homepage = "https://github.com/fpco/unliftio/tree/master/unliftio#readme";
  description =
    "The MonadUnliftIO typeclass for unlifting monads to IO (batteries included)";
  license = lib.licenses.mit;
}
