{ mkDerivation, aeson, base, base-compat, bytestring, containers
, deepseq, entropy, exceptions, hspec, hspec-discover
, http-api-data, http-client, http-media, http-types, HUnit
, kan-extensions, markdown-unlit, monad-control, mtl, network
, QuickCheck, semigroupoids, servant, servant-client-core
, servant-server, stdenv, stm, tdigest, text, time, transformers
, transformers-base, transformers-compat, wai, warp
}:
mkDerivation {
  pname = "servant-client";
  version = "0.18";
  sha256 = "31a483e773ce2d05755f2cf704394b0d0acbfaaa89457ef0cded2ae2a662a09e";
  libraryHaskellDepends = [
    base base-compat bytestring containers deepseq exceptions
    http-client http-media http-types kan-extensions monad-control mtl
    semigroupoids servant servant-client-core stm text time
    transformers transformers-base transformers-compat
  ];
  testHaskellDepends = [
    aeson base base-compat bytestring entropy hspec http-api-data
    http-client http-types HUnit kan-extensions markdown-unlit mtl
    network QuickCheck servant servant-client-core servant-server stm
    tdigest text transformers transformers-compat wai warp
  ];
  testToolDepends = [ hspec-discover markdown-unlit ];
  homepage = "http://docs.servant.dev/";
  description = "Automatic derivation of querying functions for servant";
  license = stdenv.lib.licenses.bsd3;
}
