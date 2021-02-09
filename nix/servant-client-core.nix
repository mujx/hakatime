{ mkDerivation, aeson, base, base-compat, base64-bytestring
, bytestring, containers, deepseq, exceptions, free, hspec
, hspec-discover, http-media, http-types, network-uri, QuickCheck
, safe, servant, stdenv, template-haskell, text, transformers
}:
mkDerivation {
  pname = "servant-client-core";
  version = "0.18";
  sha256 = "16e6213755e3d009773b9495fc579957983bc5e228e8ad76019f0c1f658f13a5";
  libraryHaskellDepends = [
    aeson base base-compat base64-bytestring bytestring containers
    deepseq exceptions free http-media http-types network-uri safe
    servant template-haskell text transformers
  ];
  testHaskellDepends = [ base base-compat deepseq hspec QuickCheck ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://docs.servant.dev/";
  description = "Core functionality and class for client function generation for servant APIs";
  license = stdenv.lib.licenses.bsd3;
}
