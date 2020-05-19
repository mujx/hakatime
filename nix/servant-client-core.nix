{ mkDerivation, aeson, base, base-compat, base64-bytestring
, bytestring, containers, deepseq, exceptions, free, hspec
, hspec-discover, http-media, http-types, network-uri, QuickCheck
, safe, servant, stdenv, template-haskell, text, transformers
}:
mkDerivation {
  pname = "servant-client-core";
  version = "0.17";
  sha256 = "ce65f5d2fd0d07918f8a660527d21929f4a273591e6e9e82842825925bdd53f7";
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
