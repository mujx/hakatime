{ mkDerivation, aeson, attoparsec, base, base-compat, bifunctors
, bytestring, case-insensitive, deepseq, hspec, hspec-discover
, http-api-data, http-media, http-types, mmorph, mtl, network-uri
, QuickCheck, quickcheck-instances, singleton-bool, stdenv
, string-conversions, tagged, text, transformers, vault
}:
mkDerivation {
  pname = "servant";
  version = "0.18";
  sha256 = "721747edb750d2856538f73d155d83023fa1617057d37b10a8d1098eb0488bb5";
  libraryHaskellDepends = [
    aeson attoparsec base base-compat bifunctors bytestring
    case-insensitive deepseq http-api-data http-media http-types mmorph
    mtl network-uri QuickCheck singleton-bool string-conversions tagged
    text transformers vault
  ];
  testHaskellDepends = [
    aeson base base-compat bytestring hspec http-media mtl QuickCheck
    quickcheck-instances string-conversions text transformers
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://docs.servant.dev/";
  description = "A family of combinators for defining webservices APIs";
  license = stdenv.lib.licenses.bsd3;
}
