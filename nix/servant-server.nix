{ mkDerivation, aeson, base, base-compat, base64-bytestring
, bytestring, containers, directory, exceptions, filepath, hspec
, hspec-discover, hspec-wai, http-api-data, http-media, http-types
, monad-control, mtl, network, network-uri, QuickCheck, resourcet
, safe, servant, should-not-typecheck, stdenv, string-conversions
, tagged, temporary, text, transformers, transformers-base
, transformers-compat, wai, wai-app-static, wai-extra, warp, word8
}:
mkDerivation {
  pname = "servant-server";
  version = "0.18";
  sha256 = "8867b98926593848ef281511de4f4074abfe52f2efb939ace4e2264d4dfecb4e";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base-compat base64-bytestring bytestring containers exceptions
    filepath http-api-data http-media http-types monad-control mtl
    network network-uri resourcet servant string-conversions tagged
    text transformers transformers-base wai wai-app-static word8
  ];
  executableHaskellDepends = [
    aeson base base-compat servant text wai warp
  ];
  testHaskellDepends = [
    aeson base base-compat base64-bytestring bytestring directory hspec
    hspec-wai http-types mtl QuickCheck resourcet safe servant
    should-not-typecheck string-conversions temporary text transformers
    transformers-compat wai wai-extra
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://docs.servant.dev/";
  description = "A family of combinators for defining webservices APIs and serving them";
  license = stdenv.lib.licenses.bsd3;
}
