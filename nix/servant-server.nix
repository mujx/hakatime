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
  version = "0.17";
  sha256 = "b44c61c40ce889af7bc324544e1cb007f9fed562a531b1dce8f32f94d162c787";
  revision = "1";
  editedCabalFile = "1kbdga7bi7slgcskqc3sb1xwmwif52dj8gvkxcskaw0b9xbdynhs";
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
  doHaddock = false;
  doCheck = false;
  homepage = "http://docs.servant.dev/";
  description = "A family of combinators for defining webservices APIs and serving them";
  license = stdenv.lib.licenses.bsd3;
}
