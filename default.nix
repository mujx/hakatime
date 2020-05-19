{ mkDerivation, aeson, base, base64-bytestring, bits, bytestring
, case-insensitive, containers, contravariant-extras, cookie
, cryptonite, fakedata, hasql, hasql-pool, hasql-transaction
, http-client, http-client-tls, katip, mr-env, mtl
, optparse-applicative, polysemy, polysemy-plugin
, postgresql-binary, pretty-simple, raw-strings-qq, safe-exceptions
, servant, servant-client, servant-server, stdenv, text, time
, transformers, unliftio-core, uuid, wai, wai-cors, wai-extra
, wai-logger, warp
}:
mkDerivation {
  pname = "hakatime";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base64-bytestring bits bytestring case-insensitive
    containers contravariant-extras cookie cryptonite hasql hasql-pool
    hasql-transaction katip mr-env mtl optparse-applicative polysemy
    polysemy-plugin postgresql-binary raw-strings-qq safe-exceptions
    servant servant-server text time transformers unliftio-core uuid
  ];
  executableHaskellDepends = [
    aeson base base64-bytestring bytestring fakedata hasql-pool
    http-client http-client-tls katip mr-env mtl optparse-applicative
    polysemy polysemy-plugin pretty-simple servant servant-client
    servant-server text time transformers wai wai-cors wai-extra
    wai-logger warp
  ];
  testHaskellDepends = [ base ];
  doHaddock = false;
  doCheck = false;
  description = "Wakatime API server implementation / Dashboard UI";
  license = stdenv.lib.licenses.publicDomain;
}
