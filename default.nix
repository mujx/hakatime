{ mkDerivation, aeson, base, base64-bytestring, bits, blaze-builder
, bytestring, case-insensitive, containers, contravariant-extras
, cookie, cryptonite, fakedata, file-embed, hasql, hasql-pool
, hasql-queue, hasql-transaction, http-client, http-client-tls
, http-media, http-types, katip, mr-env, optparse-applicative
, polysemy, polysemy-plugin, postgresql-binary, postgresql-simple
, postgresql-simple-migration, random, raw-strings-qq, relude, req
, safe, safe-exceptions, servant, servant-client, servant-server
, stdenv, system-filepath, text, time, unix, unliftio-core, uuid
, uuid-types, wai, wai-cors, wai-extra, wai-logger, warp
}:
mkDerivation {
  pname = "hakatime";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base64-bytestring bits blaze-builder bytestring
    case-insensitive containers contravariant-extras cookie cryptonite
    file-embed hasql hasql-pool hasql-queue hasql-transaction
    http-client http-client-tls http-media http-types katip mr-env
    optparse-applicative polysemy polysemy-plugin postgresql-binary
    postgresql-simple postgresql-simple-migration raw-strings-qq relude
    req safe safe-exceptions servant servant-server system-filepath
    text time unix unliftio-core uuid uuid-types wai wai-extra
  ];
  executableHaskellDepends = [
    aeson base base64-bytestring fakedata hasql hasql-pool hasql-queue
    http-client http-client-tls katip mr-env optparse-applicative
    polysemy polysemy-plugin random relude safe-exceptions servant
    servant-client servant-server time unix wai wai-cors wai-extra
    wai-logger warp
  ];
  testHaskellDepends = [ base ];
  doHaddock = false;
  doCheck = false;
  description = "Wakatime API server implementation / Dashboard UI";
  license = stdenv.lib.licenses.unlicense;
}
