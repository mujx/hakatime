{ mkDerivation, aeson, base, base64-bytestring, bytestring
, case-insensitive, containers, contravariant-extras, cookie
, cryptonite, fakedata, file-embed, hasql, hasql-pool, hasql-queue
, hasql-transaction, http-client, http-client-tls, http-media
, http-types, katip, lib, mr-env, optparse-applicative
, postgresql-binary, postgresql-simple, postgresql-simple-migration
, random, raw-strings-qq, relude, req, safe-exceptions, servant
, servant-client, servant-server, system-filepath, text, time, unix
, uuid, uuid-types, vector, wai, wai-cors, wai-extra, wai-logger
, warp
}:
mkDerivation {
  pname = "hakatime";
  version = "1.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base64-bytestring bytestring case-insensitive containers
    contravariant-extras cookie cryptonite file-embed hasql hasql-pool
    hasql-queue hasql-transaction http-client http-client-tls
    http-media http-types katip mr-env optparse-applicative
    postgresql-binary postgresql-simple postgresql-simple-migration
    raw-strings-qq relude req safe-exceptions servant servant-server
    system-filepath text time unix uuid uuid-types vector wai wai-extra
  ];
  executableHaskellDepends = [
    aeson base base64-bytestring fakedata hasql hasql-pool hasql-queue
    http-client http-client-tls katip mr-env optparse-applicative
    random relude safe-exceptions servant servant-client servant-server
    time unix wai wai-cors wai-extra wai-logger warp
  ];
  testHaskellDepends = [ base ];
  doHaddock = false;
  doCheck = false;
  description = "Wakatime API server implementation / Dashboard UI";
  license = lib.licenses.unlicense;
}
