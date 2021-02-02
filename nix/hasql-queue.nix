{ mkDerivation, aeson, async, base, base64-bytestring, bytestring
, cryptohash-sha1, exceptions, hasql, here, hspec, hspec-core
, hspec-expectations-lifted, monad-control, postgresql-libpq
, postgresql-libpq-notify, random, resource-pool, split, stdenv
, stm, text, time, tmp-postgres, transformers
}:
mkDerivation {
  pname = "hasql-queue";
  version = "1.2.0.2";
  sha256 = "b5d843d2e759153e4cedd6860c34af774ef965b4bdcb2cb6080f362f92a31e35";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring exceptions hasql here monad-control
    postgresql-libpq postgresql-libpq-notify random stm text time
    transformers
  ];
  executableHaskellDepends = [
    aeson async base base64-bytestring bytestring cryptohash-sha1
    exceptions hasql here monad-control postgresql-libpq
    postgresql-libpq-notify random resource-pool stm text time
    tmp-postgres transformers
  ];
  testHaskellDepends = [
    aeson async base base64-bytestring bytestring cryptohash-sha1
    exceptions hasql here hspec hspec-core hspec-expectations-lifted
    monad-control postgresql-libpq postgresql-libpq-notify random
    resource-pool split stm text time tmp-postgres transformers
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/jfischoff/hasql-queue#readme";
  description = "A PostgreSQL backed queue";
  license = stdenv.lib.licenses.bsd3;
}
