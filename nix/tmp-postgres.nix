{ mkDerivation, ansi-wl-pprint, async, base, base64-bytestring
, bytestring, containers, criterion, cryptohash-sha1, deepseq
, directory, generic-monoid, hspec, mtl, network, port-utils
, postgres-options, postgresql-simple, process, stdenv, stm
, temporary, transformers, unix
}:
mkDerivation {
  pname = "tmp-postgres";
  version = "1.34.1.0";
  sha256 = "98514428edaf527cc464cb9a30df89c6168c858e039ab1baf8293471196c3ba2";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-wl-pprint async base base64-bytestring bytestring containers
    cryptohash-sha1 deepseq directory generic-monoid port-utils
    postgres-options postgresql-simple process stm temporary
    transformers unix
  ];
  executableHaskellDepends = [
    async base directory postgres-options postgresql-simple process
    temporary
  ];
  testHaskellDepends = [
    async base containers directory generic-monoid hspec mtl network
    port-utils postgres-options postgresql-simple process temporary
    unix
  ];
  benchmarkHaskellDepends = [
    base criterion deepseq postgres-options postgresql-simple temporary
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/jfischoff/tmp-postgres#readme";
  description = "Start and stop a temporary postgres";
  license = stdenv.lib.licenses.bsd3;
}
