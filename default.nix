{ mkDerivation, aeson, attoparsec, base, base16-bytestring
, base64-bytestring, byteable, bytestring, containers, cryptonite
, digest, directory, filepath, hspec, hspec-expectations, memory
, mmap, pipes, pipes-attoparsec, pipes-bytestring, pipes-zlib
, process, QuickCheck, stdenv, text, transformers
, unordered-containers, utf8-string, zlib
}:
mkDerivation {
  pname = "duffer";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base base16-bytestring base64-bytestring byteable
    bytestring containers cryptonite digest directory filepath memory
    mmap pipes pipes-attoparsec pipes-bytestring pipes-zlib text
    transformers unordered-containers utf8-string zlib
  ];
  executableHaskellDepends = [
    aeson attoparsec base byteable bytestring containers digest
    directory filepath hspec hspec-expectations process QuickCheck
    transformers utf8-string
  ];
  testHaskellDepends = [
    aeson attoparsec base byteable bytestring containers digest
    directory filepath hspec process QuickCheck transformers
    utf8-string
  ];
  doCheck = false;
  homepage = "http://github.com/vaibhavsagar/duffer#readme";
  description = "Initial project template from stack";
  license = stdenv.lib.licenses.bsd3;
}
