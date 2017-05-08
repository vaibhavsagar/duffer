{ mkDerivation, attoparsec, base, base16-bytestring, byteable
, bytestring, bytestring-tree-builder, containers, cryptonite
, digest, directory, filepath, hspec, memory, mmap, process
, QuickCheck, stdenv, transformers, utf8-string, zlib
}:
mkDerivation {
  pname = "duffer";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base base16-bytestring byteable bytestring
    bytestring-tree-builder containers cryptonite digest directory
    filepath memory mmap transformers utf8-string zlib
  ];
  testHaskellDepends = [
    attoparsec base byteable bytestring containers digest directory
    filepath hspec process QuickCheck transformers utf8-string
  ];
  homepage = "http://github.com/vaibhavsagar/duffer#readme";
  description = "Initial project template from stack";
  license = stdenv.lib.licenses.bsd3;
  doCheck = false;
}
