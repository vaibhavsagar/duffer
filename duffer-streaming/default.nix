{ mkDerivation, attoparsec, base, base16-bytestring, bytestring
, containers, duffer, filepath, hspec, pipes, pipes-attoparsec
, pipes-bytestring, pipes-zlib, stdenv, transformers, zlib, pkgs
}:
let pipes-zlib' = pkgs.haskell.lib.doJailbreak pipes-zlib; in
mkDerivation {
  pname = "duffer-streaming";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base base16-bytestring bytestring duffer filepath pipes
    pipes-attoparsec pipes-bytestring pipes-zlib' transformers zlib
  ];
  testHaskellDepends = [ base containers duffer hspec pipes ];
  homepage = "http://github.com/vaibhavsagar/duffer#readme";
  description = "Initial project template from stack";
  license = stdenv.lib.licenses.bsd3;
}
