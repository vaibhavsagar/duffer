{ mkDerivation, aeson, base, base64-bytestring, bytestring
, containers, duffer, hspec, process, stdenv, text, utf8-string
}:
mkDerivation {
  pname = "duffer-json";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base base64-bytestring bytestring containers duffer text
  ];
  testHaskellDepends = [
    aeson base bytestring duffer hspec process utf8-string
  ];
  homepage = "http://github.com/vaibhavsagar/duffer#readme";
  description = "Initial project template from stack";
  license = stdenv.lib.licenses.bsd3;
}
