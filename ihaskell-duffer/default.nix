{ mkDerivation, base, byteable, bytestring-tree-builder, containers
, duffer, ihaskell, stdenv, utf8-string
}:
mkDerivation {
  pname = "ihaskell-duffer";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base byteable bytestring-tree-builder containers duffer ihaskell
    utf8-string
  ];
  homepage = "http://github.com/vaibhavsagar/duffer#readme";
  description = "Initial project template from stack";
  license = stdenv.lib.licenses.bsd3;
}
