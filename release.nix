let
  pkgs = import <nixpkgs> { };

in
  { duffer = pkgs.haskellPackages.callPackage ./default.nix {}; }
