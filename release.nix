let
  pkgs = import <nixpkgs> { };
  pipes-zlib = pkgs.haskell.lib.doJailbreak pkgs.haskellPackages.pipes-zlib;

in rec {
  duffer           = pkgs.haskellPackages.callPackage ./duffer           {};
  duffer-streaming = pkgs.haskellPackages.callCabal2nix "duffer-streaming" ./duffer-streaming { inherit duffer; pipes-zlib = pipes-zlib; };
  duffer-json      = pkgs.haskellPackages.callCabal2nix "duffer-json"      ./duffer-json      { inherit duffer; };
  ihaskell-duffer  = pkgs.haskellPackages.callCabal2nix "ihaskell-duffer"  ./ihaskell-duffer  { inherit duffer; };
}
