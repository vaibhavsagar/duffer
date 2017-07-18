let
  pkgs       = import <nixpkgs> { };
  pipes-zlib = pkgs.haskell.lib.doJailbreak pkgs.haskellPackages.pipes-zlib;
  cabal2nix  = pkgs.haskellPackages.callCabal2nix;

in rec {
  duffer           = pkgs.haskellPackages.callPackage ./duffer           {};
  duffer-streaming = cabal2nix "duffer-streaming"     ./duffer-streaming { inherit duffer; pipes-zlib = pipes-zlib; };
  duffer-json      = cabal2nix "duffer-json"          ./duffer-json      { inherit duffer; };
  ihaskell-duffer  = cabal2nix "ihaskell-duffer"      ./ihaskell-duffer  { inherit duffer; };
}
