let
  pkgs       = import <nixpkgs> { };
  pipes-zlib = pkgs.haskell.lib.doJailbreak pkgs.haskellPackages.pipes-zlib;
  cabal2nix  = pkgs.haskellPackages.callCabal2nix;
  dontCheck  = pkgs.haskell.lib.dontCheck;

in rec {
  duffer           = dontCheck (cabal2nix "duffer"           ./duffer           {});
  duffer-streaming = dontCheck (cabal2nix "duffer-streaming" ./duffer-streaming { inherit duffer pipes-zlib; });
  duffer-json      = dontCheck (cabal2nix "duffer-json"      ./duffer-json      { inherit duffer; });
  ihaskell-duffer  = dontCheck (cabal2nix "ihaskell-duffer"  ./ihaskell-duffer  { inherit duffer; });
}
