let
  pkgs       = import <nixpkgs> { };
  pipes-zlib = pkgs.haskell.lib.doJailbreak pkgs.haskellPackages.pipes-zlib;
  produce    = path: args: pkgs.haskell.lib.dontCheck
      (pkgs.haskellPackages.callCabal2nix (builtins.baseNameOf path) path args);

in rec {
  duffer           = produce ./duffer           {};
  duffer-streaming = produce ./duffer-streaming { inherit duffer pipes-zlib; };
  duffer-json      = produce ./duffer-json      { inherit duffer; };
  ihaskell-duffer  = produce ./ihaskell-duffer  { inherit duffer; };
}
