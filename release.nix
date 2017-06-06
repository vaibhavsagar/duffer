let pkgs = import <nixpkgs> { };

in rec {
  duffer           = pkgs.haskellPackages.callPackage ./duffer           {};
  duffer-streaming = pkgs.haskellPackages.callPackage ./duffer-streaming { inherit duffer; };
  duffer-json      = pkgs.haskellPackages.callPackage ./duffer-json      { inherit duffer; };
  ihaskell-duffer  = pkgs.haskellPackages.callPackage ./ihaskell-duffer  { inherit duffer; };
}
