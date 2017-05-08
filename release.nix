let pkgs = import <nixpkgs> { };

in rec {
  duffer           = pkgs.haskellPackages.callPackage ./duffer/default.nix           {};
  duffer-streaming = pkgs.haskellPackages.callPackage ./duffer-streaming/default.nix { duffer = duffer; };
  duffer-json      = pkgs.haskellPackages.callPackage ./duffer-json/default.nix      { duffer = duffer; };
}
