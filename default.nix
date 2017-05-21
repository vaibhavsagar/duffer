haskellPackages: rec {
  duffer           = haskellPackages.callPackage ./duffer {};
  duffer-json      = haskellPackages.callPackage ./duffer-json      { inherit duffer; };
  duffer-streaming = haskellPackages.callPackage ./duffer-streaming { inherit duffer; };
}
