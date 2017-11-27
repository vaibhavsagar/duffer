let
  pkgs       = import <nixpkgs> { };
  pipes-zlib = pkgs.haskell.lib.doJailbreak pkgs.haskellPackages.pipes-zlib;
  gitRepo    = pkgs.runCommand "gitRepo" { src = ./.; } ''
    mkdir -p $out
    cp -r $src/.git/* $out/
  '';
  testPatch  = prj: pkgs.haskell.lib.overrideCabal prj (oldDrv: {
    testSystemDepends = (oldDrv.testSystemDepends or []) ++ [ pkgs.git ];
    postPatch = ''
      original="$(${pkgs.gnugrep}/bin/grep "repo =" ./test/Spec.hs)"
      replacement='repo = "${gitRepo}"'
      substituteInPlace ./test/Spec.hs --replace "$original" "$replacement"
    '';
  });
  produce    = path: args: let
    drv = testPatch (pkgs.haskellPackages.callCabal2nix (builtins.baseNameOf path) path args);
  in if pkgs.lib.inNixShell then drv.env else drv;

in rec {
  duffer           = produce ./duffer           {};
  duffer-json      = produce ./duffer-json      { inherit duffer; };
  duffer-streaming = produce ./duffer-streaming { inherit duffer pipes-zlib; };
}
