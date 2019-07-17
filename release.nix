let
  pkgs       = import <nixpkgs> { };
  pipes-zlib = pkgs.haskell.lib.doJailbreak pkgs.haskellPackages.pipes-zlib;
  gitRepo    = pkgs.runCommand "gitRepo" { src = ./.; } ''
    mkdir -p $out
    cp -r $src/.git/* $out/
  '';
  testPatch  = prj: pkgs.haskell.lib.overrideCabal prj (oldDrv: {
    testSystemDepends = (oldDrv.testSystemDepends or []) ++ [ pkgs.git ];
    postPatch = if pkgs.lib.inNixShell then "" else ''
      original="$(${pkgs.gnugrep}/bin/grep "repo =" ./test/Spec.hs)"
      gitDir=$(${pkgs.coreutils}/bin/mktemp -d)
      cp -r ${gitRepo}/* $gitDir/
      ${pkgs.coreutils}/bin/chmod -R 0755 $gitDir
      replacement="repo = \"$gitDir\""
      substituteInPlace ./test/Spec.hs --replace "$original" "$replacement"
    '';
    checkPhase = if pkgs.lib.inNixShell then ":" else oldDrv.checkPhase;
  });
  haskellPackages = pkgs.haskellPackages.extend (self: super: let
    produce = path: args: let
      drv = testPatch (self.callCabal2nix (builtins.baseNameOf path) path args);
    in drv;
  in {
    duffer = produce ./duffer {};
    duffer-json = produce ./duffer-json {};
    duffer-streaming = produce ./duffer-streaming {};
  });

in with haskellPackages; {
  inherit duffer duffer-json duffer-streaming;
}
