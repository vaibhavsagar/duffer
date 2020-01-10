let
  pkgs = import <nixpkgs> { };
  gitRepo = pkgs.runCommand "gitRepo" { src = ./.; } ''
    mkdir -p $out
    cp -r $src/.git/* $out/
  '';
  base64-src = builtins.fetchTarball {
    url = "https://github.com/emilypi/base64/tarball/0fb0ff3043110bcbee6e50788bfba1c0db8549c4";
    sha256 = "1q5qhk3b6hfcwimxnra2xyzixgnkfgiy7gizg6j624n4ghq3y474";
  };
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
    checkPhase = if pkgs.lib.inNixShell then ":" else (oldDrv.checkPhase or null);
  });
  haskellPackages = pkgs.haskellPackages.extend (self: super: let
    produce = path: args: let
      drv = testPatch (self.callCabal2nix (builtins.baseNameOf path) path args);
    in drv;
  in {
    base64 = self.callCabal2nix "base64" base64-src {};
    duffer = produce ./duffer {};
    duffer-json = produce ./duffer-json {};
    duffer-streaming = produce ./duffer-streaming {};
  });

in with haskellPackages; {
  inherit duffer duffer-json duffer-streaming;
}
