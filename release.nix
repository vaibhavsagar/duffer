let
  pkgs = import <nixpkgs> { };
  gitRepo = pkgs.runCommand "gitRepo" { src = ./.; } ''
    mkdir -p $out
    cp -r $src/.git/* $out/
  '';
  base64-src = builtins.fetchTarball {
    url = "https://github.com/emilypi/base64/tarball/fab53ab62e115bf21986e91ee539ea8764db9a7a";
    sha256 = "1d7pv6fp3m3ny7iayzss6iqyp0jd1sd5w11pviy8sn112vbmjj52";
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
