let
  pkgs = import <unstable> {};

  ghc = pkgs.haskellPackages.ghcWithPackages (hpkgs: [
    hpkgs.time
    hpkgs.aeson
    hpkgs.either
  ]);
in pkgs.stdenv.mkDerivation {
  name = "url-blocker";
  src = ./.;
  buildInputs = [];
  buildPhase = ''
    ${ghc}/bin/ghc Main.hs
  '';
  installPhase = ''
    mv ./Main $out
  '';
}
