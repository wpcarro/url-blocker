let
  pkgs = import <unstable> {};
in pkgs.mkShell {
  buildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
      time
      aeson
      either
      hspec
    ]))
  ];
}
