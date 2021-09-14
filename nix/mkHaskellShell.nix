{ pkgs ? import ./nixpkgs.pinned.nix
}: drv: with drv.hPkgs; let
  name = drv.name;
  ghc = ghcWithPackages (pkgs: (drv.propagatedBuildInputs ++ drv.buildInputs));
  watch = pkgs.writeScriptBin "watch" ''
    ${pkgs.ghcid}/bin/ghcid -c "${cabal-install}/bin/cabal v2-repl $@"
  '';
in pkgs.mkShell rec {
  inherit name;
  buildInputs = [
    cabal-install
    watch
    haskell-language-server
  ];
  HISTFILE = toString ../.history;
  LOCAL_HISTFILE = HISTFILE;
}

