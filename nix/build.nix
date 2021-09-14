{ pkgs ? import ./nixpkgs.pinned.nix 
}: let
  hPkgs = pkgs.haskell.packages.ghc8107;
in { inherit hPkgs; } // hPkgs.callCabal2nix "optparse-applicative" ../. {}

