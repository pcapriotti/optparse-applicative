{ pkgs ? import ./nix/nixpkgs.pinned.nix 
}: let
  drv = import ./nix/build.nix { inherit pkgs; };
in import ./nix/mkHaskellShell.nix { inherit pkgs; } drv

