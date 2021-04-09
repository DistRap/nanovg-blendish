{ pkgs ? import <nixpkgs> {}}:
let
  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
in
  pkgs.haskell.lib.buildFromSdist (
    pkgs.haskellPackages.callCabal2nix "nanovg-blendish" src { }
  )
