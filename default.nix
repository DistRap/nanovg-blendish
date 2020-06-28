{ nixpkgs ? import <nixpkgs> {} }:
nixpkgs.haskellPackages.callCabal2nix "nanovg-blendish" ./. { }
