args@{ ... }:

let
  sources = import ./sources.nix;

  nixpkgs = import sources.nixpkgs (args // {
    overlays = [
      (import ./haskell/overlay.nix)
    ];
  });

in
nixpkgs
