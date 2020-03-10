{ pkgs ? import ../nixpkgs {} }:

let
  inherit (pkgs.haskell) ghcVersion;

  # determine why the inherited haskell version from overlay isn't picked up
  hsPkgs = pkgs.haskell.packages.${ghcVersion};

  pkgDrv = hsPkgs.callCabal2nix "lgs" ../.. {};
  haskellDeps = pkgDrv.getBuildInputs.haskellBuildInputs;
  ghc = hsPkgs.ghcWithHoogle (_: haskellDeps);

in
{
  inherit ghc;
  inherit (hsPkgs) cabal-install ghcide hlint hindent ghcid;
}
