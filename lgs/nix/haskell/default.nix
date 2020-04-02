{ pkgs ? import ../nixpkgs {} }:

let
  inherit (pkgs.haskell) ghcVersion;
  inherit (pkgs.haskell.packages) ghcjs86;

  hsPkgs = pkgs.haskell.packages.${ghcVersion};

  server = hsPkgs.callCabal2nix "lgs" ../.. {};
  haskellDeps = server.getBuildInputs.haskellBuildInputs;
  ghc = hsPkgs.ghcWithHoogle (_: haskellDeps);

  client = ghcjs86.callCabal2nix "lgs" ../.. {};

in
{
  inherit client;
  inherit ghc;
  inherit (hsPkgs) cabal-install ghcide hlint hindent ghcid;
}
