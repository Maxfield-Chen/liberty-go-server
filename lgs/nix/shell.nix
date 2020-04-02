let
  nixpkgs = import ./nixpkgs.nix {};

  inherit (nixpkgs) pkgs;

  haskell = import ./haskell { inherit pkgs; };

in
pkgs.mkShell {
  buildInputs = with pkgs; [
    haskell.cabal-install
    haskell.ghc
    haskell.ghcid
    haskell.ghcide
    haskell.hlint
    haskell.hindent
  ];

  # Use the libraries from the derivation created by ghcWithHoogle.
  NIX_GHC_LIBDIR = "${haskell.ghc}/lib/ghc-${haskell.ghc.version}";
}
