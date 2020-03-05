with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/1.4.tar.gz";
  sha256 = "1wl9vpqxshzrlanm9rpvgkiay3xa1abvkyknnp5z41kgfw63ypdl";
}) {});

let
 overrides = self: super: {
        gdp =
          self.callPackage ./nix/gdp.nix { };
        lgl =
          self.callPackage ./nix/lgl.nix { };
 };
 newPkgs = pkgs.haskellPackages.override { inherit overrides; };
 drv = newPkgs.callCabal2nix "lgs" ./. { };

in
  { lgs = drv;
    lgs-shell = newPkgs.shellFor {
      packages = p: [drv];
      buildInputs = with newPkgs; [cabal-install hlint ghcid];
      withHoogle = true;
     };
  }
