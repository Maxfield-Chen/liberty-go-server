{ reflex-platform ? import ./nix/reflex-platform.nix {}
, withHoogle ? false
}:

reflex-platform.project ({ pkgs, ... }: rec {
  inherit withHoogle;
  useWarp = true;
  packages = {
    common = ./common;
    lgs = ./lgs;
    lgc = ./lgc;
  };

  shells = {
    ghc = ["common" "lgs" "lgc"];
    ghcjs = ["common" "lgc"];
  };

  overrides = self: super: {
    gdp =
      self.callPackage ./nix/gdp.nix { };
    lgl =
      self.callPackage ./nix/lgl.nix { };
  };

  shellToolOverrides = self: super: {
    ghcide = pkgs.haskell.lib.dontCheck (self.callHackageDirect {
      pkg = "ghcide";
      ver = "0.1.0";
      sha256 = "sha256:0vwaaqb74dzsvx5xdfkzbi8zzvbd5w9l1wdhl3rhvi8ibnrchgfs";
     } { hie-bios = self.hie-bios;
        haskell-lsp = self.haskell-lsp;
        haskell-lsp-types = self.haskell-lsp-types;
       });

    hie-bios = pkgs.haskell.lib.dontCheck (self.callHackageDirect {
      pkg = "hie-bios";
      ver = "0.4.0";
      sha256 = "sha256:19lpg9ymd9656cy17vna8wr1hvzfal94gpm2d3xpnw1d5qr37z7x";
    } {});

    haskell-lsp = pkgs.haskell.lib.dontCheck (self.callHackageDirect {
      pkg = "haskell-lsp";
      ver = "0.19.0.0";
      sha256 = "sha256:1v0r57g2dhradnjnvp40jmv5swawg9k3d735kj50dca1gbx66y0c";
    } {haskell-lsp-type = self.haskell-lsp-types;});

    haskell-lsp-types = pkgs.haskell.lib.dontCheck (self.callHackageDirect {
      pkg = "haskell-lsp-types";
      ver = "0.19.0.0";
      sha256 = "sha256:1z0c9c2zjb4ad3ffzng9njyn9ci874xd8mmqwnvnm3hyncf1430g";
    } {});
    shake = pkgs.haskell.lib.dontCheck (self.callHackage "shake" "0.18.3" {});
  };
})
