{ reflex-platform ? import ./nix/reflex-platform.nix {}
}:

reflex-platform.project ({ pkgs, ... }: rec {
  withHoogle = true;
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

  overrides = self: super: rec {
    gdp =
      self.callPackage ./nix/gdp.nix { };
    lgl =
      self.callPackage ./nix/lgl.nix { };
    servant-reflex = self.callPackage ./nix/servant-reflex.nix {};
  };

   shellToolOverrides = ghc: super: {
  };

})
