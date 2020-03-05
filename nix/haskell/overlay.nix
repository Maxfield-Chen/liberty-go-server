nixpkgsSelf: nixpkgsSuper:

let
  inherit (nixpkgsSelf) pkgs;

  ghcVersion = "ghc865";

  hsPkgs = nixpkgsSuper.haskell.packages.${ghcVersion}.override {
    overrides = self: super: {
   ghcide = pkgs.haskell.lib.dontCheck (self.callCabal2nix
        "ghcide"
        (builtins.fetchGit {
          url = "https://github.com/digital-asset/ghcide.git";
          rev = "0838dcbbd139e87b0f84165261982c82ca94fd08";
        })
        {});
      hie-bios = pkgs.haskell.lib.dontCheck (self.callHackageDirect {
        pkg = "hie-bios";
        ver = "0.3.2";
        sha256 = "08b3z2k5il72ccj2h0c10flsmz4akjs6ak9j167i8cah34ymygk6";
      } {});
      haskell-lsp = pkgs.haskell.lib.dontCheck (self.callHackageDirect {
        pkg = "haskell-lsp";
        ver = "0.18.0.0";
        sha256 = "0pd7kxfp2limalksqb49ykg41vlb1a8ihg1bsqsnj1ygcxjikziz";
      } {});
      haskell-lsp-types = pkgs.haskell.lib.dontCheck (self.callHackageDirect {
        pkg = "haskell-lsp-types";
        ver = "0.18.0.0";
        sha256 = "1s3q3d280qyr2yn15zb25kv6f5xcizj3vl0ycb4xhl00kxrgvd5f";
      } {});
      shake = pkgs.haskell.lib.dontCheck (self.callHackage "shake" "0.18.3" {});
      gdp =
        self.callPackage ./gdp.nix { };
      lgl =
        self.callPackage ./lgl.nix { };
    };
  };

in
{
  haskell = nixpkgsSuper.haskell // {
    inherit ghcVersion;

    packages = nixpkgsSuper.haskell.packages // {
      "${ghcVersion}" = hsPkgs;
    };
  };
}
