{ bootstrap ? import <nixpkgs> {} }:
let
  reflex-platform = bootstrap.fetchFromGitHub {
  owner = "reflex-frp";
  repo  = "reflex-platform";
  rev = "7e002c573a3d7d3224eb2154ae55fc898e67d211";
  sha256 = "1adhzvw32zahybwd6hn1fmqm0ky2x252mshscgq2g1qlks915436";
  };
in
  import reflex-platform {}
