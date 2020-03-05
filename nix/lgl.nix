{ mkDerivation, base, containers, gdp, HUnit, lens, mtl, sort
, stdenv, transformers
}:
mkDerivation {
  pname = "lgl";
  version = "0.1.0.0";
  src = /home/nihliphobe/projects/haskell/lgl;
  libraryHaskellDepends = [
    base containers gdp lens mtl transformers
  ];
  testHaskellDepends = [
    base containers gdp HUnit lens mtl sort transformers
  ];
  homepage = "https://github.com/Maxfield-Chen/liberty-go-server";
  license = stdenv.lib.licenses.bsd3;
}
