{ mkDerivation, base, lawful, stdenv }:
mkDerivation {
  pname = "gdp";
  version = "0.0.3.0";
  sha256 = "a0f70f3eb52d0c666ef2c6a68130d1e8db21c545fc9a7cd3a839dd538a347d5e";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base lawful ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/matt-noonan/gdp#readme";
  description = "Reason about invariants and preconditions with ghosts of departed proofs";
  license = stdenv.lib.licenses.bsd3;
}
