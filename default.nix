{ mkDerivation, base, erf, MonadRandom, random, stdenv }:
mkDerivation {
  pname = "levy";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base erf MonadRandom random ];
  homepage = "https://github.com/jamesthompson/levy.git";
  description = "Lévy distribution functions";
  license = stdenv.lib.licenses.bsd3;
}
