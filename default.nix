{ mkDerivation, base, Cabal, cabal-doctest, doctest, erf
, MonadRandom, random, stdenv
}:
mkDerivation {
  pname = "levy";
  version = "0.1.0.0";
  src = ./.;
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [ base erf MonadRandom random ];
  testHaskellDepends = [ base doctest ];
  homepage = "https://github.com/jamesthompson/levy.git";
  description = "Lévy distribution functions";
  license = stdenv.lib.licenses.bsd3;
}
