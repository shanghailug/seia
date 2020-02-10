{ mkDerivation, aeson, base, constraints, stdenv, template-haskell
}:
mkDerivation {
  pname = "constraints-extras";
  version = "0.3.0.1";
  sha256 = "7d47e6f0871b41a0228953bda965486e118733c78d20eaaadffeb6967de95255";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base constraints template-haskell ];
  executableHaskellDepends = [ aeson base constraints ];
  description = "Utility package for constraints";
  license = stdenv.lib.licenses.bsd3;
}
