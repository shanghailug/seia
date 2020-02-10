{ mkDerivation, base, constraints-extras, containers, dependent-sum
, stdenv
}:
mkDerivation {
  pname = "dependent-map";
  version = "0.3";
  sha256 = "00b9c710154d874cc5ca83a54d2485bd94bbec4c9756f1897a4d8166b337feab";
  libraryHaskellDepends = [
    base constraints-extras containers dependent-sum
  ];
  homepage = "https://github.com/mokus0/dependent-map";
  description = "Dependent finite maps (partial dependent products)";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
