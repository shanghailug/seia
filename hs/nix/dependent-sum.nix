{ mkDerivation, base, constraints-extras, stdenv }:
mkDerivation {
  pname = "dependent-sum";
  version = "0.6.2.0";
  sha256 = "4da09dfbeb62eb73cc43102de872803dc3fc4a9d8216f149300e2e9c5d2db29f";
  libraryHaskellDepends = [ base constraints-extras ];
  homepage = "https://github.com/mokus0/dependent-sum";
  description = "Dependent sum type";
  license = stdenv.lib.licenses.publicDomain;
}
