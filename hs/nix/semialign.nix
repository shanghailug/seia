{ mkDerivation, base, base-compat, containers, hashable
, semigroupoids, stdenv, tagged, these, transformers
, unordered-containers, vector
}:
mkDerivation {
  pname = "semialign";
  version = "1";
  sha256 = "b51c461b5233c9450092fc4dd4c21bdfc4dbb5b2e093e9f57e0d630d90029d00";
  revision = "1";
  editedCabalFile = "0qnqnyfng4kwy2h2anrcy5id2ijnawava3zcc5h5b8ri1y6ks6zi";
  libraryHaskellDepends = [
    base base-compat containers hashable semigroupoids tagged these
    transformers unordered-containers vector
  ];
  homepage = "https://github.com/isomorphism/these";
  description = "Align and Zip type-classes from the common Semialign ancestor";
  license = stdenv.lib.licenses.bsd3;
}
