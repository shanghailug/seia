{ mkDerivation, base, base-compat, lens, stdenv, these }:
mkDerivation {
  pname = "these-lens";
  version = "1";
  sha256 = "feb020128788dfc515164190bb051f7f1034eca90de9e82caf253d8b47f09490";
  revision = "1";
  editedCabalFile = "1lrpq5a8ldddmsi7ckaqinamn2f7kkijq5jq05yzdx818b2563wn";
  libraryHaskellDepends = [ base base-compat lens these ];
  homepage = "https://github.com/isomorphism/these";
  description = "Lenses for These";
  license = stdenv.lib.licenses.bsd3;
}
