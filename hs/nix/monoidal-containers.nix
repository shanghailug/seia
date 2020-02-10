{ mkDerivation, aeson, base, containers, deepseq, hashable, lens
, newtype, semialign, semigroups, stdenv, these
, unordered-containers
}:
mkDerivation {
  pname = "monoidal-containers";
  version = "0.6";
  sha256 = "892dbf922c415365b8aaf59a54de3791855dfd78e34510a490cb3c64804e20c6";
  revision = "1";
  editedCabalFile = "1k4k8g5a7swaylcqnga7lyp0lly8j1fqzdwsnznmps8bwn1pn1kk";
  libraryHaskellDepends = [
    aeson base containers deepseq hashable lens newtype semialign
    semigroups these unordered-containers
  ];
  homepage = "http://github.com/bgamari/monoidal-containers";
  description = "Containers with monoidal accumulation";
  license = stdenv.lib.licenses.bsd3;
}
