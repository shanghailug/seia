{ mkDerivation, base, base-orphans, containers, hashable, stdenv
, transformers, transformers-compat, unordered-containers, vector
}:
mkDerivation {
  pname = "witherable";
  version = "0.3";
  sha256 = "620f619d37d50e5248a985888153149531155455f037746fdeca9c2c9fdf19c0";
  libraryHaskellDepends = [
    base base-orphans containers hashable transformers
    transformers-compat unordered-containers vector
  ];
  homepage = "https://github.com/fumieval/witherable";
  description = "filterable traversable";
  license = stdenv.lib.licenses.bsd3;
}
