{ mkDerivation, base, constraints-extras, dependent-sum, stdenv
, template-haskell, th-extras
}:
mkDerivation {
  pname = "dependent-sum-template";
  version = "0.1.0.0";
  sha256 = "0ef0ad78360cf9b4c304ef8bca96a6544e131df80424178047ab5d79842be91f";
  libraryHaskellDepends = [
    base dependent-sum template-haskell th-extras
  ];
  testHaskellDepends = [ base constraints-extras dependent-sum ];
  homepage = "/dev/null";
  description = "Template Haskell code to generate instances of classes in dependent-sum package";
  license = stdenv.lib.licenses.publicDomain;
}
