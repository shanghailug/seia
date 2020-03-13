{ mkDerivation, base, dependent-sum, lens, mtl, primitive, ref-tf
, reflex, stdenv, stm, witherable
}:
mkDerivation {
  pname = "reflex-basic-host";
  version = "0.2.0.1";
  sha256 = "31b08de05a46c09b9f6c9922aa676b8808077855f9aad3f82164449e591e5dad";
  revision = "1";
  editedCabalFile = "11bzd169wpdn57d7krgx9bw4x5qzskp9d5abdn74x6ipy34cj5ml";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base dependent-sum lens mtl primitive ref-tf reflex stm
  ];
  executableHaskellDepends = [ base lens reflex witherable ];
  homepage = "https://github.com/qfpl/reflex-basic-host/";
  description = "A basic Reflex host for backend work";
  license = stdenv.lib.licenses.bsd3;
}
