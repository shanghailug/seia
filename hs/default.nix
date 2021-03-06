{ mkDerivation, aeson, base, base32-z-bytestring, binary
, bytestring, co-log, co-log-core, containers, eccrypto, exceptions
, ghcjs-base, ghcjs-dom, ghcjs-prim, jsaddle, lens, mtl, random
, reflex, reflex-basic-host, reflex-dom-core, stdenv, stm, text
, time, transformers, unicode-show, utf8-string
}:
mkDerivation {
  pname = "seia";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base base32-z-bytestring binary bytestring co-log co-log-core
    containers eccrypto exceptions ghcjs-base ghcjs-dom ghcjs-prim
    jsaddle lens mtl random reflex reflex-basic-host reflex-dom-core
    stm text time transformers unicode-show utf8-string
  ];
  license = stdenv.lib.licenses.gpl3;
}
