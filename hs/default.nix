{ mkDerivation, aeson, base, base32-z-bytestring, binary
, bytestring, containers, eccrypto, exceptions, ghcjs-base
, ghcjs-dom, ghcjs-prim, jsaddle, lens, random, reflex
, reflex-dom-core, stdenv, text, time, transformers, unicode-show
, utf8-string
}:
mkDerivation {
  pname = "seia";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base base32-z-bytestring binary bytestring containers
    eccrypto exceptions ghcjs-base ghcjs-dom ghcjs-prim jsaddle lens
    random reflex reflex-dom-core text time transformers unicode-show
    utf8-string
  ];
  license = stdenv.lib.licenses.gpl3;
}
