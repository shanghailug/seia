{ mkDerivation, aeson, base, bytestring, containers, eccrypto
, exceptions, ghcjs-base, ghcjs-dom, ghcjs-prim, jsaddle, lens
, reflex, reflex-dom-core, stdenv, text, time, utf8-string
, unicode-show
}:
mkDerivation {
  pname = "seia";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers eccrypto exceptions ghcjs-base
    ghcjs-dom ghcjs-prim jsaddle lens reflex reflex-dom-core text time
    utf8-string unicode-show
  ];
  license = stdenv.lib.licenses.gpl3;
}
