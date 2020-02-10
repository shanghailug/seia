{ mkDerivation, aeson, base, bifunctors, bimap, blaze-builder
, bytestring, constraints, constraints-extras
, containers, contravariant, data-default, dependent-map
, dependent-sum, dependent-sum-template, directory
, exception-transformers, exceptions, filepath, ghcjs-dom, hlint
, hspec, hspec-webdriver, http-types, HUnit, jsaddle, jsaddle-warp
, keycode, lens, lifted-base, monad-control, mtl, network
, network-uri, primitive, process, random, ref-tf, reflex
, semigroups, silently, stdenv, stm, template-haskell, temporary
, text, these, transformers, unix, wai, wai-websockets, warp
, webdriver, websockets, zenc
, fetchFromGitHub
, semialign
}:
mkDerivation {
  pname = "reflex-dom-core";
  version = "0.5.2";

  src = fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex-dom";
    rev = "bee4823e79def0a059f97745a7aff493ca129029";
    sha256 = "17qrna9n6c1f24d2dwqpdkf9w6lyb29np04syk5302nqpf0rq5q7";
  } + "/reflex-dom-core";

  libraryHaskellDepends = [
    aeson base bifunctors bimap blaze-builder bytestring constraints
    containers contravariant data-default dependent-map dependent-sum
    dependent-sum-template directory exception-transformers ghcjs-dom
    jsaddle keycode lens monad-control mtl network-uri primitive random
    ref-tf reflex semigroups stm template-haskell text these
    transformers unix zenc semialign
  ];
  testHaskellDepends = [
    aeson base bytestring constraints
    constraints-extras containers dependent-map dependent-sum
    dependent-sum-template directory exceptions filepath ghcjs-dom
    hlint hspec hspec-webdriver http-types HUnit jsaddle jsaddle-warp
    lens lifted-base network process random ref-tf reflex silently
    temporary text wai wai-websockets warp webdriver websockets
  ];
  description = "Functional Reactive Web Apps with Reflex";
  license = stdenv.lib.licenses.bsd3;
}
