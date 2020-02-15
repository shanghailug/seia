{ mkDerivation, aeson, attoparsec, base, bytestring, clock
, criterion, deepseq, doctest, hashable, HUnit, old-locale
, primitive, QuickCheck, semigroups, stdenv, test-framework
, test-framework-hunit, test-framework-quickcheck2, text, thyme
, time, torsor, vector
}:
mkDerivation {
  pname = "chronos";
  version = "1.0.7";
  sha256 = "7d462a8db222e3fab9ea8ae0864bdb2fbeb5e3294897d66f1e5303d1d520137b";
  revision = "1";
  editedCabalFile = "0rf2rcdx7cvx1aa6yb9khbgkrh08v4l52sg1w89qz0245dalw14r";
  libraryHaskellDepends = [
    aeson attoparsec base bytestring clock hashable primitive
    semigroups text torsor vector
  ];
  testHaskellDepends = [
    attoparsec base bytestring doctest HUnit QuickCheck test-framework
    test-framework-hunit test-framework-quickcheck2 text torsor
  ];
  benchmarkHaskellDepends = [
    attoparsec base bytestring criterion deepseq old-locale QuickCheck
    text thyme time vector
  ];
  homepage = "https://github.com/andrewthad/chronos";
  description = "A performant time library";
  license = stdenv.lib.licenses.bsd3;

  postPatch = ''
    sed -i -e 's/, CTimespec(..)//' src/Chronos/Internal/CTimespec.hs
  '';
}
