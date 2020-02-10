{ mkDerivation, base, bifunctors, comonad, constraints-extras
, containers, criterion, data-default, deepseq, dependent-map
, dependent-sum, directory, exception-transformers, filemanip
, filepath, haskell-src-exts, haskell-src-meta, hlint, lens
, loch-th, MemoTrie, monad-control, monoidal-containers, mtl
, prim-uniq, primitive, process, profunctors, random, ref-tf
, reflection, semialign, semigroupoids, split, stdenv, stm, syb
, template-haskell, these, these-lens, time, transformers
, unbounded-delays, witherable
, ghcjs-base
}:
mkDerivation {
  pname = "reflex";
  version = "0.6.2.4";
  sha256 = "8d99104926752d15da6433b3b541d459db30ef094ec411d6f0579910b3e0a8be";
  libraryHaskellDepends = [
    base bifunctors comonad constraints-extras containers data-default
    dependent-map dependent-sum exception-transformers haskell-src-exts
    haskell-src-meta lens MemoTrie monad-control monoidal-containers
    mtl prim-uniq primitive profunctors random ref-tf reflection
    semialign semigroupoids stm syb template-haskell these time
    transformers unbounded-delays witherable
    ghcjs-base
  ];
  testHaskellDepends = [
    base bifunctors containers deepseq dependent-map dependent-sum
    directory filemanip filepath hlint lens monoidal-containers mtl
    ref-tf semialign split these these-lens transformers
  ];
  benchmarkHaskellDepends = [
    base containers criterion deepseq dependent-map dependent-sum
    loch-th mtl primitive process ref-tf split stm time transformers
  ];
  homepage = "https://reflex-frp.org";
  description = "Higher-order Functional Reactive Programming";
  license = stdenv.lib.licenses.bsd3;

  postPatch = ''
    sed -i -e 's/transformers >= 0.5.6.0 && < 0.6/transformers >= 0.5.5.0 \&\& < 0.6/' *.cabal
    #sed -i -e 's/containers >= 0.4 && < 0.6/containers >= 0.4 \&\& < 0.6.1/' *.cabal
    cat *.cabal
  '';

}
