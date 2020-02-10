pkgs: self: super:
{
  rank1dynamic = pkgs.haskell.lib.dontCheck super.rank1dynamic;
  QuickCheck = pkgs.haskell.lib.dontCheck super.QuickCheck;

  # test actually fail, but in rare cast, use anyway
  unicode-show = pkgs.haskell.lib.dontCheck super.unicode-show;

  # avoid test out of mem
  scientific = pkgs.haskell.lib.dontCheck super.scientific;

  # more test fail, fix for 19.09
  time-compat = pkgs.haskell.lib.dontCheck super.time-compat;

  # avoid doctest, fail to build on ghcjs
  distributive = pkgs.haskell.lib.dontCheck super.distributive;
  comonad = pkgs.haskell.lib.dontCheck super.comonad;
  semigroupoids = pkgs.haskell.lib.dontCheck super.semigroupoids;
  lens = pkgs.haskell.lib.dontCheck super.lens;
  http-types = pkgs.haskell.lib.dontCheck super.http-types;

  # test do not finish
  tasty-quickcheck = pkgs.haskell.lib.dontCheck super.tasty-quickcheck;

  # jsaddle, miss ghcjs-base deps
  jsaddle = pkgs.haskell.lib.addBuildDepend super.jsaddle super.ghcjs-base;

  # gen doc cause OOM
  ghcjs-dom-jsffi = pkgs.haskell.lib.dontHaddock super.ghcjs-dom-jsffi;

  eccrypto = pkgs.haskell.lib.dontCheck super.eccrypto;

  extra = pkgs.haskell.lib.dontCheck super.extra;
  temporary = pkgs.haskell.lib.dontCheck super.temporary;
  unliftio = pkgs.haskell.lib.dontCheck super.unliftio;
  conduit = pkgs.haskell.lib.dontCheck super.conduit;

  reflex = pkgs.haskell.lib.dontCheck (self.callPackage ./reflex_0_6_2_4.nix {});

  Glob = pkgs.haskell.lib.dontCheck super.Glob;
  SHA = pkgs.haskell.lib.dontCheck super.SHA;
  directory-tree = pkgs.haskell.lib.dontCheck super.directory-tree;

  # avoid network, etc
  reflex-dom-core = pkgs.haskell.lib.dontCheck (self.callPackage ./reflex-dom-core_0_5_2.nix {});

  # deps of reflex-dom-core
  chrome-test-utils = null;

  semialign = self.callPackage ./semialign.nix {};
  these = self.callPackage ./these.nix {};
  these-lens = self.callPackage ./these-lens.nix {};

  constraints-extras = self.callPackage ./constraints-extras.nix {};
  dependent-sum = self.callPackage ./dependent-sum.nix {};
  dependent-map = self.callPackage ./dependent-map.nix {};
  dependent-sum-template = self.callPackage ./dependent-sum-template.nix {};

  monoidal-containers = self.callPackage ./monoidal-containers.nix {};

  # for 19.09
  witherable = self.callPackage ./witherable_0_3.nix {};  
  bimap = self.callPackage ./bimap_0_3_3.nix {};
}

