{ compiler ? "ghcjs86"
, nixpkgs ? <nixpkgs>
}:
let
  config = {
    allowBroken = true;
  };

  pkgs = (import nixpkgs) { inherit config; };

  hpkgs = pkgs.haskell.packages.${compiler}.override {
    overrides = (import ./nix/overrides.nix) pkgs;
  };

  hpkgs' = hpkgs;
in {
  seia = hpkgs'.callPackage ./default.nix { };
}

