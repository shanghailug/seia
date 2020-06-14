{ pkgs ? import <nixpkgs> {},
  system ? builtins.currentSystem
}:

let
  wrtc_0_4_5_binary = pkgs.fetchurl {
    url = https://node-webrtc.s3.amazonaws.com/wrtc/v0.4.5/Release/linux-x64.tar.gz;
    sha256 = "0gnc9fyk22isxp9ybdswv009f7bj4ixfpcyf7hc26c4jw7npn57c";
  };

  nodePackages = import ./default.nix { inherit pkgs system; };
  ovFunc = oldAttrs: {
    buildInputs = oldAttrs.buildInputs ++ [pkgs.nodePackages.node-pre-gyp];

    preRebuild = ''
    pushd ./node_modules/wrtc
    mkdir -p build
    tar -C build -xf ${wrtc_0_4_5_binary}
    popd
    '';
  };
in
nodePackages // {
  package = nodePackages.package.override ovFunc;
  shell = nodePackages.shell.override ovFunc;
}
