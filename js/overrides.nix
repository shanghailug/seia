{ pkgs ? import <nixpkgs> {},
  system ? builtins.currentSystem
}:

let
  wrtc_0_4_3_binary = pkgs.fetchurl {
    url = https://node-webrtc.s3.amazonaws.com/wrtc/v0.4.3/Release/linux-x64.tar.gz;
    sha256 = "1j4vy67v2y40928x1cfqxdfwycbc4jyvmvv8g0z76kclv11p0856";
  };

  nodePackages = import ./default.nix { inherit pkgs system; };
  ovFunc = oldAttrs: {
    buildInputs = oldAttrs.buildInputs ++ [pkgs.nodePackages.node-pre-gyp];

    preRebuild = ''
    pushd ./node_modules/wrtc
    mkdir -p build
    tar -C build -xf ${wrtc_0_4_3_binary}
    popd
    '';
  };
in
nodePackages // {
  package = nodePackages.package.override ovFunc;
  shell = nodePackages.shell.override ovFunc;
}
