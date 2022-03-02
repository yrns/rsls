{
  inputs.nixpkgs.url = "nixpkgs/nixpkgs-unstable";
  inputs.rustify.url = "github:yrns/rustify";
  outputs = { self, nixpkgs, rustify }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      crateOverrides = rustify.lib.crateOverrides {
        lockFile = ./Cargo.lock;
        inherit pkgs;
      };
    in
    {
      devShell.${system} = pkgs.mkShell rec
      {
        buildInputs = crateOverrides.buildInputs;
        LD_LIBRARY_PATH = "${pkgs.lib.makeLibraryPath buildInputs}";
        shellHook = ''
          PATH="./target/debug:$PATH"
        '';
      };
    };
}
