{
  inputs.nixpkgs.url = "nixpkgs/nixpkgs-unstable";
  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShells.default = with pkgs;
          mkShell {
            buildInputs = [
              haskell.compiler.ghc912
              (haskell-language-server.override {
                supportedGhcVersions = [ "912" ];
              })
            ];
          };
      });
}
