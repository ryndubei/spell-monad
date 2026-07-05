{
  inputs = {
    nixpkgs.follows = "haskell-language-server/nixpkgs";
    ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
    ghc-wasm-meta.inputs.nixpkgs.follows = "nixpkgs";
    haskell-language-server.url = "github:haskell/haskell-language-server";
  };

  outputs =
    {
      self,
      nixpkgs,
      ghc-wasm-meta,
      haskell-language-server,
    }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      devShells.${system} = {
        hls = haskell-language-server.devShells.${system}.shell-ghc914;
        wasm = pkgs.mkShell {
          buildInputs = [
            ghc-wasm-meta.packages.${system}.all_9_14
          ];
          shellHook = ''
            export BSDTAR_WASM_PATH=${self.packages.${system}.pkgsCross.wasi32.bsdtar-wasm}
          '';
        };
        default = pkgs.mkShell {
          inputsFrom = with self.devShells.${system}; [
            hls
            wasm
          ];
          shellHook = ''
            # Uninstall empty pre-commit hook installed by HLS shell
            pre-commit uninstall
          '';
        };
      };
      packages.${system}.pkgsCross.wasi32 = rec {
        zstd-wasm = pkgs.callPackage ./zstd-wasm.nix {
          ghc-wasm-meta = ghc-wasm-meta.packages.${system}.all_9_14;
        };
        bsdtar-wasm = pkgs.callPackage ./bsdtar-wasm.nix {
          inherit zstd-wasm;
          ghc-wasm-meta = ghc-wasm-meta.packages.${system}.all_9_14;
        };
      };
    };
  nixConfig.extra-substituters = [ "https://haskell-language-server.cachix.org" ];
}
