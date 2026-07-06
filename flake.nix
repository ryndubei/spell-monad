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
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ self.overlay.${system} ];
      };
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
      overlay.${system} = k: p: {
        pkgsCross = p.pkgsCross // {
          wasi32 = p.pkgsCross.wasi32 // {
            zstd-wasm =
              assert p.pkgsCross.wasi32 ? zstd-wasm == false;
              k.callPackage ./zstd-wasm.nix {
                ghc-wasm-meta = ghc-wasm-meta.packages.${system}.all_9_14;
              };
            bsdtar-wasm =
              assert p.pkgsCross.wasi32 ? bsdtar-wasm == false;
              k.callPackage ./bsdtar-wasm.nix {
                ghc-wasm-meta = ghc-wasm-meta.packages.${system}.all_9_14;
              };
          };
        };
      };
      packages.${system}.pkgsCross.wasi32 = {
        inherit (pkgs.pkgsCross.wasi32) zstd-wasm bsdtar-wasm;
      };
    };
  nixConfig.extra-substituters = [ "https://haskell-language-server.cachix.org" ];
}
