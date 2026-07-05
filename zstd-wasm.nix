{
  pkgs,
  stdenvNoCC,
  cmake,
  ghc-wasm-meta,
}:

stdenvNoCC.mkDerivation {
  name = "zstd-wasm";
  src = pkgs.fetchFromGitHub {
    owner = "haskell-wasm";
    repo = "zstd";
    rev = "198fa1bd25becd5f29c1b487a7195584e06593d6";
    hash = "sha256-W062klZgPE6Exzt3yp9PsEZVXy1QExWIm1GI1+ZhfiY=";
  };
  cmakeDir = "cmake";
  cmakeFlags = [
    "-Bbuild-wasi"
    "-DCMAKE_TOOLCHAIN_FILE=${ghc-wasm-meta}/share/cmake/wasi-sdk.cmake"
    "-DCMAKE_EXE_LINKER_FLAGS='-Wl,--error-limit=0,--keep-section=target_features,--stack-first,--strip-debug,--lto-O3'"
    "-DCMAKE_BUILD_TYPE=Release"
    "-DCMAKE_INTERPROCEDURAL_OPTIMIZATION=ON"
    "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
    "-DZSTD_BUILD_PROGRAMS=ON"
    "-DZSTD_BUILD_SHARED=OFF"
    "-DZSTD_LEGACY_SUPPORT=OFF"
    "-DZSTD_MULTITHREAD_SUPPORT=OFF"
  ];
  buildPhase = ''
    cmake --build build-wasi --target install
  '';
  installPhase = ''
    ${ghc-wasm-meta}/bin/wasm-opt --debuginfo --low-memory-unused --strip-dwarf -O4 --converge "$out/bin/zstd" -o $out/bin/zstd.wasm
    rm $out/bin/zstd
  '';
  nativeBuildInputs = [
    cmake
  ];
}
