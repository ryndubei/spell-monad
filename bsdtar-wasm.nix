{
  pkgs,
  stdenvNoCC,
  cmake,
  ghc-wasm-meta,
}:

let
  zstd-wasm = pkgs.pkgsCross.wasi32.zstd-wasm;
in
stdenvNoCC.mkDerivation {
  name = "bsdtar-wasm";
  src = pkgs.fetchFromGitHub {
    owner = "haskell-wasm";
    repo = "libarchive";
    rev = "28bf5cbcd271a3dcf3dea228585b228921dce1de";
    hash = "sha256-esr2Cq0hfLPPjdVhHm0krjUZmLsN1G6Xv6xDV2kWuyc=";
  };
  cmakeFlags = [
    "-Bbuild-wasi"
    "-DCMAKE_TOOLCHAIN_FILE=${ghc-wasm-meta}/share/cmake/wasi-sdk.cmake"
    "-DCMAKE_EXE_LINKER_FLAGS='-Wl,--error-limit=0,--keep-section=target_features,--stack-first,--strip-debug,--lto-O3'"
    "-DCMAKE_BUILD_TYPE=Release"
    "-DCMAKE_INTERPROCEDURAL_OPTIMIZATION=ON"
    "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
    "-DCMAKE_MACOSX_RPATH=OFF"
    "-DBUILD_SHARED_LIBS=OFF"
    "-DENABLE_OPENSSL=OFF"
    "-DENABLE_LIBB2=OFF"
    "-DENABLE_LZ4=OFF"
    "-DENABLE_LZMA=OFF"
    "-DENABLE_ZSTD=ON"
    "-DZSTD_INCLUDE_DIR=${zstd-wasm}/include"
    "-DZSTD_LIBRARY=${zstd-wasm}/lib/libzstd.a"
    "-DENABLE_ZLIB=OFF"
    "-DENABLE_BZip2=OFF"
    "-DENABLE_LIBXML2=OFF"
    "-DENABLE_EXPAT=OFF"
    "-DENABLE_WIN32_XMLLITE=OFF"
    "-DENABLE_PCREPOSIX=OFF"
    "-DENABLE_PCRE2POSIX=OFF"
    "-DENABLE_CNG=OFF"
    "-DENABLE_TAR=ON"
    "-DENABLE_CPIO=OFF"
    "-DENABLE_CAT=OFF"
    "-DENABLE_UNZIP=OFF"
    "-DENABLE_XATTR=OFF"
    "-DENABLE_ACL=OFF"
    "-DENABLE_ICONV=OFF"
    "-DENABLE_TEST=OFF"
  ];
  buildPhase = ''
    cmake --build build-wasi --target install
  '';
  installPhase = ''
    ${ghc-wasm-meta}/bin/wasm-opt --debuginfo --low-memory-unused --strip-dwarf -O4 --converge "$out/bin/bsdtar" -o $out/bin/bsdtar.wasm
    rm $out/bin/bsdtar
  '';
  nativeBuildInputs = [
    cmake
  ];
}
