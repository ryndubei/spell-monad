#!/usr/bin/env bash

set -euo pipefail


scratch=$(mktemp -d)


MAIN_DYNLIB_DIR="$(realpath .)/dist-newstyle/build/wasm32-wasi/$("$CABAL" path --compiler-info | awk '/^compiler-id:/ {print $2}')/$PKG_FULL_NAME/opt/build"


PKG_DBS="$(echo "$GHC_ENV" | awk '/^package-db/ {print $2}')"
PKG_IDS="$(echo "$GHC_ENV" | awk '/^package-id/ {print $2}')"


# Convert package dbs to ghc-pkg arguments
GHC_PKG_ARGS=()
for db in $PKG_DBS; do
    GHC_PKG_ARGS+=(--package-db="$db")
done


# Copy cabal library directories to the same paths in the rootfs
DYN_LIB_DIRS=()
for pkgid in $PKG_IDS; do
    for dir in $("$GHC_PKG" "${GHC_PKG_ARGS[@]}" field --unit-id "$pkgid" dynamic-library-dirs --simple-output || true); do
        mkdir -p "$scratch/$dir"
        cp --no-preserve=mode -r "$dir"/* "$scratch/$dir" 
        DYN_LIB_DIRS+=("$dir")
    done
done


# Delete unnecessary library files
find "$scratch" "(" \
    -name '*.hi' \
    -o -name '*.a' \
    -o -name '*.p_hi' \
    -o -name 'libHS*_p.a' \
    -o -name '*.p_dyn_hi' \
    -o -name 'libHS*_p*.so' \
    -o -name 'libHSrts*_debug*.so' \
    ")" -delete


mkdir -p _rootfs
cp -r "$scratch"/* _rootfs


# Write a custom GHC env file and copy package dbs
mkdir -p "$scratch"/tmp
echo 'clear-package-db' > "$scratch/tmp/ghc_env"
echo 'global-package-db' >> "$scratch/tmp/ghc_env"

mkdir -p "$scratch/tmp/pkgdbs"
for pkgdb in $PKG_DBS; do
    cp --no-preserve=mode -r "$pkgdb" "$scratch/tmp/pkgdbs" 
    echo "package-db /tmp/pkgdbs/$(basename "$pkgdb")" >> "$scratch/tmp/ghc_env"
done

for pkgid in $PKG_IDS; do
    echo "package-id $pkgid" >> "$scratch/tmp/ghc_env"
done


mkdir -p _rootfs/tmp
cp -r "$scratch/tmp/pkgdbs" _rootfs/tmp 
cp "$scratch/tmp/ghc_env" _rootfs/tmp/ghc_env


# Record library directories in constants.mjs
MAIN_SO_PATH="$(find "$MAIN_DYNLIB_DIR" -type f -name '*.so' -print -quit)"
echo "export const HS_SEARCH_DIR      = \"/tmp/hslib/$HS_SEARCHDIR\";" > "$scratch/constants.mjs"
echo "export const MAIN_SO_PATH       = \"$MAIN_SO_PATH\";" >> "$scratch/constants.mjs"
echo "export const MAIN_SO_BASE_NAME  = \"$(basename "$MAIN_SO_PATH")\";" >> "$scratch/constants.mjs"
echo "export const CABAL_DYN_LIB_DIRS = [$(printf '"%s", ' "${DYN_LIB_DIRS[@]}")];" >> "$scratch/constants.mjs"


mkdir -p www/generated
cp "$scratch/constants.mjs" www/generated/constants.mjs
