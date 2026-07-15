PKG_NAME := spell-monad
PKG_VERSION := 0.1.0.0
export PKG_FULL_NAME := $(PKG_NAME)-$(PKG_VERSION)

HASKELL_SOURCES := $(shell find src -name '*.hs') spell-monad.cabal cabal.project cabal.project.local cabal.project.freeze

WEB_SOURCES := www/public/favicon.png $(wildcard www/*.css) $(wildcard www/*.mjs)
WEB_GENERATED := www/generated/constants.mjs \
	www/ghc/dyld.mjs \
	www/ghc/post-link.mjs \
	www/ghc/prelude.mjs \
	www/public/bsdtar.wasm \
	www/public/rootfs.tar.zst

ROOTFS_KEY_PATHS := _rootfs/tmp/ghc_env _rootfs/tmp/hslib/lib _rootfs/tmp/clib _rootfs/usr/share/terminfo/x/xterm

GHC := wasm32-wasi-ghc
export GHC_PKG := wasm32-wasi-ghc-pkg
export CABAL := wasm32-wasi-cabal
CLANG := wasm32-wasi-clang

GHC_LIBDIR := $(shell realpath $(shell $(GHC) --print-libdir))
C_LIBDIR := $(shell dirname $(shell which $(CLANG)))/../share/wasi-sysroot/lib/wasm32-wasi

GHC_LIBDIR_PREFIX := $(shell dirname $(GHC_LIBDIR))
export HS_SEARCHDIR := $(shell find $(GHC_LIBDIR) -name '*.so' -print0 -quit | xargs -0 -n1 dirname | sed 's|^$(GHC_LIBDIR_PREFIX)/|/|')


dist: node_modules $(WEB_SOURCES) $(WEB_GENERATED)
	npm run build
	touch -m dist

node_modules:
	npm install
	touch -m node_modules

dist-newstyle/build/wasm32-wasi: $(HASKELL_SOURCES)
	$(CABAL) build $(PKG_NAME)
	touch -m dist-newstyle/build/wasm32-wasi

# ghc_env is used as a marker for having copied all the cabal libraries to _rootfs as well
_rootfs/tmp/ghc_env www/generated/constants.mjs: dist-newstyle/build/wasm32-wasi
	GHC_ENV="$$($(CABAL) exec -- sh -c 'cat "$$GHC_ENVIRONMENT"')" ./copy-cabal-libs.sh

# Create minimal terminfo db in /usr/share/terminfo
_rootfs/usr/share/terminfo/x/xterm: scratch := $(shell mktemp -d)
_rootfs/usr/share/terminfo/x/xterm:
	mkdir -p $(scratch)

	IFS=":"; \
	set -o noglob; \
	for dir in $$TERMINFO_DIRS; do \
		cp -H "$$dir/x/xterm" $(scratch) || continue; \
		break; \
	done

	mkdir -p _rootfs/usr/share/terminfo/x

	cp --no-preserve=mode -r $(scratch)/* _rootfs/usr/share/terminfo/x

_rootfs/tmp/hslib/lib: scratch := $(shell mktemp -d)
_rootfs/tmp/hslib/lib:
	mkdir -p $(@D)

	mkdir -p $(scratch)/lib

	cp --no-preserve=mode -r $(GHC_LIBDIR)/* $(scratch)/lib
	
	rm -rf $(scratch)/lib/doc $(scratch)/lib/html $(scratch)/lib/latex $(scratch)/lib/*.mjs $(scratch)/lib/*.js $(scratch)/lib/*.txt
	
	# Delete unnecessary library files
	find $(scratch)/lib "(" \
		-name '*.hi' \
		-o -name '*.a' \
		-o -name '*.p_hi' \
		-o -name 'libHS*_p.a' \
		-o -name '*.p_dyn_hi' \
		-o -name 'libHS*_p*.so' \
		-o -name 'libHSrts*_debug*.so' \
		")" -delete

	# Remove Cabal fully from the libdir
	$(GHC_PKG) --no-user-package-db --global-package-db=$(scratch)/lib/package.conf.d unregister Cabal Cabal-syntax
	$(GHC_PKG) --no-user-package-db --global-package-db=$(scratch)/lib/package.conf.d recache
	rm -rf $(scratch)$(HS_SEARCHDIR)/*Cabal*


	if [[ -d $@ ]]; then \
		exch $(scratch)/lib $@; \
	else \
		mv $(scratch)/lib $@; \
	fi

_rootfs/tmp/clib: scratch := $(shell mktemp -d)
_rootfs/tmp/clib:
	mkdir -p $(@D)

	cp --no-preserve=mode $(C_LIBDIR)/* $(scratch)
	find $(scratch) -type f -follow ! -name '*.so' -delete
	rm -f $(scratch)/libsetjmp.so $$scratch/libwasi-emulated-*.so

	if [[ -d $@ ]]; then \
		exch $(scratch) $@; \
	else \
		mv $(scratch) $@; \
	fi

www/public/rootfs.tar.zst: $(ROOTFS_KEY_PATHS)
	tar --zstd -hcf $@ -C _rootfs .

www/public/bsdtar.wasm:
	cp --no-preserve=mode -T $(BSDTAR_WASM_PATH)/bin/bsdtar.wasm $@

www/ghc/dyld.mjs www/ghc/post-link.mjs www/ghc/prelude.mjs:
	mkdir -p $(@D)
	cp --no-preserve=mode $(GHC_LIBDIR)/dyld.mjs $(@D) 
	cp --no-preserve=mode $(GHC_LIBDIR)/post-link.mjs $(@D) 
	cp --no-preserve=mode $(GHC_LIBDIR)/prelude.mjs $(@D) 

	# Vendor URL imports
	sed -i 's|\"https://esm.sh/gh/haskell-wasm/browser_wasi_shim\"|"@bjorn3/browser_wasi_shim"|' $(@D)/dyld.mjs
	# Make #wasi a public field
	sed -i 's|#wasi|_wasi|' $(@D)/dyld.mjs

cabal.project.local:
	touch cabal.project.local

.PHONY: clean
clean:
	-rm -rf _rootfs
	-rm -rf dist
	-rm www/public/rootfs.tar.zst
	-rm www/public/bsdtar.wasm
	-rm -rf www/generated
	-rm -rf www/ghc
