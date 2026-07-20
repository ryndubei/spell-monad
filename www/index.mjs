import {
    ConsoleStdout,
    File,
    OpenFile,
    PreopenDirectory,
    Directory,
} from '@bjorn3/browser_wasi_shim';
import { DyLDBrowserHost, main } from "./ghc/dyld.mjs"
import { HS_SEARCH_DIR, MAIN_SO_PATH, MAIN_SO_BASE_NAME, CABAL_DYN_LIB_DIRS } from './generated/constants.mjs';
import { RootfsExtractor } from './RootfsExtractor.ts'
import { Game } from './Game.ts'

const game = new Game()
const game_initialised = game.init()

if (!("WebAssembly" in window)) {
    throw new Error("No WebAssembly")
} else {
    console.log("WebAssembly present")
}

const [[rootfs_stream1, rootfs_stream2], rootfsStreamLength] = await fetch("/spell-monad/rootfs.tar.zst")
    .then((r) => {
        console.log(r);
        return [r.body.tee(), r.headers.get('content-length')];
    });

console.log(`Fetching and extracting rootfs (${(rootfsStreamLength / (1024 * 1024)).toFixed(1)}MiB)...`)

const rootfs_extractor = new RootfsExtractor(rootfs_stream2)

var progress = 0
var dprogress = 0

for await (const chunk of rootfs_stream1) {
    progress += chunk.length;
    dprogress += chunk.length;
    if (dprogress >= (5 * 1024 * 1024)) {
        console.log(`${(progress / (1024 * 1024)).toFixed(1)}MiB`)
        dprogress = 0
    }
}

console.log("rootfs.tar.zst downloaded")

const rootfs = await rootfs_extractor.rootfs

console.log(rootfs)
console.log("rootfs extracted")

await game_initialised
await game.run(rootfs)
