import { Game } from './Game.ts'
import { DownloadScreen } from './Screen.ts'
import { ARCHIVE_CHECKSUM } from './generated/constants.mjs'
import { RootfsExtractor } from './RootfsExtractor.ts'

const game = new Game()
const game_initialised = game.init()

if (!("WebAssembly" in window)) {
    throw new Error("No WebAssembly")
} else {
    console.log("WebAssembly present")
}

const ROOTFS_CACHE_NAME = "rootfs-" + ARCHIVE_CHECKSUM

// remove any other rootfs caches
caches.keys().then(async ks => {
    ks.forEach(async k => {
        if (k.startsWith("rootfs-") && k !== ROOTFS_CACHE_NAME) {
            (async () => { await caches.delete(k) })()
        }
    })
})

// Check cache for rootfs.tar.zst, redownload if missing
const rootfs_cache = await caches.open(ROOTFS_CACHE_NAME)

const rootfs_req = (await rootfs_cache.match("/spell-monad/rootfs.tar.zst"))

let rootfs
if (rootfs_req?.ok && rootfs_req.body !== undefined && rootfs_req.body !== null) {
    rootfs = rootfs_req.body
    console.log("Found a cached rootfs.tar.zst with matching checksum")
} else {
    console.log("Did not find a cached rootfs.tar.zst with matching checksum")
    rootfs = null
}

if (rootfs != null) {
    try {
        const rootfs_extractor = new RootfsExtractor(rootfs)
        rootfs = await rootfs_extractor.rootfs
    } catch (e) {
        console.error(e)
        console.warn("Redownloading rootfs due to extraction failure")
        await game_initialised
        game.viewport.screen = new DownloadScreen(rootfs_cache) 
        rootfs = await (game.viewport.screen as DownloadScreen).rootfs
    }
} else {
    console.log("Did not find already-downloaded rootfs.tar.zst, downloading again")
    await game_initialised
    game.viewport.screen = new DownloadScreen(rootfs_cache)
    rootfs = await (game.viewport.screen as DownloadScreen).rootfs
}
console.log("rootfs extracted")
console.log(rootfs)

await game_initialised
await game.run(rootfs)

