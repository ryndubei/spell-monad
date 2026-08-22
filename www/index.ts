import { Game } from './Game.ts'
import { DownloadScreen } from './Screen.ts'
import { ARCHIVE_CHECKSUM } from './generated/constants.mjs'
import { RootfsExtractor } from './RootfsExtractor.ts'
import { BlobsDB } from './BlobsDB.ts'

const game = new Game()
const game_initialised = game.init()

if (!("WebAssembly" in window)) {
    throw new Error("No WebAssembly")
} else {
    console.log("WebAssembly present")
}

await game_initialised

// Check persistent storage for rootfs.tar.zst,
// redownload if missing

const blobs_db = new BlobsDB

let rootfs_blob: Blob | null

try {
    await blobs_db.open()

    const checksum = await blobs_db.get("checksum")

    if (checksum == ARCHIVE_CHECKSUM) {
        rootfs_blob = await blobs_db.get("archive")
    } else {
        console.warn("Rootfs blob found, but checksum does not match")
        rootfs_blob = null
    }
} catch (e) {
    console.error(e)
    rootfs_blob = null
}

let rootfs

if (rootfs_blob != null) {
    console.log("Found existing rootfs blob")
    try {
        const rootfs_extractor = new RootfsExtractor(rootfs_blob.stream())
        rootfs = await rootfs_extractor.rootfs
    } catch (e) {
        console.error(e)
        console.warn("Redownloading rootfs due to extraction failure")
        game.viewport.screen = new DownloadScreen(blobs_db) 
        rootfs = await (game.viewport.screen as DownloadScreen).rootfs
    }
} else {
    console.log("Did not find already-downloaded rootfs.tar.zst, downloading again")
    game.viewport.screen = new DownloadScreen(blobs_db)
    rootfs = await (game.viewport.screen as DownloadScreen).rootfs
}

blobs_db.close()

console.log("rootfs extracted")
console.log(rootfs)

await game.run(rootfs)

