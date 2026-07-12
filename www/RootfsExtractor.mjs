import { PreopenDirectory, File, Directory } from '@bjorn3/browser_wasi_shim'

export class RootfsExtractor {
    #worker = new Worker(new URL('./RootfsExtractor/worker.mjs', import.meta.url))

    #resolveResult
    /**
     * @type {Promise<PreopenDirectory>}
     */
    rootfs = new Promise(res => {
        this.#resolveResult = res 
    }).then( msg => {
        if (msg.data.wasi_result === 0) {
            return this.#reconstructRootfs(msg.data.rootfs)
        } else {
            throw new Error(`Failed to extract rootfs: ${msg.data.wasi_result}`)
        }
    })

    /**
     * Takes ownership of the stream. The stream may no longer be used after
     * passing it to the constructor.
     * @param {ReadableStream<Uint8Array<ArrayBuffer>>} stream 
     */
    constructor(stream) {
        this.#worker.postMessage(stream, [stream])
        this.#worker.onmessage = msg => {
            this.#resolveResult(msg)
        }
    }


    // mutably convert the received rootfs back to a PreopenDirectory
    // needed because worker thread messages lose the class methods
    #reconstructRootfs(rfs) {
        function go(m) {
            for (const k of m.keys()) {
                const ino = m.get(k)
                // is a directory
                if ('contents' in ino) {
                    m.set(k, new Directory(go(ino.contents)))
                    // is a file
                } else if ('data' in ino) {
                    m.set(k, new File(ino.data))
                } else {
                    throw new Error("unexpected structure found in rootfs")
                }
            }
            return m
        }
        return new PreopenDirectory("/", go(rfs.dir.contents))
    }
}