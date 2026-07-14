import { PreopenDirectory, File, Directory, InodeMem } from '@bjorn3/browser_wasi_shim'

type WorkerMessage = {
    wasi_result: number
    rootfs: any
}

export class RootfsExtractor {
    #worker = new Worker(new URL('./RootfsExtractor/worker.mjs', import.meta.url))

    rootfs = (<Promise<MessageEvent<WorkerMessage>>>new Promise(res => {
        this.#worker.onmessage = msg => {
            res(msg)
        } 
    })).then( msg => {
        if (msg.data.wasi_result === 0) {
            return this.#reconstructRootfs(msg.data.rootfs)
        } else {
            throw new Error(`Failed to extract rootfs: ${msg.data.wasi_result}`)
        }
    })

    /**
     * Takes ownership of the stream. The stream may no longer be used after
     * passing it to the constructor.
     */
    constructor(stream: ReadableStream<Uint8Array<ArrayBufferLike>>) {
        this.#worker.postMessage(stream, [stream])
    }


    // mutably convert the received rootfs back to a PreopenDirectory
    // needed because worker thread messages lose the class methods
    #reconstructRootfs(rfs: any) {
        function go(m: Map<string, InodeMem>) {
            for (const [k, ino] of m.entries()) {
                // is a directory
                if ('contents' in ino) {
                    const contents = ino.contents as Map<string, InodeMem>
                    m.set(k, new Directory(go(contents)))
                    // is a file
                } else if ('data' in ino) {
                    const data = ino.data as ArrayBufferLike
                    m.set(k, new File(data))
                } else {
                    throw new Error("unexpected structure found in rootfs")
                }
            }
            return m
        }
        return new PreopenDirectory("/", go(rfs.dir.contents))
    }
}