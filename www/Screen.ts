import type { PreopenDirectory } from '@bjorn3/browser_wasi_shim'
import { Application, Container } from 'pixi.js'
import { RootfsExtractor } from './RootfsExtractor'
import { DownloadDialog, DownloadProgressBar, LoadingText } from './Widgets'
import { Signal } from 'typed-signals'

export interface Screen {
    setup(app: Application): void
    cleanup(app: Application): void
}

export class EmptyScreen implements Screen {
    setup() { }
    cleanup() { }
}

export class DownloadScreen implements Screen {
    rootfs: Promise<PreopenDirectory>

    async #extractRootfs(): Promise<PreopenDirectory> {

        const ROOTFS_URL = "/spell-monad/rootfs.tar.zst"

        const [[rootfs_stream1, rootfs_stream2], rootfsStreamLength] = await fetch(ROOTFS_URL)
            .then((r) => {
                if (r.body != null) {

                    // request persistence so that the cache stays around longer
                    console.log("Asking for persistent storage...")
                    navigator.storage.persist().then(persistence => {
                        if (persistence) {
                            console.log("Got persistent storage")
                        } else {
                            console.warn("Did not get persistent storage")
                        }
                    })

                    // no await so that we can begin extracting asynchronously
                    this.#cache.put(ROOTFS_URL, r.clone())

                    console.log(r);
                    return [r.body.tee(), r.headers.get('content-length')];
                } else {
                    throw new Error("rootfs.tar.zst: empty body")
                }
            }) as any

        console.log(`Fetching and extracting rootfs (${(rootfsStreamLength / (1024 * 1024)).toFixed(1)}MiB)...`)

        const rootfs_extractor = new RootfsExtractor(rootfs_stream2)

        let progress = 0
        for await (const chunk of rootfs_stream1) {
            progress += chunk.length
            this.#onProgressUpdate.emit(progress, rootfsStreamLength)
        }

        console.log("rootfs.tar.zst downloaded")

        return await rootfs_extractor.rootfs.catch(async (e) => {
            // Remove rootfs from cache if extraction fails
            await this.#cache.delete(ROOTFS_URL)
            throw e
        })
    }

    #onProgressUpdate = new Signal<(downloadedBytes: number, toDownloadBytes: number) => void>()

    #cache

    #dialog

    #progressBar

    // to properly align the progress bar horizontally
    #progressBarWrapper

    constructor(cache: Cache) {
        this.#cache = cache

        this.#dialog = new DownloadDialog()
        this.#dialog.layout = true

        this.#progressBar = new DownloadProgressBar(this.#onProgressUpdate)
        this.#progressBarWrapper = new Container({children: [this.#progressBar]})
        this.#progressBarWrapper.layout = true
        this.#progressBar.x = -this.#progressBar.width / 2

        const that = this
        this.rootfs = new Promise<PreopenDirectory>((res) => {
            this.#dialog.onConfirmDownload.connect(() => {
                res(that.#extractRootfs())
            })
        })
    }

    setup(app: Application): void {
        app.stage.addChild(this.#dialog)
        this.#dialog.open()

        const that = this
        this.#dialog.onConfirmDownload.connect(() => {
            app.stage.addChild(that.#progressBarWrapper)
        })
    }

    cleanup(): void {
        this.#dialog.destroy()
        this.#progressBarWrapper.destroy()
    }
}

export class LoadingScreen implements Screen {
    #loadingText

    constructor() {
        this.#loadingText = new LoadingText()
        this.#loadingText.layout = true
        this.#loadingText.anchor.set(0.5)
    }

    setup(app: Application) {
        app.stage.addChild(this.#loadingText)
    }

    cleanup() {
        this.#loadingText.destroy()
    }
}
