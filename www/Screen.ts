import type { PreopenDirectory } from '@bjorn3/browser_wasi_shim'
import { Application, Container } from 'pixi.js'
import { RootfsExtractor } from './RootfsExtractor'
import { DownloadDialog, DownloadProgressBar, LoadingText, DOWNLOAD_CONFIRM_DIALOG_TEXT, GameWindow, Border } from './Widgets'
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

        this.#progressBar = new DownloadProgressBar(this.#onProgressUpdate)
        this.#progressBarWrapper = new Container({ children: [this.#progressBar] })
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

        const that = this
        this.#dialog.onConfirmDownload.connect(() => {
            app.stage.addChild(that.#progressBarWrapper)
        })

        // Try native dialog first, fall back to pixi.js dialog if cancelled or
        // blocked by the browser
        const response = window.confirm(DOWNLOAD_CONFIRM_DIALOG_TEXT)
        if (response) {
            this.#dialog.onConfirmDownload.emit()
        } else {
            this.#dialog.open()
        }
    }

    cleanup(): void {
        this.#dialog.destroy({ children: true })
        this.#progressBarWrapper.destroy({ children: true })
    }
}

export class LoadingScreen implements Screen {
    #loadingText

    constructor() {
        const t = new LoadingText()
        t.anchor.set(0.5)
        this.#loadingText = new Border({ children: [t], borderThickness: 3, borderMargin: 5 })
    }

    setup(app: Application) {
        app.stage.addChild(this.#loadingText)
    }

    cleanup() {
        this.#loadingText.destroy({ children: true })
    }
}

export class GameScreen implements Screen {
    #gameWindow
    #border

    constructor() {
        this.#gameWindow = new GameWindow()
        this.#border = new Border({ children: [this.#gameWindow], borderThickness: 4, borderMargin: 0 })
    }

    #listenerToRemove: null | ((w: number, h: number) => void) = null

    setup(app: Application) {
        this.#gameWindow.windowSize = { w: app.renderer.width - 16, h: app.renderer.height - 16 }
        this.#border.redraw()

        app.stage.addChild(this.#border)

        const thatGameWindow = this.#gameWindow
        const thatBorder = this.#border
        const onResize = (w: number, h: number) => {
            thatGameWindow.windowSize = { w: w - 16, h: h - 16 }
            thatBorder.redraw()
        }

        app.renderer.addListener('resize', onResize)
        this.#listenerToRemove = onResize
    }

    cleanup(app: Application) {
        this.#border.destroy({ children: true })
        const rsz = this.#listenerToRemove
        if (rsz !== null) {
            app.renderer.removeListener('resize', rsz)
        }
    }
}
