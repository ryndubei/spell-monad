import type { PreopenDirectory } from '@bjorn3/browser_wasi_shim'
import { Application, BitmapText, Color, Graphics, Text, Ticker } from 'pixi.js'
import { RootfsExtractor } from './RootfsExtractor'
import { Button, Dialog, ProgressBar } from '@pixi/ui'

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

    /**
     * Call to begin downloading rootfs
     */
    #resolveConfirmation: () => void

    /**
    * Number of bytes downloaded so far
    */
    #rootfsDownloadProgress = 0

    /**
    * Total number of bytes to be downloaded
    */
    #rootfsDownloadSize = 0

    #dialog

    #progressBar

    #progressText

    #mkProgressString() {
        return `${(this.#rootfsDownloadProgress / (1024 * 1024)).toFixed(1)} / ${(this.#rootfsDownloadSize / (1024 * 1024)).toFixed(1)} MiB`
    }

    #ticker = new Ticker()

    async #extractRootfs(rootfsDownloadConfirmed: Promise<unknown>): Promise<PreopenDirectory> {

        const ROOTFS_URL = "/spell-monad/rootfs.tar.zst"

        await rootfsDownloadConfirmed

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

        this.#rootfsDownloadSize = rootfsStreamLength

        console.log(`Fetching and extracting rootfs (${(rootfsStreamLength / (1024 * 1024)).toFixed(1)}MiB)...`)

        const rootfs_extractor = new RootfsExtractor(rootfs_stream2)

        for await (const chunk of rootfs_stream1) {
            this.#rootfsDownloadProgress += chunk.length
        }

        console.log("rootfs.tar.zst downloaded")

        return await rootfs_extractor.rootfs.catch(async (e) => {
            // Remove rootfs from cache if extraction fails
            await this.#cache.delete(ROOTFS_URL)
            throw e
        })
    }

    #cache

    constructor(cache: Cache) {
        this.#cache = cache

        const WIDTH = 400
        const HEIGHT = 250
        const RADIUS = 20
        const PADDING = 20
        const PANEL_COLOUR = '#222425'
        const PANEL_BORDER_COLOUR = '#3E3F40'
        const BACKDROP_COLOUR = '#000000'
        const TITLE_COLOUR = '#FFFFFF'
        const CONTENT_COLOUR = '#FFFFFF'
        const BUTTON_COLOUR = '#E91E63'
        const BUTTON_HOVER_COLOUR = '#FF729A'
        const BUTTON_PRESSED_COLOUR = '#B42E5B'

        const PROGRESS_WIDTH = 450
        const PROGRESS_HEIGHT = 35
        const PROGRESS_BORDER = 3
        const PROGRESS_FILL_COLOUR = BUTTON_COLOUR

        const { promise: rootfsDownloadConfirmed, resolve } = Promise.withResolvers()

        this.rootfs = this.#extractRootfs(rootfsDownloadConfirmed)

        this.#resolveConfirmation = resolve as () => void

        const buttonBg = new Graphics()
        const button = new Button(buttonBg)
        const textInstance = new Text({
            text: 'OK',
            style: {
              fontSize: 22
            }
        })

        const defaultButton = (fillColor = BUTTON_COLOUR) => {
            buttonBg.clear().roundRect(0, 0, 150, 40, RADIUS).fill(fillColor)
        }

        button.onPress.connect(() =>  defaultButton(BUTTON_PRESSED_COLOUR))
        button.onDown.connect(() =>  defaultButton(BUTTON_PRESSED_COLOUR))
        button.onUp.connect(() =>  defaultButton(BUTTON_COLOUR))
        button.onHover.connect(() =>  defaultButton(BUTTON_HOVER_COLOUR))
        button.onOut.connect(() =>  defaultButton(BUTTON_COLOUR))
        button.onUpOut.connect(() =>  defaultButton(BUTTON_COLOUR))

        defaultButton()

        textInstance.x = buttonBg.width / 2
        textInstance.y = buttonBg.height / 2
        textInstance.anchor.set(0.5)

        buttonBg.addChild(textInstance)

        this.#dialog = new Dialog({
            background: new Graphics().roundRect(0, 0, WIDTH, HEIGHT, RADIUS).fill(PANEL_COLOUR).stroke({
                color: PANEL_BORDER_COLOUR,
                width: 1
            }),
            backdropColor: Color.shared.setValue(BACKDROP_COLOUR).toNumber(),
            title: new Text({ 
                text: 'Warning!',
                style: {
                  fontSize: 24,
                  fontWeight: 'bold',
                  fill: Color.shared.setValue(TITLE_COLOUR).toNumber()
                }
            }),
            content: new Text({
              // TODO: calculate size of the download at compile time?
                text: "Pressing 'OK' will fetch a ~85MiB blob containing binaries necessary to "
                + "run the game, such as GHC shared libraries. If you are on a metered "
                + "connection, you may wish to leave the page now instead.",
                style: {
                  fontSize: 16,
                  align: 'center',
                  fontWeight: 'normal',
                  wordWrapWidth: WIDTH - 2 * PADDING - PADDING,
                  wordWrap: true,
                  lineHeight: 20,
                  fill: Color.shared.setValue(CONTENT_COLOUR).toNumber()
                }
            }),
            buttons: [ button ],
            buttonList: {
              elementsMargin: 40
            },
            width: WIDTH,
            height: HEIGHT,
            padding: PADDING,
            closeOnBackdropClick: false,
        })
        this.#dialog.open()

        const progressBg = new Graphics()
            .roundRect(0, 0, PROGRESS_WIDTH, PROGRESS_HEIGHT, RADIUS)
            .fill(PANEL_BORDER_COLOUR)
            .roundRect(PROGRESS_BORDER, PROGRESS_BORDER, PROGRESS_WIDTH - PROGRESS_BORDER * 2, PROGRESS_HEIGHT - PROGRESS_BORDER * 2, RADIUS)
            .fill(PANEL_COLOUR)
        const progressFill = new Graphics()
            .roundRect(0, 0, PROGRESS_WIDTH, PROGRESS_HEIGHT, RADIUS)
            .fill(PANEL_BORDER_COLOUR)
            .roundRect(PROGRESS_BORDER, PROGRESS_BORDER, PROGRESS_WIDTH - PROGRESS_BORDER * 2, PROGRESS_HEIGHT - PROGRESS_BORDER * 2, RADIUS)
            .fill(PROGRESS_FILL_COLOUR)

        this.#progressBar = new ProgressBar({
            progress: 0,
            bg: progressBg,
            fill: progressFill,
          })

        this.#progressText = new BitmapText({
            text: "",
            style: {
                fontFamily: 'Arial',
                fontSize: 24
            }
        })
        this.#progressText.anchor.set(0.5)

        this.#progressText.x = this.#progressBar.width / 2
        this.#progressText.y = this.#progressBar.height / 2

        this.#progressBar.addChild(this.#progressText)
    }

    setup(app: Application): void {
        this.#dialog.x = app.screen.width / 2
        this.#dialog.y = app.screen.height / 2

        app.renderer.on('resize', (w,h) => {
            this.#dialog.x = w / 2
            this.#dialog.y = h / 2
        })

        this.#dialog.onSelect.connect(() => {
            console.log("Received download confirmation")
            this.#resolveConfirmation()
            app.stage.removeChild(this.#dialog)

            app.renderer.removeListener('resize')

            this.#progressBar.x = app.screen.width / 2 - (this.#progressBar.width / 2)
            this.#progressBar.y = app.screen.height / 2

            app.renderer.on('resize', (w,h) => {
              this.#progressBar.x = w / 2 - (this.#progressBar.width / 2)
              this.#progressBar.y = h / 2
            })

            app.stage.addChild(this.#progressBar)

            this.#ticker.add(() => {
                if (this.#rootfsDownloadSize != 0) {
                    this.#progressText.text = this.#mkProgressString()
                }
                this.#progressBar.progress = 100 * this.#rootfsDownloadProgress / this.#rootfsDownloadSize
            })
        })

        app.stage.addChild(this.#dialog)
        this.#ticker.start()
    }

    cleanup(app: Application): void {
        app.stage.removeChild(this.#dialog)
        app.stage.removeChild(this.#dialog)
        this.#dialog.destroy()
        this.#progressBar.destroy()
        this.#ticker.destroy()
        app.renderer.removeListener('resize')
    }
}

