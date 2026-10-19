import { Button, Dialog, type DialogOptions, ProgressBar, type ProgressBarOptions } from "@pixi/ui"
import { BitmapText, Color, Graphics, Text, type CanvasTextOptions, Container, type GraphicsOptions, type ColorSource, type ContainerChild, type ContainerOptions } from "pixi.js"
import { Signal } from "typed-signals"

const WIDTH = 400
const HEIGHT = 280
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
const BUTTON_WIDTH = 150
const BUTTON_HEIGHT = 40

const PROGRESS_WIDTH = 450
const PROGRESS_HEIGHT = 35
const PROGRESS_BORDER = 3
const PROGRESS_FILL_COLOUR = BUTTON_COLOUR

export const DOWNLOAD_CONFIRM_DIALOG_TEXT =
    "Pressing 'OK' will fetch a ~85MiB blob containing binaries necessary to "
    + "run the game, such as GHC shared libraries. If you are on a metered "
    + "connection, you may wish to leave the page now instead.\n\n"
    + "Are you sure that you want to continue?"

export class DownloadDialog extends Dialog {
    onConfirmDownload: Signal<() => void> = new Signal()

    constructor() {
        const buttonBg = new Graphics()
        const button = new Button(buttonBg)
        const textInstance = new Text({
            text: 'OK',
            style: {
                fontSize: 22
            }
        })

        const options: DialogOptions = {
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
                text: DOWNLOAD_CONFIRM_DIALOG_TEXT,
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
            buttons: [button],
            buttonList: {
                elementsMargin: 40
            },
            width: WIDTH,
            height: HEIGHT,
            padding: PADDING,
            closeOnBackdropClick: false,
        }

        const defaultButton = (fillColor = BUTTON_COLOUR) => {
            buttonBg.clear().roundRect(0, 0, BUTTON_WIDTH, BUTTON_HEIGHT, RADIUS).fill(fillColor)
        }

        button.onPress.connect(() => defaultButton(BUTTON_PRESSED_COLOUR))
        button.onDown.connect(() => defaultButton(BUTTON_PRESSED_COLOUR))
        button.onUp.connect(() => defaultButton(BUTTON_COLOUR))
        button.onHover.connect(() => defaultButton(BUTTON_HOVER_COLOUR))
        button.onOut.connect(() => defaultButton(BUTTON_COLOUR))
        button.onUpOut.connect(() => defaultButton(BUTTON_COLOUR))

        defaultButton()

        textInstance.x = buttonBg.width / 2
        textInstance.y = buttonBg.height / 2
        textInstance.anchor.set(0.5)

        buttonBg.addChild(textInstance)

        super(options)

        const that = this
        this.onSelect.connect(() => {
            that.onConfirmDownload.emit()
            that.close()
        })
    }
}

export class DownloadProgressBar extends ProgressBar {
    constructor(onProgressUpdate: Signal<(downloadedBytes: number, toDownloadBytes: number) => void>) {
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

        const options: ProgressBarOptions = {
            progress: 0,
            bg: progressBg,
            fill: progressFill
        }

        super(options)

        const progressText = new BitmapText({
            text: "",
            style: {
                fontFamily: 'monospace',
                fontWeight: 'bold',
                fill: 'white',
                fontSize: 24
            }
        })
        progressText.anchor.set(0.5)
        progressText.x = this.width / 2
        progressText.y = this.height / 2

        this.addChild(progressText)

        // https://stackoverflow.com/questions/1081499/accessing-an-objects-property-from-an-event-listener-call-in-javascript
        // this is so fucking stupid 💔 🥀
        const that = this

        onProgressUpdate.connect((downloadedBytes, toDownloadBytes) => {
            progressText.text = `${((downloadedBytes) / (1024 * 1024)).toFixed(1)} / ${(toDownloadBytes / (1024 * 1024)).toFixed(1)} MiB`
            if (toDownloadBytes !== 0) {
                that.progress = 100 * downloadedBytes / toDownloadBytes
            }
            // mildly unnecessary edge cases
            else if (downloadedBytes === 0) {
                that.progress = 0
            } else {
                that.progress = 100
            }
        })
    }
}

export class LoadingText extends Text {
    constructor() {
        const options: CanvasTextOptions = {
            text: "Loading...",
            style: {
                fontSize: 36,
                align: 'center',
                fontWeight: 'bold',
                fill: 'white',
                fontFamily: 'monospace'
            }
        }

        super(options)
    }
}

export interface BorderOptions extends GraphicsOptions {
    borderColor?: ColorSource
    borderThickness?: number
    borderMargin?: number
}

export class Border extends Graphics {
    borderColor
    borderThickness
    borderMargin

    constructor(options?: BorderOptions) {
        super(options)

        this.borderColor = options?.borderColor || 'white'
        this.borderThickness = options?.borderThickness || 1
        this.borderMargin = options?.borderMargin || 0

        this.#drawBorder(this.borderColor, this.borderThickness, this.borderMargin)
    }

    redraw() {
        this.clear()
        this.#drawBorder(this.borderColor, this.borderThickness, this.borderMargin)
    }

    #drawBorder(borderColor: ColorSource, borderThickness: number, borderMargin: number) {
        const bounds = this.getLocalBounds()
        this.rect(bounds.minX - borderThickness - borderMargin
            , bounds.minY - borderThickness - borderMargin
            , bounds.width + 2 * borderThickness + 2 * borderMargin
            , bounds.height + 2 * borderThickness + 2 * borderMargin)
            .fill({ color: borderColor })
            .rect(bounds.minX - borderMargin, bounds.minY - borderMargin, bounds.width + 2 * borderMargin, bounds.height + 2 * borderMargin)
            .cut()
    }
}

export interface GameWindowOptions extends ContainerOptions {
    windowSize?: { w: number, h: number }
}

export class GameWindow extends Container {
    set windowSize(ws: { w: number, h: number }) {
        this.#windowSize = ws
        this.#redraw()
    }

    get windowSize() {
        return this.#windowSize
    }

    #background = new Graphics()

    #windowSize: { w: number, h: number }

    #redraw() {
        this.#background.clear()
        this.#drawBackground()
    }

    #drawBackground() {
        console.log(this.#windowSize)
        this.#background.rect(-(this.#windowSize.w / 2), -(this.#windowSize.h / 2), this.#windowSize.w, this.#windowSize.h).fill('black')
    }

    constructor(options?: GameWindowOptions) {
        super(options)

        this.#windowSize = { w: options?.windowSize?.w || 1000, h: options?.windowSize?.h || 1000 }
        this.#drawBackground()
        this.addChild(this.#background)
    }
}
