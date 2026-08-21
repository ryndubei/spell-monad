import { PreopenDirectory } from "@bjorn3/browser_wasi_shim";
import { HS_SEARCH_DIR, CABAL_DYN_LIB_DIRS, MAIN_SO_PATH, MAIN_SO_BASE_NAME } from "./generated/constants.mjs";
import { main, DyLDBrowserHost } from './ghc/dyld.mjs'
import { Application } from 'pixi.js'
import type { Screen } from './Screen.ts'
import { EmptyScreen } from './Screen.ts'

declare global {
    /**
     * The Haskell part of the game controls the user interface by calling methods on
     * __GAME_VIEWPORT over Javascript FFI.
     */
    var __GAME_VIEWPORT: GameViewport
}

export class Game {
    #app = new Application()
    viewport = new GameViewport(this.#app)

    async init() {
        await this.#app.init(
            { resizeTo: window })

        // Needs to be in globalThis to be exposed to the game.
        // Alternatively, could sneak in an instance of InodeMem with extra methods
        // to rootfs, but that is a far more annoying approach.
        globalThis.__GAME_VIEWPORT = this.viewport

        // Show the canvas as soon as the DOM is loaded
        if (document.readyState === "loading") {
            document.addEventListener("DOMContentLoaded", () => {
                document.body.appendChild(this.#app.canvas)
            }, { once: true })
        } else {
            document.body.appendChild(this.#app.canvas)
        }

        console.log("Graphics initialised")
    }

    async run(rootfs: PreopenDirectory) {
        const dyld = await
            main({
                rpc: new DyLDBrowserHost({ rootfs: rootfs } as any),
                searchDirs: [
                    "/tmp/clib",
                    HS_SEARCH_DIR,
                ].concat(CABAL_DYN_LIB_DIRS),
                mainSoPath: MAIN_SO_PATH,
                args: [MAIN_SO_BASE_NAME, "+RTS", "-c", "-RTS"],
                isIserv: false,
            })

        console.log("DyLD loaded")

        // "run_game" must be exported by the main .so
        await dyld.exportFuncs.run_game();
    }
}

export class GameViewport {
    constructor(app: Application) {
        this.#app = app
    }

    #app: Application

    #screen: Screen = new EmptyScreen

    get screen(): Screen {
        return this.#screen
    }

    set screen(s: Screen) {
        this.#screen.cleanup(this.#app)
        this.#screen = s
        s.setup(this.#app)
    }
}
