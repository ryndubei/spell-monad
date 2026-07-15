import type { PreopenDirectory } from "@bjorn3/browser_wasi_shim";
import { HS_SEARCH_DIR, CABAL_DYN_LIB_DIRS, MAIN_SO_PATH, MAIN_SO_BASE_NAME } from "./generated/constants.mjs";
import { main, DyLDBrowserHost } from './ghc/dyld.mjs'
import { Application } from 'pixi.js'
import { GameViewport } from './GameViewport.ts'

declare global {
    var __GAME_VIEWPORT: GameViewport
}

export class Game {
    constructor(rootfs: PreopenDirectory) {
        this.rootfs = rootfs
    }

    #app = new Application()

    rootfs: PreopenDirectory

    async run() {
        const [_, dyld] = await Promise.all([
            this.#app.init({ background: '#1099bb', resizeTo: window }),
            main({
                rpc: new DyLDBrowserHost({ rootfs: this.rootfs } as any),
                searchDirs: [
                    "/tmp/clib",
                    HS_SEARCH_DIR,
                ].concat(CABAL_DYN_LIB_DIRS),
                mainSoPath: MAIN_SO_PATH,
                args: [MAIN_SO_BASE_NAME, "+RTS", "-c", "-RTS"],
                isIserv: false,
            })
        ])

        console.log("DyLD loaded and graphics initialised")

        // Wait until manipulating the DOM is safe
        if (document.readyState === "loading") {
            await new Promise((res) =>
                document.addEventListener("DOMContentLoaded", res, { once: true })
            );
        }

        document.body.appendChild(this.#app.canvas)

        // Needs to be in globalThis to be exposed to the game.
        // Alternatively, could sneak in an instance of InodeMem with extra methods
        // to rootfs, but that is a far more annoying approach.
        globalThis.__GAME_VIEWPORT = new GameViewport(this.#app)

        // "run_game" must be exported by the main .so
        await dyld.exportFuncs.run_game();
    }
}
