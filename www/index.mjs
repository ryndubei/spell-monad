import { Game } from './Game.ts'
import { DownloadScreen } from './Screen.ts'

const game = new Game()
const game_initialised = game.init()

if (!("WebAssembly" in window)) {
    throw new Error("No WebAssembly")
} else {
    console.log("WebAssembly present")
}

await game_initialised

game.viewport.screen = new DownloadScreen 
const rootfs = await game.viewport.screen.rootfs
console.log("rootfs extracted")
console.log(rootfs)

await game.run(rootfs)

