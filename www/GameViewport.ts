import { Application } from "pixi.js"

export class GameViewport {
    constructor(app: Application) {
        this.app = app
    }

    app: Application
}
