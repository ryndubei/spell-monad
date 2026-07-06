// @ts-check
import { defineConfig } from '@rsbuild/core';
import { pluginNodePolyfill } from '@rsbuild/plugin-node-polyfill';

// Docs: https://rsbuild.rs/config/
export default defineConfig({
    plugins: [pluginNodePolyfill()],
    source: {
        entry: {
            index: './www/index.mjs'
        },
    },
    server: {
        publicDir: {
            name: "www/public"
        },
    },
    output: {
        minify: false,
        sourceMap: {
            js: 'source-map',
            css: true,
            extract: true,
        },
    }
});
