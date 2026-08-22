// @ts-check
import { defineConfig } from '@rsbuild/core';
import { pluginNodePolyfill } from '@rsbuild/plugin-node-polyfill';

// Docs: https://rsbuild.rs/config/
export default defineConfig({
    plugins: [pluginNodePolyfill()],
    html: {
        title: 'spell-monad',
        template: './www/index.html'
    },
    source: {
        entry: {
            index: './www/index.ts'
        },
    },
    server: {
        publicDir: {
            name: "www/public"
        },
        base: "/spell-monad"
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
