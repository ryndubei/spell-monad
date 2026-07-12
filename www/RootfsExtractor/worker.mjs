import {
    ConsoleStdout,
    File,
    OpenFile,
    PreopenDirectory,
    WASI,
} from '@bjorn3/browser_wasi_shim'

const rootfs = new PreopenDirectory("/", []);

const bsdtar_wasi = new WASI(
    ["bsdtar.wasm", "-x"],
    [],
    [
        new OpenFile(new File(new Uint8Array(), { readonly: true })),
        ConsoleStdout.lineBuffered((msg) => console.info(msg)),
        ConsoleStdout.lineBuffered((msg) => console.warn(msg)),
        rootfs,
    ],
    { debug: false }
);

const rootfs_stream = new Promise(res => {
    onmessage = (e) => {
        res(e.data)
        onmessage = null;
    }
});

const [{ instance }, rootfs_bytes] = await Promise.all([
    WebAssembly.instantiateStreaming(
        fetch("/spell-monad/bsdtar.wasm"),
        { wasi_snapshot_preview1: bsdtar_wasi.wasiImport }
    ),
    rootfs_stream.then(stream => new Response(stream).arrayBuffer())
]);

bsdtar_wasi.fds[0] = new OpenFile(
    new File(rootfs_bytes, { readonly: true })
);

const wasi_result = bsdtar_wasi.start(instance);

postMessage({wasi_result, rootfs})