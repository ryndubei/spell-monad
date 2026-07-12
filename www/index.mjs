import {
    ConsoleStdout,
    File,
    OpenFile,
    PreopenDirectory,
    Directory,
} from '@bjorn3/browser_wasi_shim';
import { DyLDBrowserHost, main } from "./ghc/dyld.mjs"
import { Terminal } from '@xterm/xterm'
import { FitAddon } from '@xterm/addon-fit'
import { openpty, Flags } from 'xterm-pty'
import { HS_SEARCH_DIR, MAIN_SO_PATH, MAIN_SO_BASE_NAME, CABAL_DYN_LIB_DIRS } from './generated/constants.mjs';
import { RootfsExtractor } from './RootfsExtractor.mjs'

import './xterm.css';
import './index.css';

document.querySelector('#root').innerHTML = `
<div id="terminal"></div>
`;

const term = new Terminal()
const fitAddon = new FitAddon()
term.open(document.getElementById('terminal'));

const { master, slave } = openpty();

term.loadAddon(master)
term.loadAddon(fitAddon)
fitAddon.fit()

addEventListener("resize", (event) => { fitAddon.fit() })

function set_echo(b) {
    const cfg = slave.ioctl('TCGETS')
    if ((cfg.ECHO_P) !== b) {
        slave.ioctl('TCSETS',
            {
                iflag: cfg.iflag,
                oflag: cfg.oflag,
                cflag: cfg.cflag,
                lflag: cfg.lflag ^ Flags.ECHO,
                cc: cfg.cc
            })
    }
}

set_echo(false)

slave.write('Hello from \x1B[1;3;31mxterm.js\x1B[0m\n')

const term_logger = Object.create(console)
term_logger.debug = function (data, cb) {
    console.debug(data)
    slave.write(`${data}\n`, cb)
}
term_logger.log = function (data, cb) {
    console.log(data)
    slave.write(`${data}\n`, cb)
}
term_logger.info = function (data, cb) {
    console.info(data)
    slave.write(`${data}\n`, cb)
}
term_logger.warn = function (data, cb) {
    console.warn(data)
    slave.write(`${data}\n`, cb)
}
term_logger.error = function (data, cb) {
    console.error(data)
    slave.write(`${data}\n`, cb)
}
term_logger.crash = function (data, cb) {
    console.error(data)
    slave.write(`${data}\n`, cb)
    throw new Error(`${data}`)
}

if (!"WebAssembly" in window) {
    term_logger.crash("No WebAssembly")
} else {
    term_logger.log("WebAssembly present")
}

const [[rootfs_stream1, rootfs_stream2], rootfsStreamLength] = await fetch("/spell-monad/rootfs.tar.zst")
    .then((r) => {
        console.log(r);
        return [r.body.tee(), r.headers.get('content-length')];
    });

term_logger.log("Fetching and extracting rootfs...")

const rootfs_extractor = new RootfsExtractor(rootfs_stream2)

var progress = 0

slave.write(`0.0 /${(rootfsStreamLength / (1024 * 1024)).toFixed(1)}MiB`)
for await (const chunk of rootfs_stream1) {
    progress += chunk.length;
    slave.write(`\r${(progress / (1024 * 1024)).toFixed(1)}`)
}
slave.write('\n')
term_logger.log("rootfs.tar.zst downloaded")

const rootfs = await rootfs_extractor.rootfs.catch( r => term_logger.crash(r) )

{
const ERRNO_NODEV = 43
const ERRNO_PIPE = 64
let r
if ((r = rootfs.path_create_directory("dev"))) {
    throw new Error(`Failed to create /dev: ${r}`)
}
if ((r = rootfs.path_open(0, "dev/")).ret) {
    throw new Error(`Failed to open /dev: ${r}`)
}
if ((r = r.fd_obj.path_link("pty", new class extends File {
        path_open(oflags, fs_rights_base, fd_flags) {
            const fd_obj = new class extends OpenFile {
                ioctl() {
                    return slave.ioctl(...arguments)
                }
                onSignal() {
                    return slave.onSignal(...arguments)
                }
                onReadable() {
                    return slave.onReadable(...arguments)
                }
                get readable() {
                    return slave.readable
                }
                fd_allocate() {
                    return ERRNO_NODEV
                }
                fd_read(size) {
                    return { ret: 0, data: slave.read(size) }
                }
                fd_pread(size, offset) {
                    return { ret: ERRNO_PIPE, data: new Uint8Array() }
                }
                fd_seek(offset, whence) {
                    return { ret: ERRNO_PIPE, offset: BigInt(0) }
                }
                fd_tell() {
                    return { ret: ERRNO_PIPE, offset: BigInt(0) }
                }
                fd_write(data) {
                    slave.write(Array.from(data))
                    return { ret: 0, nwritten: data.length }
                }
                fd_pwrite(data) {
                    return { ret: ERRNO_PIPE, nwritten: 0 }
                }
                file_pos = BigInt(0)
            }(this)
            return { ret: 0, fd_obj }
        }
    }([]))
    )) {
    throw new Error(`Failed to create /dev/pty: ${r}`)
}
}

console.log(rootfs)

term_logger.log("rootfs extracted")

if (document.readyState === "loading") {
    await new Promise((res) =>
        document.addEventListener("DOMContentLoaded", res, { once: true })
    );
}

term_logger.log("Initialising DyLDBrowserHost...")
try {
    const dyld = await main({
        rpc: new DyLDBrowserHost({
            rootfs,
            stdout: msg => term_logger.info(`${msg}`),
            stderr: msg => term_logger.warn(`${msg}`)
        }),
        searchDirs: [
            "/tmp/clib",
            HS_SEARCH_DIR,
        ].concat(CABAL_DYN_LIB_DIRS),
        mainSoPath: MAIN_SO_PATH,
        args: [MAIN_SO_BASE_NAME, "+RTS", "-c", "-RTS"],
        isIserv: false,
    });

    term_logger.log("DyLDBrowserHost loaded")

    await dyld.exportFuncs.run_game();
} catch (err) {
    term_logger.error(`${err}`)
}

