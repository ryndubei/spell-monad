
export type BlobsDBEntry = { archive: Blob, checksum: string }

export class BlobsDB {
    #db: IDBDatabase | null = null

    isOpen = () => (this.#db != null)

    async open() {
        const {promise, resolve, reject} = Promise.withResolvers<IDBDatabase>()
  
        const db_req = window.indexedDB.open("blobs")
  
        db_req.onerror = () => reject(`Failed to open blobs DB: ${db_req.error}`)
        db_req.onsuccess = () => {
            console.log("Opened blobs DB")
            resolve(db_req.result)
        }
        db_req.onupgradeneeded = () => {
            const db = db_req.result

            const objectStore = db.createObjectStore("rootfs", {keyPath: "key"})

            // Currently, stores only two keys: the archive blob and the checksum string.
            objectStore.createIndex("value", "value")
            objectStore.createIndex("key", "key")

            resolve(db)
        }
  
        this.#db = await promise
    }

    async close() {
        this.#db?.close()
        this.#db = null
    }

    async get<T extends keyof BlobsDBEntry>(key: T): Promise<BlobsDBEntry[T]> {

        const { promise, resolve, reject } = Promise.withResolvers<BlobsDBEntry[T]>()

        if (this.#db == null) {
            throw new Error("Blobs DB is closed")
        }

        const transaction = (this.#db).transaction("rootfs", "readonly")

        transaction.onerror = () => reject(`Blobs DB transaction for key ${key} failed: ${transaction.error}`)
        transaction.onabort = () => reject(`Blobs DB transaction for key ${key} aborted`)

        const objectStore = transaction.objectStore("rootfs")

        const req = objectStore.get(key)

        req.onsuccess = () => {
            console.log(`Got entry of key ${key} from blobs DB`)
            resolve(req.result.value)
        }

        return await promise
    }

    async write(entry: BlobsDBEntry) {
        const { promise, resolve, reject } = Promise.withResolvers<void>()

        if (this.#db == null) {
            throw new Error("Blobs DB is closed")
        }

        const transaction = (this.#db).transaction("rootfs", "readwrite")

        transaction.onerror = () => reject(`Blobs DB write failed: ${transaction.error}`)
        transaction.onabort = () => reject(`Blobs DB write aborted`)
        transaction.oncomplete = () => {
            console.log("Wrote to blobs DB")
            resolve()
        }

        const objectStore = transaction.objectStore("rootfs")

        objectStore.put({key: "checksum", value: entry.checksum})
        objectStore.put({key: "archive", value: entry.archive})

        return await promise
    }
}
