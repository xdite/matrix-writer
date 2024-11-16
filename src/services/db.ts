import { Writing } from '@/domains/matrix/types'

const DB_NAME = 'matrix-writer'
const STORE_NAME = 'writings'
const DB_VERSION = 1

export class WritingDB {
  private db: IDBDatabase | null = null

  async connect(): Promise<void> {
    return new Promise((resolve, reject) => {
      const request = indexedDB.open(DB_NAME, DB_VERSION)

      request.onerror = () => reject(request.error)
      request.onsuccess = () => {
        this.db = request.result
        resolve()
      }

      request.onupgradeneeded = (event) => {
        const db = (event.target as IDBOpenDBRequest).result
        if (!db.objectStoreNames.contains(STORE_NAME)) {
          db.createObjectStore(STORE_NAME, { keyPath: 'id' })
        }
      }
    })
  }

  async saveWriting(writing: Writing): Promise<void> {
    if (!this.db) await this.connect()
    
    return new Promise((resolve, reject) => {
      const transaction = this.db!.transaction([STORE_NAME], 'readwrite')
      const store = transaction.objectStore(STORE_NAME)
      
      const request = store.put(writing)
      
      request.onerror = () => reject(request.error)
      request.onsuccess = () => resolve()
    })
  }

  async getWriting(id: string): Promise<Writing | undefined> {
    if (!this.db) await this.connect()
    
    return new Promise((resolve, reject) => {
      const transaction = this.db!.transaction([STORE_NAME], 'readonly')
      const store = transaction.objectStore(STORE_NAME)
      
      const request = store.get(id)
      
      request.onerror = () => reject(request.error)
      request.onsuccess = () => resolve(request.result)
    })
  }

  async getAllWritings(): Promise<Writing[]> {
    if (!this.db) await this.connect()
    
    return new Promise((resolve, reject) => {
      const transaction = this.db!.transaction([STORE_NAME], 'readonly')
      const store = transaction.objectStore(STORE_NAME)
      
      const request = store.getAll()
      
      request.onerror = () => reject(request.error)
      request.onsuccess = () => resolve(request.result)
    })
  }

  async deleteWriting(id: string): Promise<void> {
    if (!this.db) await this.connect()
    
    return new Promise((resolve, reject) => {
      const transaction = this.db!.transaction([STORE_NAME], 'readwrite')
      const store = transaction.objectStore(STORE_NAME)
      
      const request = store.delete(id)
      
      request.onerror = () => reject(request.error)
      request.onsuccess = () => resolve()
    })
  }
}

export const writingDB = new WritingDB() 