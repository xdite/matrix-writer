import React from 'react'
import { MatrixProvider } from './domains/matrix/contexts/MatrixContext'
import { ContentMatrixGenerator } from './domains/matrix/components/ContentMatrixGenerator'
import { Settings } from './components/Settings'
import { CartButton } from './components/CartButton'
import { Toaster } from '@/components/ui/toaster'

function App() {
  return (
    <MatrixProvider>
      <div className="min-h-screen bg-gray-100 p-8 relative">
        <div className="absolute top-4 right-4 flex items-center gap-4">
          <CartButton />
          <Settings />
        </div>
        <div className="max-w-7xl mx-auto space-y-8">
          <h1 className="text-3xl font-bold">Content Matrix Generator</h1>
          <ContentMatrixGenerator />
        </div>
        <Toaster />
      </div>
    </MatrixProvider>
  )
}

export default App
