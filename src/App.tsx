import React from 'react'
import { MatrixProvider } from './domains/matrix/contexts/MatrixContext'
import { ContentMatrixGenerator } from './domains/matrix/components/ContentMatrixGenerator'
import { Settings } from './components/Settings'
import { Toaster } from '@/components/ui/toaster'

function App() {
  return (
    <MatrixProvider>
      <div className="min-h-screen bg-gray-100 p-8 relative">
        <Settings />
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
