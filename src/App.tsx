import React from 'react'
import { BrowserRouter, Routes, Route } from 'react-router-dom'
import { MatrixProvider } from './domains/matrix/contexts/MatrixContext'
import { ContentMatrixGenerator } from './domains/matrix/components/ContentMatrixGenerator'
import { Settings } from './components/Settings'
import { CartButton } from './components/CartButton'
import { Toaster } from '@/components/ui/toaster'
import { WritingPage } from './pages/WritingPage'

function App() {
  return (
    <BrowserRouter>
      <MatrixProvider>
        <div className="min-h-screen bg-gray-100 p-8">
          <div className="absolute top-8 right-8 flex gap-2">
            <CartButton />
            <Settings />
          </div>
          <Routes>
            <Route path="/" element={
              <div className="max-w-7xl mx-auto space-y-8">
                <h1 className="text-3xl font-bold">Content Matrix Generator</h1>
                <ContentMatrixGenerator />
              </div>
            } />
            <Route path="/writing/:ideaId" element={<WritingPage />} />
          </Routes>
          <Toaster />
        </div>
      </MatrixProvider>
    </BrowserRouter>
  )
}

export default App
