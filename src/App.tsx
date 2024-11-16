import React from 'react'
import { BrowserRouter, Routes, Route } from 'react-router-dom'
import { MatrixProvider } from './domains/matrix/contexts/MatrixContext'
import { ContentMatrixGenerator } from './domains/matrix/components/ContentMatrixGenerator'
import { WritingPage } from './pages/WritingPage'
import { WritingsPage } from './pages/WritingsPage'
import { Navbar } from './components/Navbar'
import { Toaster } from '@/components/ui/toaster'

function App() {
  return (
    <BrowserRouter>
      <MatrixProvider>
        <div className="min-h-screen bg-gray-100">
          <Navbar />
          <div className="p-2">
            <Routes>
              <Route path="/" element={
                <div className="max-w-7xl mx-auto space-y-8">
                  <ContentMatrixGenerator />
                </div>
              } />
              <Route path="/writings" element={<WritingsPage />} />
              <Route path="/writing/:id" element={<WritingPage />} />
            </Routes>
          </div>
          <Toaster />
        </div>
      </MatrixProvider>
    </BrowserRouter>
  )
}

export default App
