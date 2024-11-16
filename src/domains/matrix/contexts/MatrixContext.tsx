import React, { createContext, useContext, useState, useEffect } from 'react'
import { Topic, Style, Idea, MatrixCell, SelectedIdea, Writing } from '../types'
import { writingDB } from '@/services/db'

interface MatrixContextType {
  topics: Topic[]
  styles: Style[]
  ideas: Idea[]
  selectedIdeas: SelectedIdea[]
  writings: Writing[]
  addTopic: (topic: Omit<Topic, 'id'>) => void
  addStyle: (style: Omit<Style, 'id'>) => void
  addIdea: (idea: Omit<Idea, 'id' | 'createdAt'>) => void
  getIdeasForCell: (topicId: string, styleId: string) => Idea[]
  addToSelected: (idea: Idea, topic: string, style: string) => void
  removeFromSelected: (id: string) => void
  clearSelected: () => void
  createWriting: (selectedIdea: SelectedIdea) => void
  updateWriting: (id: string, text: string) => void
  getWriting: (id: string) => Writing | undefined
  deleteWriting: (id: string) => void
}

const MatrixContext = createContext<MatrixContextType | undefined>(undefined)

const STORAGE_KEY = {
  SELECTED_IDEAS: 'matrix-writer-selected-ideas',
  WRITINGS: 'matrix-writer-writings'
}

export function MatrixProvider({ children }: { children: React.ReactNode }) {
  const [topics, setTopics] = useState<Topic[]>([])
  const [styles, setStyles] = useState<Style[]>([])
  const [ideas, setIdeas] = useState<Idea[]>([])
  const [selectedIdeas, setSelectedIdeas] = useState<SelectedIdea[]>(() => {
    const saved = localStorage.getItem(STORAGE_KEY.SELECTED_IDEAS)
    return saved ? JSON.parse(saved) : []
  })
  const [writings, setWritings] = useState<Writing[]>([])

  useEffect(() => {
    localStorage.setItem(STORAGE_KEY.SELECTED_IDEAS, JSON.stringify(selectedIdeas))
  }, [selectedIdeas])

  useEffect(() => {
    localStorage.setItem(STORAGE_KEY.WRITINGS, JSON.stringify(writings))
  }, [writings])

  useEffect(() => {
    const loadWritings = async () => {
      try {
        const loadedWritings = await writingDB.getAllWritings()
        setWritings(loadedWritings)
      } catch (error) {
        console.error('Error loading writings:', error)
      }
    }
    loadWritings()
  }, [])

  const addTopic = (topic: Omit<Topic, 'id'>) => {
    setTopics(prev => [...prev, { ...topic, id: crypto.randomUUID() }])
  }

  const addStyle = (style: Omit<Style, 'id'>) => {
    setStyles(prev => [...prev, { ...style, id: crypto.randomUUID() }])
  }

  const addIdea = (idea: Omit<Idea, 'id' | 'createdAt'>) => {
    setIdeas(prev => [...prev, { 
      ...idea, 
      id: crypto.randomUUID(),
      createdAt: new Date()
    }])
  }

  const getIdeasForCell = (topicId: string, styleId: string) => {
    return ideas.filter(idea => 
      idea.topicId === topicId && idea.styleId === styleId
    )
  }

  const addToSelected = (idea: Idea, topic: string, style: string) => {
    const exists = selectedIdeas.some(selected => selected.id === idea.id)
    if (exists) return

    setSelectedIdeas(prev => [...prev, {
      id: idea.id,
      content: idea.content,
      topic,
      style,
      addedAt: new Date()
    }])
  }

  const removeFromSelected = (id: string) => {
    setSelectedIdeas(prev => prev.filter(idea => idea.id !== id))
  }

  const clearSelected = () => {
    setSelectedIdeas([])
  }

  const createWriting = async (selectedIdea: SelectedIdea) => {
    const newWriting: Writing = {
      id: selectedIdea.id,
      content: selectedIdea.content,
      topic: selectedIdea.topic,
      style: selectedIdea.style,
      text: '',
      createdAt: new Date(),
      updatedAt: new Date()
    }

    try {
      await writingDB.saveWriting(newWriting)
      setWritings(prev => [...prev, newWriting])
    } catch (error) {
      console.error('Error creating writing:', error)
      throw error
    }
  }

  const updateWriting = async (id: string, text: string) => {
    const writing = writings.find(w => w.id === id)
    if (!writing) return

    const updatedWriting = {
      ...writing,
      text,
      updatedAt: new Date()
    }

    try {
      await writingDB.saveWriting(updatedWriting)
      setWritings(prev => prev.map(w => 
        w.id === id ? updatedWriting : w
      ))
    } catch (error) {
      console.error('Error updating writing:', error)
      throw error
    }
  }

  const getWriting = async (id: string) => {
    try {
      return await writingDB.getWriting(id)
    } catch (error) {
      console.error('Error getting writing:', error)
      return undefined
    }
  }

  const deleteWriting = async (id: string) => {
    try {
      await writingDB.deleteWriting(id)
      setWritings(prev => prev.filter(w => w.id !== id))
    } catch (error) {
      console.error('Error deleting writing:', error)
      throw error
    }
  }

  return (
    <MatrixContext.Provider value={{
      topics,
      styles,
      ideas,
      selectedIdeas,
      writings,
      addTopic,
      addStyle,
      addIdea,
      getIdeasForCell,
      addToSelected,
      removeFromSelected,
      clearSelected,
      createWriting,
      updateWriting,
      getWriting,
      deleteWriting
    }}>
      {children}
    </MatrixContext.Provider>
  )
}

export const useMatrix = () => {
  const context = useContext(MatrixContext)
  if (!context) {
    throw new Error('useMatrix must be used within a MatrixProvider')
  }
  return context
} 