import React, { createContext, useContext, useState, useEffect } from 'react'
import { Topic, Style, Idea, MatrixCell, SelectedIdea, Writing } from '../types'

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
  const [writings, setWritings] = useState<Writing[]>(() => {
    const saved = localStorage.getItem(STORAGE_KEY.WRITINGS)
    return saved ? JSON.parse(saved) : []
  })

  useEffect(() => {
    localStorage.setItem(STORAGE_KEY.SELECTED_IDEAS, JSON.stringify(selectedIdeas))
  }, [selectedIdeas])

  useEffect(() => {
    localStorage.setItem(STORAGE_KEY.WRITINGS, JSON.stringify(writings))
  }, [writings])

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
    // 檢查是否已經存在相同的點子
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

  const createWriting = (selectedIdea: SelectedIdea) => {
    const newWriting: Writing = {
      id: selectedIdea.id,
      content: selectedIdea.content,
      topic: selectedIdea.topic,
      style: selectedIdea.style,
      text: '',
      createdAt: new Date(),
      updatedAt: new Date()
    }
    setWritings(prev => [...prev, newWriting])
    // 可選：從 selectedIdeas 中移除
    // setSelectedIdeas(prev => prev.filter(idea => idea.id !== selectedIdea.id))
  }

  const updateWriting = (id: string, text: string) => {
    setWritings(prev => prev.map(writing => 
      writing.id === id 
        ? { ...writing, text, updatedAt: new Date() }
        : writing
    ))
  }

  const getWriting = (id: string) => {
    return writings.find(writing => writing.id === id)
  }

  const deleteWriting = (id: string) => {
    setWritings(prev => prev.filter(writing => writing.id !== id))
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