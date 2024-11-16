import React, { createContext, useContext, useState, useEffect } from 'react'
import { Topic, Style, Idea, MatrixCell, SelectedIdea } from '../types'

interface MatrixContextType {
  topics: Topic[]
  styles: Style[]
  ideas: Idea[]
  selectedIdeas: SelectedIdea[]
  addTopic: (topic: Omit<Topic, 'id'>) => void
  addStyle: (style: Omit<Style, 'id'>) => void
  addIdea: (idea: Omit<Idea, 'id' | 'createdAt'>) => void
  getIdeasForCell: (topicId: string, styleId: string) => Idea[]
  addToSelected: (idea: Idea, topic: string, style: string) => void
  removeFromSelected: (id: string) => void
  clearSelected: () => void
}

const MatrixContext = createContext<MatrixContextType | undefined>(undefined)

const STORAGE_KEY = 'matrix-writer-selected-ideas'

export function MatrixProvider({ children }: { children: React.ReactNode }) {
  const [topics, setTopics] = useState<Topic[]>([])
  const [styles, setStyles] = useState<Style[]>([])
  const [ideas, setIdeas] = useState<Idea[]>([])
  const [selectedIdeas, setSelectedIdeas] = useState<SelectedIdea[]>(() => {
    // 從 localStorage 讀取已保存的點子
    const saved = localStorage.getItem(STORAGE_KEY)
    return saved ? JSON.parse(saved) : []
  })

  // 當 selectedIdeas 改變時，自動保存到 localStorage
  useEffect(() => {
    localStorage.setItem(STORAGE_KEY, JSON.stringify(selectedIdeas))
  }, [selectedIdeas])

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

  return (
    <MatrixContext.Provider value={{
      topics,
      styles,
      ideas,
      selectedIdeas,
      addTopic,
      addStyle,
      addIdea,
      getIdeasForCell,
      addToSelected,
      removeFromSelected,
      clearSelected
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