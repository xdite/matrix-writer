import React, { createContext, useContext, useState } from 'react'
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

export function MatrixProvider({ children }: { children: React.ReactNode }) {
  const [topics, setTopics] = useState<Topic[]>([])
  const [styles, setStyles] = useState<Style[]>([])
  const [ideas, setIdeas] = useState<Idea[]>([])
  const [selectedIdeas, setSelectedIdeas] = useState<SelectedIdea[]>([])

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