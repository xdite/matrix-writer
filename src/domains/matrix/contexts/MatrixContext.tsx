import React, { createContext, useContext, useState } from 'react'
import { Topic, Style, Idea, MatrixCell } from '../types'

interface MatrixContextType {
  topics: Topic[]
  styles: Style[]
  ideas: Idea[]
  addTopic: (topic: Omit<Topic, 'id'>) => void
  addStyle: (style: Omit<Style, 'id'>) => void
  addIdea: (idea: Omit<Idea, 'id' | 'createdAt'>) => void
  getIdeasForCell: (topicId: string, styleId: string) => Idea[]
}

const MatrixContext = createContext<MatrixContextType | undefined>(undefined)

export function MatrixProvider({ children }: { children: React.ReactNode }) {
  const [topics, setTopics] = useState<Topic[]>([])
  const [styles, setStyles] = useState<Style[]>([])
  const [ideas, setIdeas] = useState<Idea[]>([])

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

  return (
    <MatrixContext.Provider value={{
      topics,
      styles,
      ideas,
      addTopic,
      addStyle,
      addIdea,
      getIdeasForCell
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