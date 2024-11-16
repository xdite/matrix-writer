export interface Topic {
  id: string
  name: string
  category: 'general' | 'niche' | 'industry'
  description?: string
}

export interface Style {
  id: string
  name: string
  description?: string
}

export interface Idea {
  id: string
  topicId: string
  styleId: string
  content: string
  createdAt: Date
}

export interface MatrixCell {
  topicId: string
  styleId: string
  ideas: Idea[]
}

export interface SelectedIdea {
  id: string
  content: string
  topic: string
  style: string
  addedAt: Date
}

export interface Writing {
  id: string
  content: string
  topic: string
  style: string
  text: string
  createdAt: Date
  updatedAt: Date
} 