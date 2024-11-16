import { generatePrompt } from '../hooks/useClaudePrompt'
import { processIdeas } from '../hooks/useClaudeIdeas'
import { generateIdeasWithClaude } from '@/services/claude'

export class IdeaService {
  async generateIdeas(topic: string, theme: string, style: string, ideaCount: number) {
    const prompt = generatePrompt(topic, theme, style, ideaCount)
    const response = await generateIdeasWithClaude(prompt)
    return processIdeas(response, ideaCount)
  }
}

export const ideaService = new IdeaService() 