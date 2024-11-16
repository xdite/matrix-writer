import Anthropic from '@anthropic-ai/sdk'
import { generatePrompt } from '@/domains/matrix/hooks/useClaudePrompt'
import { processIdeas } from '@/domains/matrix/hooks/useClaudeIdeas'

export async function generateIdeasWithClaude(topic: string, theme: string, style: string) {
  const apiKey = localStorage.getItem('claudeApiKey')
  const ideaCount = Number(localStorage.getItem('ideaCount') || '10')
  
  if (!apiKey) {
    console.log('API Key missing in localStorage')
    throw new Error('Claude API key not found')
  }
  
  console.log('Using API Key:', apiKey.substring(0, 8) + '...')
  console.log('Generating ideas for:', { topic, theme, style, ideaCount })

  const anthropic = new Anthropic({
    apiKey: apiKey,
    dangerouslyAllowBrowser: true
  })

  try {
    console.log('Sending request to Claude API...')
    const response = await anthropic.messages.create({
      model: 'claude-3-5-sonnet-20241022',
      max_tokens: 2000,
      temperature: 0.7,
      messages: [
        {
          role: 'user',
          content: generatePrompt(topic, theme, style, ideaCount)
        }
      ]
    })
    
    console.log('Received response:', response)

    const ideas = processIdeas(response, ideaCount)
    console.log('Processed ideas:', ideas)
    
    return ideas

  } catch (error: unknown) {
    console.error('Detailed error:', {
      name: error instanceof Error ? error.name : 'Unknown',
      message: error instanceof Error ? error.message : 'Unknown error',
      stack: error instanceof Error ? error.stack : undefined,
      response: error instanceof Error ? (error as any).response : undefined
    })
    throw error
  }
} 