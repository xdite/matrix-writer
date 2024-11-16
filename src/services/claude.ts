import Anthropic from '@anthropic-ai/sdk'

interface ClaudeConfig {
  model: string
  maxTokens: number
  temperature: number
  stream?: boolean
}

interface ClaudeOptions {
  config: ClaudeConfig
  onStream?: (content: string) => void
}

export async function generateIdeasWithClaude(prompt: string, options: ClaudeOptions) {
  const apiKey = localStorage.getItem('claudeApiKey')
  
  if (!apiKey) {
    console.log('API Key missing in localStorage')
    throw new Error('Claude API key not found')
  }
  
  console.log('Using API Key:', apiKey.substring(0, 8) + '...')

  const anthropic = new Anthropic({
    apiKey: apiKey,
    dangerouslyAllowBrowser: true
  })

  try {
    console.log('Sending request to Claude API...')
    
    if (options.config.stream) {
      const response = await anthropic.messages.create({
        model: options.config.model,
        max_tokens: options.config.maxTokens,
        temperature: options.config.temperature,
        messages: [
          {
            role: 'user',
            content: prompt
          }
        ],
        stream: true
      })

      let fullContent = ''
      for await (const chunk of response) {
        const content = chunk.content[0]?.text || ''
        fullContent += content
        options.onStream?.(content)
      }

      return {
        content: [{ text: fullContent }]
      }
    } else {
      const response = await anthropic.messages.create({
        model: options.config.model,
        max_tokens: options.config.maxTokens,
        temperature: options.config.temperature,
        messages: [
          {
            role: 'user',
            content: prompt
          }
        ],
        stream: false
      })
      
      console.log('Received response:', response)
      return response
    }

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