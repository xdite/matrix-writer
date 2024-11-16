import Anthropic from '@anthropic-ai/sdk'

interface ClaudeConfig {
  model: string
  maxTokens: number
  temperature: number
  stream?: boolean
  systemPrompt?: string
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
        system: options.config.systemPrompt,
        messages: [{
          role: 'user',
          content: prompt
        }],
        stream: true
      })

      let fullContent = ''
      for await (const chunk of response) {
        if (chunk.type === 'content_block_delta') {
          const content = chunk.delta?.text || ''
          fullContent += content
          console.log(content)
          options.onStream?.(content)

        }

      }

      return {
        content: [{ text: fullContent }]
      }
    } else {
      const response = await anthropic.messages.create({
        model: options.config.model,
        max_tokens: options.config.maxTokens,
        temperature: options.config.temperature,
        system: options.config.systemPrompt,
        messages: [{
          role: 'user',
          content: prompt
        }],
        stream: false
      })

      console.log('Received response:', response)
      return {
        content: [{ text: response.content[0].text }]
      }
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
