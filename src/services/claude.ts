import Anthropic from '@anthropic-ai/sdk'

export async function generateIdeasWithClaude(prompt: string) {
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
    const response = await anthropic.messages.create({
      model: 'claude-3-5-sonnet-20241022',
      max_tokens: 2000,
      temperature: 0.7,
      messages: [
        {
          role: 'user',
          content: prompt
        }
      ]
    })
    
    console.log('Received response:', response)
    return response

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