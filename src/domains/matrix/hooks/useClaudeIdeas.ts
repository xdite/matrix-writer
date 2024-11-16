import { useCallback } from 'react'

interface ClaudeResponse {
  content: Array<{
    text: string
  }>
}

export function processIdeas(response: ClaudeResponse, ideaCount: number) {
  if (!response?.content?.[0]?.text) {
    throw new Error('Invalid response format')
  }

  return response.content[0].text
    .split('\n')
    .filter((line: string) => line.trim().length > 0)
    .map((line: string) => line.replace(/^\d+\.\s*/, ''))
    .slice(1, ideaCount + 1)
}

export function useClaudeIdeas() {
  const processIdeas = useCallback((response: any, ideaCount: number) => {
    if (!response?.content?.[0]?.text) {
      throw new Error('Invalid response format')
    }

    return response.content[0].text
      .split('\n')
      .filter((line: string) => line.trim().length > 0)
      .map((line: string) => line.replace(/^\d+\.\s*/, ''))
      .slice(1, ideaCount + 1)
  }, [])

  return {
    processIdeas
  }
} 