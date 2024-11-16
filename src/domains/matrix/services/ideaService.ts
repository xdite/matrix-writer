import { generateIdeasWithClaude } from '@/services/claude'

interface ClaudeResponse {
  content: Array<{
    text: string
  }>
}

export class IdeaService {
  private generatePrompt(topic: string, theme: string, style: string, ideaCount: number) {
    return `
你是一個內容創作專家。請針對以下主題和風格，生成 ${ideaCount} 個具體的內容創作點子：

主題: ${topic}
分類: ${theme}
寫作風格: ${style}

每個點子應該要：
1. 具體且可執行
2. 符合該寫作風格的特點
3. 能吸引目標讀者
4. 有獨特的切入角度

請直接列出點子，每個點子一行，不要加序號。
`
  }

  private processIdeas(response: ClaudeResponse, ideaCount: number) {
    if (!response?.content?.[0]?.text) {
      throw new Error('Invalid response format')
    }

    return response.content[0].text
      .split('\n')
      .filter((line: string) => line.trim().length > 0)
      .map((line: string) => line.replace(/^\d+\.\s*/, ''))
      .slice(1, ideaCount + 1)
  }

  async generateIdeas(topic: string, theme: string, style: string, ideaCount: number) {
    const prompt = this.generatePrompt(topic, theme, style, ideaCount)
    const response = await generateIdeasWithClaude(prompt)
    return this.processIdeas(response, ideaCount)
  }
}

export const ideaService = new IdeaService() 