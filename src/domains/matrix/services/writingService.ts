import { generateIdeasWithClaude } from '@/services/claude'

interface ClaudeConfig {
  model: string
  maxTokens: number
  temperature: number
  stream: boolean
}

export class WritingService {
  private config: ClaudeConfig = {
    model: 'claude-3-5-sonnet-20241022',
    maxTokens: 4000,
    temperature: 0.7,
    stream: true
  }

  private generatePrompt(topic: string, style: string, content: string) {
    return `
你是一位專業的內容寫作者。請根據以下資訊，生成一篇完整的文章：

主題: ${topic}
寫作風格: ${style}
文章大綱: ${content}

要求：
1. 使用繁體中文
2. 符合該寫作風格的特點
3. 文章結構完整，包含開頭、正文和結尾
4. 段落分明，易於閱讀
5. 使用適當的標題和小標題
6. 確保內容流暢且有邏輯性
7. 加入實際案例或具體範例來支持論點
8. 結尾要有清楚的總結或呼籲行動

請直接開始寫作，不需要額外的說明。
`
  }

  async generateArticle(topic: string, style: string, content: string, onProgress?: (text: string) => void) {
    const prompt = this.generatePrompt(topic, style, content)
    console.log('Generating article with prompt:', prompt)
    
    try {
      let generatedText = ''
      const response = await generateIdeasWithClaude(prompt, {
        config: this.config,
        onStream: (content) => {
          console.log('Received chunk:', content)
          generatedText += content
          onProgress?.(content)
        }
      })

      console.log('Final generated text:', generatedText)
      return generatedText
    } catch (error) {
      console.error('Error generating article:', error)
      throw error
    }
  }
}

export const writingService = new WritingService() 