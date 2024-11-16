import { generateIdeasWithClaude } from '@/services/claude'
import { marked } from 'marked'

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
4. 段落分明，每段之間要空一行
5. 使用適當的標題和小標題，標題前後要空行
6. 確保內容流暢且有邏輯性
7. 加入實際案例或具體範例來支持論點
8. 結尾要有清楚的總結或呼籲行動
9. 使用 Markdown 格式
10. 每個段落之間要有空行

請直接開始寫作，不需要額外的說明。使用 Markdown 格式。
`
  }

  async generateArticle(topic: string, style: string, content: string, onProgress?: (text: string) => void) {
    const prompt = this.generatePrompt(topic, style, content)
    console.log('Generating article with prompt:', prompt)
    
    try {
      let fullContent = ''
      
      const response = await generateIdeasWithClaude(prompt, {
        config: this.config,
        onStream: (content) => {
          console.log('Received chunk:', content)
          fullContent += content
          
          // 每次都重新渲染完整的 markdown
          const renderedHtml = marked(fullContent, { breaks: true })
          onProgress?.(renderedHtml)
        }
      })

      // 最後返回原始的 markdown 文本
      console.log('Final generated text:', fullContent)
      return fullContent
    } catch (error) {
      console.error('Error generating article:', error)
      throw error
    }
  }
}

export const writingService = new WritingService() 