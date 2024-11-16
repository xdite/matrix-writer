import { generateIdeasWithClaude } from '@/services/claude'
import { marked } from 'marked'

interface ClaudeConfig {
  model: string
  maxTokens: number
  temperature: number
  stream: boolean
  systemPrompt: string
}

export class WritingService {
  private config: ClaudeConfig = {
    model: 'claude-3-5-sonnet-20241022',
    maxTokens: 4000,
    temperature: 0.7,
    stream: true,
    systemPrompt: `你是一位專業的內容寫作者，具有以下特點：
1. 擅長使用 Markdown 格式編寫文章
2. 善於結構化內容，確保文章層次分明
3. 能夠根據不同的寫作風格調整表達方式
4. 擅長使用具體案例和數據支持論點
5. 注重文章的可讀性和實用性
6. 能夠生成引人入勝的標題和小標題
7. 善於使用轉場句來連接段落
8. 會在適當位置加入總結和行動建議`
  }

  private generatePrompt(topic: string, style: string, content: string) {
    return `
請根據以下資訊，生成一篇完整的文章：

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
          fullContent += content
          const htmlContent = marked(fullContent, {
            breaks: true,
            gfm: true,
          })
          console.log('Converted HTML:', htmlContent)
          onProgress?.(htmlContent)
        }
      })

      const finalHtml = marked(fullContent, {
        breaks: true,
        gfm: true,
      })
      console.log('Final HTML content:', finalHtml)
      return finalHtml
    } catch (error) {
      console.error('Error generating article:', error)
      throw error
    }
  }
}

export const writingService = new WritingService() 