import { generateIdeasWithClaude } from '@/services/claude'
import { marked } from 'marked'

interface TextAssistRequest {
  command: string
  selectedText: string
  fullText: string
}

interface ClaudeConfig {
  model: string
  maxTokens: number
  temperature: number
  stream: boolean
  systemPrompt: string
}

export class TextAssistantService {
  private config: ClaudeConfig = {
    model: 'claude-3-5-sonnet-20241022',
    maxTokens: 8192,
    temperature: 0.7,
    stream: true,
    systemPrompt: `你是一位專業的寫作助手，擅長：

* 根據用戶的需求提供具體的寫作建議
* 提供改進建議
* 靈活運用不同的寫作技巧
* 保持原文的核心意思，同時提升表達效果`
  }

  private generatePrompt(command: string, selectedText: string, fullText: string) {
    return `
請根據以下資訊給出建議：


完整文章：
<fulltext>
${fullText}
</fulltext>
選中的文字：
<selected>
${selectedText}
</selected>

<instruction>
請給出具體的修改建議，包括：
* 提供具體的改進建議
* 如果適合的話，提供 2-3 個可能的改寫版本，每個版本至少 500 字。
* 需要考慮上下文，否則承接生硬

請使用繁體中文回答。使用 Markdown 格式。

用戶指令：${command}
</instruction>
`
  }

  async getAssistance({ command, selectedText, fullText }: TextAssistRequest, onProgress?: (content: string) => void) {
    const prompt = this.generatePrompt(command, selectedText, fullText)
    
    try {
      let fullContent = ''
      
      const response = await generateIdeasWithClaude(prompt, {
        config: this.config,
        onStream: (content) => {
          fullContent += content
          const renderedHtml = marked(fullContent, { breaks: true })
          onProgress?.(renderedHtml)
        }
      })
      
      return fullContent
    } catch (error) {
      console.error('Error getting AI assistance:', error)
      throw error
    }
  }
}

export const textAssistantService = new TextAssistantService() 