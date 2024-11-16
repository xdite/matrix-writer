import { generateIdeasWithClaude } from '@/services/claude'

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
    maxTokens: 1000,
    temperature: 0.7,
    stream: false,
    systemPrompt: `你是一位專業的寫作助手，擅長：
1. 根據用戶的需求提供具體的寫作建議
2. 分析文章的優缺點
3. 提供改進建議
4. 靈活運用不同的寫作技巧
5. 保持原文的核心意思，同時提升表達效果`
  }

  private generatePrompt(command: string, selectedText: string, fullText: string) {
    return `
請根據以下資訊給出建議：

用戶指令：${command}

選中的文字：
${selectedText}

完整文章：
${fullText}

請給出具體的修改建議，包括：
1. 分析現有內容的優缺點
2. 提供具體的改進建議
3. 如果適合的話，提供 2-3 個可能的改寫版本

請使用繁體中文回答。
`
  }

  async getAssistance({ command, selectedText, fullText }: TextAssistRequest) {
    const prompt = this.generatePrompt(command, selectedText, fullText)
    
    try {
      const response = await generateIdeasWithClaude(prompt, {
        config: this.config
      })
      
      return response
    } catch (error) {
      console.error('Error getting AI assistance:', error)
      throw error
    }
  }
}

export const textAssistantService = new TextAssistantService() 