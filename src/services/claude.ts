import Anthropic from '@anthropic-ai/sdk'

export async function generateIdeasWithClaude(topic: string, theme: string, style: string) {
  const apiKey = localStorage.getItem('claudeApiKey')
  if (!apiKey) {
    console.log('API Key missing in localStorage')
    throw new Error('Claude API key not found')
  }
  
  console.log('Using API Key:', apiKey.substring(0, 8) + '...')
  console.log('Generating ideas for:', { topic, theme, style })

  const anthropic = new Anthropic({
    apiKey: apiKey,
    dangerouslyAllowBrowser: true
  })

  const prompt = `
你是一個內容創作專家。請針對以下主題和風格，生成 10 個具體的內容創作點子：

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

  try {
    console.log('Sending request to Claude API...')
    const response = await anthropic.messages.create({
      model: 'claude-3-sonnet-20240229',
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

    // 解析回應並轉換成字串陣列，同時清理格式
    const ideas = response.content[0].text
      .split('\n')
      .filter(line => line.trim().length > 0)
      .map(line => line.replace(/^\d+\.\s*/, '')) // 移除開頭的數字和點
      .slice(1, 11)

    console.log('Processed ideas:', ideas)
    return ideas
  } catch (error) {
    console.error('Detailed error:', {
      name: error.name,
      message: error.message,
      stack: error.stack,
      response: error.response
    })
    throw error
  }
} 