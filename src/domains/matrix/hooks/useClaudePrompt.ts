export function generatePrompt(topic: string, theme: string, style: string, ideaCount: number) {
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