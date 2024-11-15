import React, { useState } from 'react'
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Tabs, TabsList, TabsTrigger, TabsContent } from '@/components/ui/tabs'
import { Input } from '@/components/ui/input'
import { useMatrix } from '../../contexts/MatrixContext'
import { generateIdeasWithClaude } from '@/services/claude'
import { useToast } from '@/components/ui/use-toast'

const THEMES = {
  general: ['生活習慣', '生產力', '個人成長', '理財知識', '時間管理'],
  niche: ['技術寫作', '產品管理', '團隊領導', '跨部門溝通', '職涯發展'],
  industry: ['SaaS產業', '軟體開發', '人工智能', '數位轉型', '產業趨勢']
}

const STYLES = [
  'Actionable Guide Form (行動指引)',
  'Opinion Form (意見文)',
  'Curated List Form (策展文)',
  'Story Form (故事文)',
  'Credible Talking (人物訪談)',
  'Motivational (勵志文)',
  'Analytical (數據文)',
  'Contrarian (對比文)',
  'X vs Y (比較文)',
  'Listicle (清單文)'
]

export function ContentMatrixGenerator() {
  const { addTopic, addStyle, addIdea } = useMatrix()
  const [selectedTheme, setSelectedTheme] = useState('')
  const [selectedStyle, setSelectedStyle] = useState('')
  const [generatedIdeas, setGeneratedIdeas] = useState<string[]>([])
  const [customTopic, setCustomTopic] = useState('')
  const [isGenerating, setIsGenerating] = useState(false)
  const { toast } = useToast()

  const generateIdeas = async () => {
    if (!selectedTheme || !selectedStyle || !customTopic) return

    setIsGenerating(true)

    try {
      const topic = {
        name: customTopic,
        category: 'general' as const,
        description: `Generated topic for ${customTopic} under ${selectedTheme}`
      }
      addTopic(topic)

      const style = {
        name: selectedStyle,
        description: `Generated style for ${selectedStyle}`
      }
      addStyle(style)

      const ideas = await generateIdeasWithClaude(customTopic, selectedTheme, selectedStyle)
      setGeneratedIdeas(ideas)

      ideas.forEach(content => {
        addIdea({
          topicId: topic.id,
          styleId: style.id,
          content
        })
      })
    } catch (error) {
      console.error('Error:', error)
      toast({
        title: "Error generating ideas",
        description: "Please check your Claude API key in settings",
        variant: "destructive"
      })
    } finally {
      setIsGenerating(false)
    }
  }

  return (
    <Card>

      <CardContent>
        <div className="mb-6 mt-6">
          <label htmlFor="customTopic" className="block text-lg font-semibold mb-2">
            1. 輸入想寫的主題
          </label>
          <Input
            id="customTopic"
            value={customTopic}
            onChange={(e) => setCustomTopic(e.target.value)}
            placeholder=""
            className="mb-4"
          />
        </div>

        <div className="mb-6">
          <h3 className="text-lg font-semibold mb-4">2. 選擇主題分類與寫作風格</h3>
          <div className="grid grid-cols-12 gap-6 relative">
            {/* 中間的分隔線 */}
            <div className="absolute top-0 bottom-0 left-1/3 w-[1px] bg-gray-200 shadow-[1px_0_0_0_rgba(0,0,0,0.1)]" />

            {/* 左側：主題分類標籤頁 (4/12) */}
            <div className="col-span-4">
              <Tabs defaultValue="general" className="flex">
                {/* 左側標籤列表 */}
                <TabsList className="flex flex-col h-auto space-y-2 w-1/3">
                  <TabsTrigger value="general" className="w-full justify-start">大眾主題</TabsTrigger>
                  <TabsTrigger value="niche" className="w-full justify-start">利基主題</TabsTrigger>
                  <TabsTrigger value="industry" className="w-full justify-start">產業主題</TabsTrigger>
                </TabsList>

                {/* 右側內容區域 */}
                <div className="flex-1 pl-4">
                  {Object.entries(THEMES).map(([key, themeList]) => (
                    <TabsContent key={key} value={key}>
                      <div className="grid grid-cols-1 gap-2">
                        {themeList.map((theme) => (
                          <Button
                            key={theme}
                            variant={selectedTheme === theme ? "default" : "outline"}
                            onClick={() => setSelectedTheme(theme)}
                            className="w-full justify-start"
                          >
                            {theme}
                          </Button>
                        ))}
                      </div>
                    </TabsContent>
                  ))}
                </div>
              </Tabs>
            </div>

            {/* 右側：寫作風格 (8/12) */}
            <div className="col-span-8 pl-8">
              <h4 className="text-sm font-medium mb-3">寫作風格</h4>
              <div className="grid grid-cols-2 gap-2">
                {STYLES.map((style) => (
                  <Button
                    key={style}
                    variant={selectedStyle === style ? "default" : "outline"}
                    onClick={() => setSelectedStyle(style)}
                    className="w-full justify-start"
                  >
                    {style}
                  </Button>
                ))}
              </div>
            </div>
          </div>
        </div>

        <div className="mt-6">
          <Button 
            onClick={generateIdeas}
            className="w-full"
            disabled={!customTopic || !selectedTheme || !selectedStyle || isGenerating}
          >
            {isGenerating ? '生成點子中...' : '生成寫作點子'}
          </Button>
        </div>

        {generatedIdeas.length > 0 && (
          <div className="mt-6">
            <h3 className="text-lg font-semibold mb-4">寫作點子清單</h3>
            <div className="space-y-2">
              {generatedIdeas.map((idea, index) => (
                <div key={index} className="p-3 bg-gray-100 rounded-lg">
                  {index + 1}. {idea}
                </div>
              ))}
            </div>
          </div>
        )}
      </CardContent>
    </Card>
  )
} 