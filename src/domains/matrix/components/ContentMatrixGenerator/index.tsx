import React, { useState } from 'react'
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Tabs, TabsList, TabsTrigger, TabsContent } from '@/components/ui/tabs'
import { Input } from '@/components/ui/input'
import { useMatrix } from '../../contexts/MatrixContext'
import { generateIdeasWithClaude } from '@/services/claude'
import { useToast } from '@/components/ui/use-toast'
import { Badge } from '@/components/ui/badge'

const THEMES = {
  general: ['生活習慣', '生產力', '個人成長', '理財知識', '時間管理'],
  niche: ['技術寫作', '產品管理', '團隊領導', '跨部門溝通', '職涯發展'],
  industry: ['SaaS產業', '軟體開發', '人工智能', '數位轉型', '產業趨勢']
}

const STYLES = [
  {
    name: 'Behind the scene post (幕後文)',
    example: '日更之旅完成！第一輪「365 天每日寫作計畫」圓滿落幕'
  },
  {
    name: 'Challenge post (挑戰文)',
    example: '挑戰 365 天連續日更'
  },
  {
    name: 'Case Study post (案例分析文)',
    example: '當工具型平台愈來愈多時，對於電商產業的商家來說是一件好事情嗎？'
  },
  {
    name: 'Checklist post (檢查清單文)',
    example: '這 50 家台北必吃餐廳，你都吃過了嗎？'
  },
  {
    name: 'Chronicle Growth (編年史)',
    example: '日本 40 年的產業電玩歷介紹'
  },
  {
    name: 'Crowdsources post (大眾討論)',
    example: '卡片盒筆記法到底要如何使用？'
  },
  {
    name: 'FAQ post (問答文)',
    example: 'Obsidian 新手常見 10 大問題'
  },
  {
    name: 'Inspirational post (啟發文)',
    example: '來自 Sean McCabe 演講的 3 點啟發'
  },
  {
    name: 'Interview leaders (訪談文)',
    example: '訪談 XXX 對於 OO 產業的看法'
  },
  {
    name: 'List post (清單文)',
    example: '10 種超好用的生產力工具'
  },
  {
    name: 'News post (新聞評論文)',
    example: '加密貨幣交易所負面新聞頻傳，為什麼還是吸引大家爭相投入市場？'
  },
  {
    name: 'Research post (研究文)',
    example: '網紅經濟的興起與生態圈懶人包'
  },
  {
    name: 'Review products (評測產品)',
    example: '評測 Mac Launcher – Raycast 功能'
  },
  {
    name: 'Review services (評測服務)',
    example: '評測 Engoo 線上 1 對 1 英文口說家教服務'
  },
  {
    name: 'Series post (系列文)',
    example: '最完整的 Obsidian 使用教學系列文'
  },
  {
    name: 'Share experiences (分享經驗)',
    example: '分享我面試 10 家軟體產品經理職缺的經驗'
  },
  {
    name: 'Showcase post (展示成果文)',
    example: '我如何在 Notion 中建立人脈資料庫？'
  },
  {
    name: 'Story post (故事文)',
    example: '我如何克服轉換工作的心理疙瘩？'
  },
  {
    name: 'Teach post (教學文)',
    example: '如何使用 Obsidian？'
  }
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
                    key={style.name}
                    variant={selectedStyle === style.name ? "default" : "outline"}
                    onClick={() => setSelectedStyle(style.name)}
                    className={`w-full text-left p-3 h-auto flex flex-col items-start ${
                      selectedStyle === style.name 
                        ? 'bg-primary text-primary-foreground hover:bg-primary/90'
                        : 'hover:bg-accent hover:text-accent-foreground'
                    }`}
                  >
                    <div className="flex flex-col items-start gap-0.5 w-full">
                      <span className="font-medium text-sm">{style.name}</span>
                      <span className={`text-xs ${
                        selectedStyle === style.name 
                          ? 'text-primary-foreground/80'
                          : 'text-muted-foreground'
                      }`}>
                        例：{style.example}
                      </span>
                    </div>
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
            <div className="mb-6 p-4 bg-muted rounded-lg">
              <h3 className="text-lg font-semibold mb-2">寫作點子清單</h3>
              <div className="space-y-1">
                <p className="text-sm">
                  <span className="font-medium">當前選擇主題： </span>
                  <span className="text-muted-foreground">{customTopic}</span>

                  <span className="font-medium"> 分類：</span>
                  <span className="text-muted-foreground">{selectedTheme}</span>

                  <span className="font-medium"> 寫作風格：</span>
                  <span className="text-muted-foreground">
                    {STYLES.find(s => s.name === selectedStyle)?.name}
                  </span>
                </p>
              </div>
            </div>

            <div className="space-y-3">
              {generatedIdeas.map((idea, index) => (
                <div key={index} className="p-4 bg-gray-100 rounded-lg">
           
                  <div className="text-sm">
                    {index + 1}. {idea}

                    <Badge variant="secondary ">{customTopic}</Badge>
                    <Badge variant="outline ">
                      {STYLES.find(s => s.name === selectedStyle)?.name.split(' ')[0]}
                    </Badge>
                  </div>
                </div>
              ))}
            </div>
          </div>
        )}
      </CardContent>
    </Card>
  )
} 