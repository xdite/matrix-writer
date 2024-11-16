import React, { useState, useEffect } from 'react'
import { useParams, useNavigate } from 'react-router-dom'
import { useMatrix } from '@/domains/matrix/contexts/MatrixContext'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { ArrowLeft, Save } from 'lucide-react'
import { Card, CardContent, CardFooter } from '@/components/ui/card'
import { Editor } from '@/components/Editor'
import { useToast } from '@/components/ui/use-toast'
import { Writing } from '@/domains/matrix/types'
import { StyleGuideDrawer } from '@/components/StyleGuideDrawer'

export function WritingPage() {
  const { id } = useParams()
  const navigate = useNavigate()
  const { getWriting, updateWriting } = useMatrix()
  const { toast } = useToast()
  const [text, setText] = useState('')
  const [isDirty, setIsDirty] = useState(false)
  const [lastSavedAt, setLastSavedAt] = useState<Date | null>(null)
  const [writing, setWriting] = useState<Writing | undefined>()
  const [isLoading, setIsLoading] = useState(true)
  
  useEffect(() => {
    const loadWriting = async () => {
      if (!id) return
      setIsLoading(true)
      try {
        const loadedWriting = await getWriting(id)
        setWriting(loadedWriting)
        if (loadedWriting) {
          setText(loadedWriting.text)
          setLastSavedAt(new Date(loadedWriting.updatedAt))
        }
      } catch (error) {
        console.error('Error loading writing:', error)
        toast({
          title: "載入失敗",
          description: "無法載入文章內容",
          variant: "destructive"
        })
      } finally {
        setIsLoading(false)
      }
    }
    loadWriting()
  }, [id, getWriting])

  if (isLoading) {
    return (
      <div className="max-w-4xl mx-auto py-8">
        <div className="text-center">載入中...</div>
      </div>
    )
  }

  if (!writing) {
    return (
      <div className="max-w-4xl mx-auto">
        <div className="flex items-center gap-4 mb-8">
          <Button variant="ghost" onClick={() => navigate('/writings')}>
            <ArrowLeft className="h-4 w-4 mr-2" />
            返回文章列表
          </Button>
        </div>
        <div className="text-center py-8">
          找不到該文章
        </div>
      </div>
    )
  }

  const handleTextChange = (newText: string) => {
    setText(newText)
    setIsDirty(true)
  }

  const handleSave = async () => {
    try {
      await updateWriting(writing.id, text)
      setIsDirty(false)
      setLastSavedAt(new Date())
      toast({
        title: "已儲存",
        description: "文章已成功儲存",
      })
    } catch (error) {
      console.error('Error saving writing:', error)
      toast({
        title: "儲存失敗",
        description: "無法儲存文章內容",
        variant: "destructive"
      })
    }
  }

  return (
    <div className="max-w-4xl mx-auto">
      <div className="flex items-center gap-4 mb-8">
        <Button variant="ghost" onClick={() => navigate('/writings')}>
          <ArrowLeft className="h-4 w-4 mr-2" />
          返回文章列表
        </Button>
        <div className="flex-1" />
        <div className="flex gap-2">
          <Badge variant="secondary">{writing.topic}</Badge>
          <Badge variant="outline">{writing.style}</Badge>
        </div>
      </div>

      <Card className="mb-4">
        <CardContent className="pt-6">
          <div className="prose prose-sm">
            <div className="flex items-center justify-between mb-4">
              <div className="font-medium text-muted-foreground">
                寫作主題：
              </div>
              <StyleGuideDrawer style={writing.style} />
            </div>
            <p>{writing.content}</p>
          </div>
        </CardContent>
      </Card>

      <Card>
        <CardContent className="pt-6">
          <Editor value={text} onChange={handleTextChange} />
        </CardContent>
        <CardFooter className="flex items-center justify-between py-4 px-6 border-t bg-muted/50">
          <div className="text-sm text-muted-foreground">
            {lastSavedAt && (
              <span>上次儲存：{lastSavedAt.toLocaleTimeString()}</span>
            )}
          </div>
          <Button 
            onClick={handleSave}
            disabled={!isDirty}
            variant={isDirty ? "default" : "outline"}
          >
            <Save className="h-4 w-4 mr-2" />
            {isDirty ? "儲存" : "已儲存"}
          </Button>
        </CardFooter>
      </Card>
    </div>
  )
} 