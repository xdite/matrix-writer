import React, { useState, useEffect } from 'react'
import { useParams, useNavigate } from 'react-router-dom'
import { useMatrix } from '@/domains/matrix/contexts/MatrixContext'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { ArrowLeft } from 'lucide-react'
import { Card, CardContent } from '@/components/ui/card'
import { Editor } from '@/components/Editor'

export function WritingPage() {
  const { id } = useParams()
  const navigate = useNavigate()
  const { getWriting, updateWriting } = useMatrix()
  const [text, setText] = useState('')
  
  const writing = getWriting(id!)
  
  useEffect(() => {
    if (writing) {
      setText(writing.text)
    }
  }, [writing])

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
    updateWriting(writing.id, newText)
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
            <div className="font-medium text-muted-foreground mb-4">
              寫作主題：
            </div>
            <p>{writing.content}</p>
          </div>
        </CardContent>
      </Card>

      <Card>
        <CardContent className="pt-6">
          <Editor value={text} onChange={handleTextChange} />
        </CardContent>
      </Card>
    </div>
  )
} 