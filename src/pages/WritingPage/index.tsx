import React from 'react'
import { useParams, useNavigate } from 'react-router-dom'
import { useMatrix } from '@/domains/matrix/contexts/MatrixContext'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { ArrowLeft } from 'lucide-react'
import { Card, CardContent } from '@/components/ui/card'

export function WritingPage() {
  const { ideaId } = useParams()
  const navigate = useNavigate()
  const { selectedIdeas } = useMatrix()
  
  const idea = selectedIdeas.find(idea => idea.id === ideaId)
  
  if (!idea) {
    return (
      <div className="max-w-4xl mx-auto">
        <div className="flex items-center gap-4 mb-8">
          <Button variant="ghost" onClick={() => navigate('/')}>
            <ArrowLeft className="h-4 w-4 mr-2" />
            返回
          </Button>
        </div>
        <div className="text-center py-8">
          找不到該點子
        </div>
      </div>
    )
  }

  return (
    <div className="max-w-4xl mx-auto">
      <div className="flex items-center gap-4 mb-8">
        <Button variant="ghost" onClick={() => navigate('/')}>
          <ArrowLeft className="h-4 w-4 mr-2" />
          返回
        </Button>
        <div className="flex-1" />
        <div className="flex gap-2">
          <Badge variant="secondary">{idea.topic}</Badge>
          <Badge variant="outline">{idea.style}</Badge>
        </div>
      </div>

      <Card className="mb-4">
        <CardContent className="pt-6">
          <div className="prose prose-sm">
            <div className="font-medium text-muted-foreground mb-4">
              寫作主題：
            </div>
            <p>{idea.content}</p>
          </div>
        </CardContent>
      </Card>

      <Card>
        <CardContent className="pt-6">
          <textarea
            className="w-full min-h-[500px] p-4 rounded-md border resize-none focus:outline-none focus:ring-2 focus:ring-gray-400"
            placeholder="開始寫作..."
          />
        </CardContent>
      </Card>
    </div>
  )
} 