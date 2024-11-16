import React from 'react'
import { useMatrix } from '@/domains/matrix/contexts/MatrixContext'
import { Card, CardContent } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Button } from '@/components/ui/button'
import { Pencil } from 'lucide-react'
import { useNavigate } from 'react-router-dom'

export function ListPage() {
  const { selectedIdeas } = useMatrix()
  const navigate = useNavigate()

  if (selectedIdeas.length === 0) {
    return (
      <div className="max-w-4xl mx-auto py-8">
        <div className="text-center py-12">
          <h2 className="text-2xl font-semibold mb-2">目前沒有寫作點子</h2>
          <p className="text-muted-foreground mb-4">
            前往點子產生器新增一些寫作點子吧！
          </p>
          <Button onClick={() => navigate('/')}>
            產生點子
          </Button>
        </div>
      </div>
    )
  }

  return (
    <div className="max-w-4xl mx-auto py-8">
      <h1 className="text-2xl font-bold mb-6">寫作清單</h1>
      <div className="space-y-4">
        {selectedIdeas.map((idea) => (
          <Card key={idea.id}>
            <CardContent className="p-6">
              <div className="flex items-start justify-between gap-4">
                <div className="flex-1">
                  <p className="text-sm mb-2">{idea.content}</p>
                  <div className="flex gap-2">
                    <Badge variant="secondary">{idea.topic}</Badge>
                    <Badge variant="outline">{idea.style}</Badge>
                  </div>
                </div>
                <Button
                  variant="ghost"
                  size="icon"
                  onClick={() => navigate(`/writing/${idea.id}`)}
                >
                  <Pencil className="h-4 w-4" />
                </Button>
              </div>
            </CardContent>
          </Card>
        ))}
      </div>
    </div>
  )
} 