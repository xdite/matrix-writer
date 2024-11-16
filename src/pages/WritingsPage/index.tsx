import React from 'react'
import { useMatrix } from '@/domains/matrix/contexts/MatrixContext'
import { Card, CardContent } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Button } from '@/components/ui/button'
import { Pencil, Trash2 } from 'lucide-react'
import { useNavigate } from 'react-router-dom'
import { useToast } from '@/components/ui/use-toast'
import {
  AlertDialog,
  AlertDialogAction,
  AlertDialogCancel,
  AlertDialogContent,
  AlertDialogDescription,
  AlertDialogFooter,
  AlertDialogHeader,
  AlertDialogTitle,
  AlertDialogTrigger,
} from "@/components/ui/alert-dialog"

export function WritingsPage() {
  const { writings, deleteWriting } = useMatrix()
  const navigate = useNavigate()
  const { toast } = useToast()

  const handleDelete = (id: string) => {
    deleteWriting(id)
    toast({
      title: "已刪除文章",
      description: "文章已成功刪除",
    })
  }

  if (writings.length === 0) {
    return (
      <div className="max-w-4xl mx-auto py-8">
        <div className="text-center py-12">
          <h2 className="text-2xl font-semibold mb-2">目前沒有文章</h2>
          <p className="text-muted-foreground mb-4">
            從購物車中選擇一個點子開始寫作吧！
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
      <h1 className="text-2xl font-bold mb-6">我的文章</h1>
      <div className="space-y-4">
        {writings.map((writing) => (
          <Card key={writing.id}>
            <CardContent className="p-6">
              <div className="flex items-start justify-between gap-4">
                <div className="flex-1">
                  <p className="text-sm mb-2">{writing.content}</p>
                  <div className="flex gap-2">
                    <Badge variant="secondary">{writing.topic}</Badge>
                    <Badge variant="outline">{writing.style}</Badge>
                  </div>
                  <div className="mt-2 text-xs text-muted-foreground">
                    最後編輯��{new Date(writing.updatedAt).toLocaleString()}
                  </div>
                </div>
                <div className="flex gap-2">
                  <Button
                    variant="ghost"
                    size="icon"
                    onClick={() => navigate(`/writing/${writing.id}`)}
                  >
                    <Pencil className="h-4 w-4" />
                  </Button>
                  <AlertDialog>
                    <AlertDialogTrigger asChild>
                      <Button variant="ghost" size="icon">
                        <Trash2 className="h-4 w-4 text-destructive" />
                      </Button>
                    </AlertDialogTrigger>
                    <AlertDialogContent>
                      <AlertDialogHeader>
                        <AlertDialogTitle>確定要刪除這篇文章嗎？</AlertDialogTitle>
                        <AlertDialogDescription>
                          此操作無法復原。文章將會永久刪除。
                        </AlertDialogDescription>
                      </AlertDialogHeader>
                      <AlertDialogFooter>
                        <AlertDialogCancel>取消</AlertDialogCancel>
                        <AlertDialogAction
                          onClick={() => handleDelete(writing.id)}
                          className="bg-destructive text-destructive-foreground hover:bg-destructive/90"
                        >
                          刪除
                        </AlertDialogAction>
                      </AlertDialogFooter>
                    </AlertDialogContent>
                  </AlertDialog>
                </div>
              </div>
            </CardContent>
          </Card>
        ))}
      </div>
    </div>
  )
} 