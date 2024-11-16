import React from 'react'
import { Button } from '@/components/ui/button'
import { ShoppingCart } from 'lucide-react'
import { useMatrix } from '@/domains/matrix/contexts/MatrixContext'
import {
  Sheet,
  SheetContent,
  SheetDescription,
  SheetHeader,
  SheetTitle,
  SheetTrigger,
} from "@/components/ui/sheet"
import { Badge } from '@/components/ui/badge'
import { ScrollArea } from '@/components/ui/scroll-area'
import { X } from 'lucide-react'
import { useToast } from '@/components/ui/use-toast'

export function CartButton() {
  const { selectedIdeas, removeFromSelected } = useMatrix()
  const { toast } = useToast()

  const handleRemove = (id: string) => {
    removeFromSelected(id)
    toast({
      title: "已從清單中移除",
      description: "點子已從寫作清單中移除",
    })
  }

  return (
    <Sheet>
      <SheetTrigger asChild>
        <Button variant="outline" size="icon" className="relative">
          <ShoppingCart className="h-5 w-5" />
          {selectedIdeas.length > 0 && (
            <Badge 
              variant="destructive" 
              className="absolute -top-2 -right-2 h-5 w-5 flex items-center justify-center p-0 text-xs"
            >
              {selectedIdeas.length}
            </Badge>
          )}
        </Button>
      </SheetTrigger>
      <SheetContent className="w-[400px] sm:w-[540px]">
        <SheetHeader>
          <SheetTitle>已選擇的寫作點子</SheetTitle>
          <SheetDescription>
            共 {selectedIdeas.length} 個點子
          </SheetDescription>
        </SheetHeader>
        <ScrollArea className="h-[calc(100vh-8rem)] mt-4">
          <div className="space-y-4">
            {selectedIdeas.map((idea) => (
              <div 
                key={idea.id} 
                className="relative bg-muted p-4 rounded-lg"
              >
                <Button
                  variant="ghost"
                  size="icon"
                  className="absolute top-2 right-2"
                  onClick={() => handleRemove(idea.id)}
                >
                  <X className="h-4 w-4" />
                </Button>
                <div className="pr-8">
                  <p className="text-sm">{idea.content}</p>
                  <div className="mt-2 flex gap-2">
                    <Badge variant="secondary">{idea.topic}</Badge>
                    <Badge variant="outline">{idea.style}</Badge>
                  </div>
                </div>
              </div>
            ))}
          </div>
        </ScrollArea>
      </SheetContent>
    </Sheet>
  )
} 