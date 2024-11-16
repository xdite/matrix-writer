import React, { useState, useEffect } from 'react'
import { Lightbulb } from 'lucide-react'
import { Button } from '@/components/ui/button'
import {
  Sheet,
  SheetContent,
  SheetHeader,
  SheetTitle,
  SheetTrigger,
} from "@/components/ui/sheet"
import { ScrollArea } from '@/components/ui/scroll-area'
import ReactMarkdown from 'react-markdown'

interface StyleGuideDrawerProps {
  style: string
}

export function StyleGuideDrawer({ style }: StyleGuideDrawerProps) {
  const [guideContent, setGuideContent] = useState<string>('')
  const [isOpen, setIsOpen] = useState(false)

  useEffect(() => {
    const loadGuideContent = async () => {
      try {
        const response = await fetch(`/docs/styles/${style}.md`)
        const content = await response.text()
        setGuideContent(content)
      } catch (error) {
        console.error('Error loading style guide:', error)
        setGuideContent('無法載入寫作指南')
      }
    }

    if (isOpen) {
      loadGuideContent()
    }
  }, [style, isOpen])

  return (
    <div className="fixed right-4 top-1/3 z-50">
      <Sheet open={isOpen} onOpenChange={setIsOpen}>
        <SheetTrigger asChild>
          <Button 
            variant="outline" 
            size="icon"
            className="h-10 w-10 rounded-full shadow-lg bg-background hover:bg-accent"
            title="寫作指南"
          >
            <Lightbulb className="h-5 w-5" />
          </Button>
        </SheetTrigger>
        <SheetContent side="right" className="w-[400px] sm:w-[540px]">
          <SheetHeader>
            <SheetTitle>寫作指南：{style}</SheetTitle>
          </SheetHeader>
          <ScrollArea className="h-[calc(100vh-8rem)] mt-6">
            <div className="prose prose-sm max-w-none">
              <ReactMarkdown>{guideContent}</ReactMarkdown>
            </div>
          </ScrollArea>
        </SheetContent>
      </Sheet>
    </div>
  )
} 