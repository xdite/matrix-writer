import { Editor, BubbleMenu } from '@tiptap/react'
import { Bold, Italic, Code, Quote, Wand2, X } from 'lucide-react'
import { Button } from '@/components/ui/button'
import { Textarea } from '@/components/ui/textarea'
import { useState, useRef, useEffect } from 'react'
import { textAssistantService } from '@/domains/matrix/services/textAssistantService'
import { useToast } from '@/components/ui/use-toast'
import { cn } from '@/lib/utils'

interface SelectionMenuProps {
  editor: Editor
}

export function SelectionMenu({ editor }: SelectionMenuProps) {
  const [isOpen, setIsOpen] = useState(false)
  const [command, setCommand] = useState('')
  const [selectedText, setSelectedText] = useState('')
  const [isLoading, setIsLoading] = useState(false)
  const [suggestion, setSuggestion] = useState('')
  const { toast } = useToast()
  
  const chatContentRef = useRef<HTMLDivElement>(null)

  if (!editor) {
    return null
  }

  const handleFormat = (callback: () => boolean) => {
    callback()
    editor.commands.setTextSelection({
      from: editor.state.selection.to,
      to: editor.state.selection.to
    })
  }

  const handleOpenAI = () => {
    const text = editor.state.doc.textBetween(
      editor.state.selection.from,
      editor.state.selection.to
    )
    setSelectedText(text)
    setIsOpen(true)
    
    // 取消選取
    editor.commands.setTextSelection({
      from: editor.state.selection.from,
      to: editor.state.selection.from
    })
  }

  const scrollToBottom = () => {
    if (chatContentRef.current) {
      chatContentRef.current.scrollTop = chatContentRef.current.scrollHeight
    }
  }

  useEffect(() => {
    if (suggestion) {
      setTimeout(scrollToBottom, 100)
    }
  }, [suggestion])

  const handleAIAssist = async () => {
    try {
      setIsLoading(true)
      setSuggestion('')
      const fullText = editor.state.doc.textContent

      await textAssistantService.getAssistance(
        {
          command,
          selectedText,
          fullText
        },
        (content) => {
          setSuggestion(content)
          setTimeout(scrollToBottom, 100)
        }
      )
    } catch (error) {
      toast({
        title: "發生錯誤",
        description: "無法取得 AI 建議",
        variant: "destructive"
      })
    } finally {
      setIsLoading(false)
    }
  }

  const handleApplySuggestion = () => {
    const tempDiv = document.createElement('div')
    tempDiv.innerHTML = suggestion
    const plainText = tempDiv.textContent || ''

    editor.commands.insertContent(plainText)
    
    handleClose()
  }

  const handleClose = () => {
    setIsOpen(false)
    setCommand('')
    setSuggestion('')
  }

  return (
    <>
      <BubbleMenu 
        editor={editor} 
        tippyOptions={{ 
          duration: 100,
          placement: 'bottom',
          offset: [0, 10],
        }}
        className="bg-white rounded-lg shadow-lg border p-1.5 min-w-[180px]"
      >
        <button
          type="button"
          className="flex items-center gap-2 w-full px-2 py-1.5 text-sm hover:bg-slate-100 rounded-sm"
          onClick={() => handleFormat(() => editor.chain().focus().toggleBold().run())}
        >
          <Bold className="w-4 h-4" />
          <span>粗體</span>
          <div className="ml-auto text-xs text-muted-foreground">⌘+B</div>
        </button>

        <button
          type="button"
          className="flex items-center gap-2 w-full px-2 py-1.5 text-sm hover:bg-slate-100 rounded-sm"
          onClick={() => handleFormat(() => editor.chain().focus().toggleItalic().run())}
        >
          <Italic className="w-4 h-4" />
          <span>斜體</span>
          <div className="ml-auto text-xs text-muted-foreground">⌘+I</div>
        </button>

        <button
          type="button"
          className="flex items-center gap-2 w-full px-2 py-1.5 text-sm hover:bg-slate-100 rounded-sm"
          onClick={() => handleFormat(() => editor.chain().focus().toggleCode().run())}
        >
          <Code className="w-4 h-4" />
          <span>程式碼</span>
          <div className="ml-auto text-xs text-muted-foreground">⌘+E</div>
        </button>

        <div className="h-px bg-border my-1" />

        <button
          type="button"
          className="flex items-center gap-2 w-full px-2 py-1.5 text-sm hover:bg-slate-100 rounded-sm"
          onClick={() => handleFormat(() => editor.chain().focus().toggleBlockquote().run())}
        >
          <Quote className="w-4 h-4" />
          <span>引用</span>
        </button>

        <div className="h-px bg-border my-1" />

        <button
          type="button"
          className="flex items-center gap-2 w-full px-2 py-1.5 text-sm hover:bg-slate-100 rounded-sm"
          onClick={handleOpenAI}
        >
          <Wand2 className="w-4 h-4" />
          <span>AI 建議</span>
        </button>
      </BubbleMenu>

      {/* Sidebar Chat */}
      <div
        className={cn(
          "fixed right-0 top-0 h-screen w-[500px] bg-white shadow-lg transform transition-transform duration-200 ease-in-out border-l",
          isOpen ? "translate-x-0" : "translate-x-full"
        )}
      >
        <div className="flex flex-col h-full">
          {/* Header */}
          <div className="flex items-center justify-between px-6 py-4 border-b">
            <h2 className="text-lg font-medium">AI 寫作建議</h2>
            <Button
              variant="ghost"
              size="icon"
              onClick={handleClose}
            >
              <X className="h-4 w-4" />
            </Button>
          </div>

          {/* Content */}
          <div 
            ref={chatContentRef}
            className="flex-1 overflow-auto p-6 space-y-4 scroll-smooth"
          >
            {selectedText && (
              <div className="p-4 bg-muted rounded-md">
                <div className="text-sm text-muted-foreground mb-2">選取的文字：</div>
                <div className="text-base">{selectedText}</div>
              </div>
            )}

            <div className="space-y-2">
              <Textarea
                placeholder="輸入你想要的建議，例如：幫我改寫得更生動..."
                value={command}
                onChange={(e) => setCommand(e.target.value)}
                rows={3}
                className="text-base resize-none"
              />
              <Button
                className="w-full"
                onClick={handleAIAssist}
                disabled={isLoading || !command.trim()}
              >
                {isLoading ? "處理中..." : "取得建議"}
              </Button>
            </div>

            {(isLoading || suggestion) && (
              <div className="mt-6 space-y-4">
                <div className="p-4 bg-muted rounded-md prose prose-sm max-w-none">
                  {isLoading && !suggestion && (
                    <div className="animate-pulse">AI 正在思考中...</div>
                  )}
                  {suggestion && (
                    <>
                      <div 
                        dangerouslySetInnerHTML={{ __html: suggestion }}
                      />
                      <div className="mt-4 pt-4 border-t">
                        <Button
                          onClick={handleApplySuggestion}
                          className="w-full"
                        >
                          套用建議
                        </Button>
                      </div>
                    </>
                  )}
                </div>
              </div>
            )}
          </div>
        </div>
      </div>
    </>
  )
} 