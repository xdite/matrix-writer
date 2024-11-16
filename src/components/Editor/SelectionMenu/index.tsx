import { Editor, BubbleMenu } from '@tiptap/react'
import { Bold, Italic, Code, Quote, Wand2 } from 'lucide-react'
import * as Dialog from '@radix-ui/react-dialog'
import { Button } from '@/components/ui/button'
import { Textarea } from '@/components/ui/textarea'
import { useState } from 'react'
import { textAssistantService } from '@/domains/matrix/services/textAssistantService'
import { useToast } from '@/components/ui/use-toast'
import { marked } from 'marked'

interface SelectionMenuProps {
  editor: Editor
}

export function SelectionMenu({ editor }: SelectionMenuProps) {
  const [open, setOpen] = useState(false)
  const [command, setCommand] = useState('')
  const [selectedText, setSelectedText] = useState('')
  const [isLoading, setIsLoading] = useState(false)
  const [suggestion, setSuggestion] = useState('')
  const { toast } = useToast()

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

  const handleOpenAIDialog = () => {
    const text = editor.state.doc.textBetween(
      editor.state.selection.from,
      editor.state.selection.to
    )
    setSelectedText(text)
    
    editor.commands.setTextSelection({
      from: editor.state.selection.from,
      to: editor.state.selection.from
    })
    setOpen(true)
  }

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
    
    setOpen(false)
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
          onClick={handleOpenAIDialog}
        >
          <Wand2 className="w-4 h-4" />
          <span>AI 建議</span>
        </button>
      </BubbleMenu>

      <Dialog.Root open={open} onOpenChange={setOpen}>
        <Dialog.Portal>
          <Dialog.Overlay className="fixed inset-0 bg-black/50" />
          <Dialog.Content className="fixed left-[50%] top-[50%] translate-x-[-50%] translate-y-[-50%] w-[90vw] max-w-[600px] max-h-[85vh] bg-white rounded-lg p-6 overflow-hidden flex flex-col">
            <Dialog.Title className="text-lg font-medium mb-4">
              AI 寫作建議
            </Dialog.Title>
            
            <div className="space-y-4 flex-1 overflow-hidden">
              {selectedText && (
                <div className="p-3 bg-muted rounded-md">
                  <div className="text-sm text-muted-foreground mb-2">選取的文字：</div>
                  <div className="text-sm">{selectedText}</div>
                </div>
              )}
              
              <Textarea
                placeholder="輸入你想要的建議，例如：幫我改寫得更生動..."
                value={command}
                onChange={(e) => setCommand(e.target.value)}
                rows={3}
              />

              {(isLoading || suggestion) && (
                <div className="flex-1 overflow-auto">
                  <div className="p-4 bg-muted rounded-md prose prose-sm max-w-none">
                    {isLoading && !suggestion && (
                      <div className="animate-pulse">AI 正在思考中...</div>
                    )}
                    {suggestion && (
                      <div 
                        dangerouslySetInnerHTML={{ __html: suggestion }}
                        className="overflow-auto max-h-[300px]"
                      />
                    )}
                  </div>
                </div>
              )}
              
              <div className="flex justify-end gap-3 pt-4 border-t">
                <Button
                  variant="outline"
                  onClick={() => {
                    setOpen(false)
                    setSuggestion('')
                    setCommand('')
                  }}
                >
                  取消
                </Button>
                {!suggestion && (
                  <Button
                    onClick={handleAIAssist}
                    disabled={isLoading || !command.trim()}
                  >
                    {isLoading ? "處理中..." : "取得建議"}
                  </Button>
                )}
                {suggestion && (
                  <Button
                    onClick={handleApplySuggestion}
                  >
                    套用建議
                  </Button>
                )}
              </div>
            </div>
          </Dialog.Content>
        </Dialog.Portal>
      </Dialog.Root>
    </>
  )
} 