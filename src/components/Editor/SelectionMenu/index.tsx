import { Editor, BubbleMenu } from '@tiptap/react'
import { Bold, Italic, Code, Quote, Wand2 } from 'lucide-react'
import * as Dialog from '@radix-ui/react-dialog'
import { Button } from '@/components/ui/button'
import { Textarea } from '@/components/ui/textarea'
import { useState } from 'react'
import { textAssistantService } from '@/domains/matrix/services/textAssistantService'
import { useToast } from '@/components/ui/use-toast'

interface SelectionMenuProps {
  editor: Editor
}

export function SelectionMenu({ editor }: SelectionMenuProps) {
  const [open, setOpen] = useState(false)
  const [command, setCommand] = useState('')
  const [isLoading, setIsLoading] = useState(false)
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
    editor.commands.setTextSelection({
      from: editor.state.selection.from,
      to: editor.state.selection.from
    })
    setOpen(true)
  }

  const handleAIAssist = async () => {
    try {
      setIsLoading(true)
      const selectedText = editor.state.doc.textBetween(
        editor.state.selection.from,
        editor.state.selection.to
      )
      const fullText = editor.state.doc.textContent

      const response = await textAssistantService.getAssistance({
        command,
        selectedText,
        fullText
      })

      toast({
        title: "AI 建議",
        description: response,
      })
    } catch (error) {
      toast({
        title: "發生錯誤",
        description: "無法取得 AI 建議",
        variant: "destructive"
      })
    } finally {
      setIsLoading(false)
      setOpen(false)
      setCommand('')
    }
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
          <Dialog.Content className="fixed left-[50%] top-[50%] translate-x-[-50%] translate-y-[-50%] w-[90vw] max-w-[450px] bg-white rounded-lg p-6">
            <Dialog.Title className="text-lg font-medium mb-4">
              AI 寫作建議
            </Dialog.Title>
            
            <div className="space-y-4">
              <Textarea
                placeholder="輸入你想要的建議，例如：幫我改寫得更生動..."
                value={command}
                onChange={(e) => setCommand(e.target.value)}
                rows={4}
              />
              
              <div className="flex justify-end gap-3">
                <Button
                  variant="outline"
                  onClick={() => setOpen(false)}
                >
                  取消
                </Button>
                <Button
                  onClick={handleAIAssist}
                  disabled={isLoading || !command.trim()}
                >
                  {isLoading ? "處理中..." : "取得建議"}
                </Button>
              </div>
            </div>
          </Dialog.Content>
        </Dialog.Portal>
      </Dialog.Root>
    </>
  )
} 