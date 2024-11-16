import { Editor, BubbleMenu } from '@tiptap/react'
import { Bold, Italic, Code, Quote, Wand2, X, Copy, Check, ArrowLeftToLine } from 'lucide-react'
import { Button } from '@/components/ui/button'
import { Textarea } from '@/components/ui/textarea'
import { useState, useRef, useEffect } from 'react'
import { textAssistantService } from '@/domains/matrix/services/textAssistantService'
import { useToast } from '@/components/ui/use-toast'
import { cn } from '@/lib/utils'
import { marked } from 'marked'
import commands from '@/domains/matrix/constants/commands.json'

interface SelectionMenuProps {
  editor: Editor
}

export function SelectionMenu({ editor }: SelectionMenuProps) {
  const [isOpen, setIsOpen] = useState(() => {
    return localStorage.getItem('aiSidebarOpen') === 'true'
  })
  
  useEffect(() => {
    localStorage.setItem('aiSidebarOpen', isOpen.toString())
  }, [isOpen])

  const [keepOpen, setKeepOpen] = useState(false)
  const [command, setCommand] = useState('')
  const [selectedText, setSelectedText] = useState('')
  const [isLoading, setIsLoading] = useState(false)
  const [suggestion, setSuggestion] = useState('')
  const { toast } = useToast()
  const [copiedIndex, setCopiedIndex] = useState<number | null>(null)
  const [selectionRange, setSelectionRange] = useState<{from: number, to: number} | null>(null)
  
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
    const from = editor.state.selection.from
    const to = editor.state.selection.to
    
    const text = editor.state.doc.textBetween(from, to)
    setSelectedText(text)
    setSelectionRange({ from, to })
    setCommand('')
    setSuggestion('')
    setIsOpen(true)
    localStorage.setItem('aiSidebarOpen', 'true')
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
  }

  const handleForceClose = () => {
    setIsOpen(false)
    setCommand('')
    setSuggestion('')
    localStorage.setItem('aiSidebarOpen', 'false')
  }

  const handleCopy = (text: string, index: number) => {
    navigator.clipboard.writeText(text)
    setCopiedIndex(index)
    setTimeout(() => {
      setCopiedIndex(null)
    }, 2000)
  }

  const handleInsertCode = (text: string, index: number) => {
    if (selectionRange) {
      // 移動到原始選取的結束位置
      editor.commands.setTextSelection(selectionRange.to)
      
      // 將文字轉換為 HTML
      const htmlContent = marked(text, {
        breaks: true,
        gfm: true
      })
      
      // 在插入前後都加入換行，並插入轉換後的 HTML
      editor.commands.insertContent('\n\n' + htmlContent + '\n\n')
    }
    
    toast({
      title: "已插入程式碼",
      description: "程式碼已插入到選取位置之後",
    })
  }

  const renderWithCopyButton = (html: string) => {
    // 先清除所有現有的事件監聽器
    const existingButtons = document.querySelectorAll('[data-action]')
    existingButtons.forEach(button => {
      button.replaceWith(button.cloneNode(true))
    })

    const parser = new DOMParser()
    const doc = parser.parseFromString(html, 'text/html')
    const preElements = doc.querySelectorAll('pre')
    
    preElements.forEach((pre, index) => {
      const wrapper = document.createElement('div')
      wrapper.className = 'relative group'
      
      const buttonGroup = document.createElement('div')
      buttonGroup.className = 'absolute right-2 top-2 flex gap-2'
      
      // 修改插入按鈕，使用 << 符號
      const insertButton = document.createElement('button')
      insertButton.className = 'p-2 rounded-md bg-white hover:bg-gray-100 transition-all font-mono font-medium'
      insertButton.setAttribute('data-index', index.toString())
      insertButton.setAttribute('data-action', 'insert')
      insertButton.innerHTML = '<<'

      // 複製按鈕
      const copyButton = document.createElement('button')
      copyButton.className = 'p-2 rounded-md bg-white hover:bg-gray-100 transition-all'
      copyButton.setAttribute('data-index', index.toString())
      copyButton.setAttribute('data-action', 'copy')
      copyButton.innerHTML = `
        <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
          ${copiedIndex === index 
            ? '<path d="M20 6 9 17l-5-5"/>' 
            : '<path d="M8 17.929H6c-1.105 0-2-.912-2-2.036V5.036C4 3.912 4.895 3 6 3h8c1.105 0 2 .912 2 2.036v1.866m-6 .17h8c1.105 0 2 .91 2 2.035v10.857C20 21.088 19.105 22 18 22h-8c-1.105 0-2-.911-2-2.036V9.107c0-1.124.895-2.036 2-2.036z"/>'
          }
        </svg>
      `
      
      buttonGroup.appendChild(insertButton)
      buttonGroup.appendChild(copyButton)
      
      pre.parentNode?.insertBefore(wrapper, pre)
      wrapper.appendChild(pre)
      wrapper.appendChild(buttonGroup)
    })

    const result = doc.body.innerHTML

    // 使用 requestAnimationFrame 而不是 setTimeout
    requestAnimationFrame(() => {
      const buttons = document.querySelectorAll('[data-action]')
      buttons.forEach(button => {
        const newButton = button.cloneNode(true)
        button.parentNode?.replaceChild(newButton, button)
        
        newButton.addEventListener('click', (e) => {
          e.preventDefault()
          e.stopPropagation()
          const index = parseInt(newButton.getAttribute('data-index') || '0')
          const action = newButton.getAttribute('data-action')
          const pre = newButton.closest('.relative')?.querySelector('pre')
          if (pre) {
            if (action === 'copy') {
              handleCopy(pre.textContent || '', index)
            } else if (action === 'insert') {
              handleInsertCode(pre.textContent || '', index)
            }
          }
        })
      })
    })

    return result
  }

  // Add keyboard shortcut handler
  useEffect(() => {
    const handleKeyDown = (e: KeyboardEvent) => {
      if (!isOpen || !selectedText) return
      
      if (e.altKey && e.key >= '1' && e.key <= '3') {
        e.preventDefault()
        const index = parseInt(e.key) - 1
        const preset = commands.presetCommands[index]
        if (preset) {
          setCommand(preset.command)
        }
      }
    }

    window.addEventListener('keydown', handleKeyDown)
    return () => window.removeEventListener('keydown', handleKeyDown)
  }, [isOpen, selectedText])

  // Add effect to update selected text when selection changes
  useEffect(() => {
    const handleSelectionUpdate = () => {
      if (editor && isOpen) {
        const { from, to } = editor.state.selection
        const text = editor.state.doc.textBetween(from, to)
        if (text) {
          setSelectedText(text)
          setSelectionRange({ from, to })
        }
      }
    }

    // Listen to selection changes
    editor.on('selectionUpdate', handleSelectionUpdate)
    
    return () => {
      editor.off('selectionUpdate', handleSelectionUpdate)
    }
  }, [editor, isOpen])

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
      <div className={cn(
        "fixed right-0 top-0 h-screen w-[500px] bg-white shadow-lg transform transition-transform duration-200 ease-in-out border-l",
        isOpen ? "translate-x-0" : "translate-x-full"
      )}>
        <div className="flex flex-col h-full">
          {/* Header */}
          <div className="flex items-center justify-between px-6 py-4 border-b">
            <h2 className="text-base font-medium">AI 寫作建議</h2>
            <Button
              variant="ghost"
              size="icon"
              onClick={handleForceClose}
            >
              <X className="h-4 w-4" />
            </Button>
          </div>

          {/* Content */}
          <div ref={chatContentRef} className="flex-1 overflow-auto p-6 space-y-4 scroll-smooth">
            {selectedText && (
              <div className="p-4 bg-muted rounded-md">
                <div className="text-sm text-muted-foreground mb-2">選取的文字：</div>
                <div className="text-sm">{selectedText}</div>
              </div>
            )}

            {/* 使用 JSON 中的預設命令 */}
            <div className="grid grid-cols-3 gap-2">
              {commands.presetCommands.map((preset, index) => (
                <Button
                  key={index}
                  variant="outline" 
                  className="h-auto py-2 border-2"
                  onClick={() => {
                    setCommand(preset.command)
                  }}
                  disabled={isLoading}
                >
                  {preset.label}
                </Button>
              ))}
            </div>
              
            <div className="relative mt-4">
              <Textarea
                placeholder="輸入你想要的建議，例如：幫我改寫得更生動..."
                value={command}
                onChange={(e) => setCommand(e.target.value)}
                rows={3}
                className="text-base resize-none"
              />
              <Button
                className="w-full mt-2"
                onClick={handleAIAssist}
                disabled={isLoading || !command.trim()}
              >
                {isLoading ? "處理中..." : "取得建議"}
              </Button>
            </div>

            {(isLoading || suggestion) && (
              <div className="mt-6 space-y-4">
                <div className="p-6 bg-muted rounded-md">
                  {isLoading && !suggestion && (
                    <div className="animate-pulse text-sm">AI 正在思考中...</div>
                  )}
                  {suggestion && (
                    <div 
                      dangerouslySetInnerHTML={{ 
                        __html: renderWithCopyButton(suggestion)
                      }}
                      className="prose max-w-none prose-pre:whitespace-pre-wrap prose-pre:break-words prose-pre:text-base prose-pre:leading-relaxed prose-headings:text-base prose-headings:font-medium prose-pre:relative prose-pre:p-4"
                    />
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