import { useEditor, EditorContent } from '@tiptap/react'
import StarterKit from '@tiptap/starter-kit'
import Placeholder from '@tiptap/extension-placeholder'
import CodeBlockLowlight from '@tiptap/extension-code-block-lowlight'
import { common, createLowlight } from 'lowlight'
import { Extension } from '@tiptap/core'
import { Plugin } from 'prosemirror-state'
import { 
  Bold, 
  Italic, 
  List, 
  ListOrdered, 
  Quote, 
  Heading2, 
  Undo, 
  Redo,
  Minus,
  Code,
  Save
} from 'lucide-react'
import { Button } from '@/components/ui/button'
import { cn } from '@/lib/utils'
import { SelectionMenu } from './SelectionMenu'
import { useEffect, useMemo, useRef } from 'react'
import { debounce } from 'lodash'

const lowlight = createLowlight(common)

const PreventContextMenu = Extension.create({
  name: 'preventContextMenu',

  addProseMirrorPlugins() {
    return [
      new Plugin({
        props: {
          handleDOMEvents: {
            contextmenu: (view, event) => {
              const { state } = view
              const { empty } = state.selection
              
              if (!empty) {
                event.preventDefault()
              }
              return true
            }
          }
        }
      })
    ]
  }
})

const CustomKeymap = Extension.create({
  name: 'customKeymap',

  addKeyboardShortcuts() {
    return {
      // Undo
      'Mod-z': () => {
        this.editor.commands.undo()
        return true
      },
      // Redo 
      'Mod-Shift-z': () => {
        this.editor.commands.redo()
        return true
      },
      // Windows 上的 Ctrl+Y 也可以 redo
      'Mod-y': () => {
        this.editor.commands.redo()
        return true
      },
    }
  },
})

interface EditorProps {
  value: string
  onChange: (value: string) => void
  onSave?: () => void
}

export function Editor({ value, onChange, onSave }: EditorProps) {
  const previousValueRef = useRef(value)
  const updateTimeoutRef = useRef<number>()

  // 建立防抖的 onChange 處理器
  const debouncedOnChange = useMemo(
    () => debounce((html: string) => {
      onChange(html)
    }, 300),
    [onChange]
  )

  const editor = useEditor({
    extensions: [
      StarterKit.configure({
        codeBlock: false,
      }),
      Placeholder.configure({
        placeholder: '開始寫作...',
      }),
      CodeBlockLowlight.configure({
        lowlight,
      }),
      PreventContextMenu,
      CustomKeymap,
    ],
    content: value,
    editorProps: {
      attributes: {
        class: 'prose prose-sm max-w-none focus:outline-none min-h-[500px] px-4 py-2',
      },
    },
    onUpdate: ({ editor }) => {
      // 清除之前的 timeout
      if (updateTimeoutRef.current) {
        window.clearTimeout(updateTimeoutRef.current)
      }

      // 設定新的 timeout 來延遲更新
      updateTimeoutRef.current = window.setTimeout(() => {
        const newContent = editor.getHTML()
        if (newContent !== value) {
          debouncedOnChange(newContent)
        }
      }, 100)
    },
  })

  // 清理防抖
  useEffect(() => {
    return () => {
      debouncedOnChange.cancel()
      if (updateTimeoutRef.current) {
        window.clearTimeout(updateTimeoutRef.current)
      }
    }
  }, [debouncedOnChange])

  // 優化 content 更新邏輯
  useEffect(() => {
    if (editor && value !== previousValueRef.current) {
      const selection = editor.state.selection
      editor.commands.setContent(value, false)
      // 嘗試保持光標位置
      editor.commands.setTextSelection(selection.from)
      previousValueRef.current = value
    }
  }, [value, editor])

  if (!editor) {
    return null
  }

  const ToolbarButton = ({ 
    onClick, 
    disabled, 
    isActive,
    children,
    title
  }: { 
    onClick: () => void
    disabled?: boolean
    isActive?: boolean
    children: React.ReactNode
    title?: string
  }) => (
    <Button
      variant="ghost"
      size="sm"
      onClick={onClick}
      disabled={disabled}
      className={cn(
        "h-8 w-8 p-0",
        isActive && "bg-muted text-foreground"
      )}
      title={title}
    >
      {children}
    </Button>
  )

  return (
    <div className="border rounded-md bg-white">
      <div className="border-b p-2 flex flex-wrap gap-1">
        <ToolbarButton
          onClick={() => editor.chain().focus().toggleBold().run()}
          isActive={editor.isActive('bold')}
          title="粗體 (Ctrl+B)"
        >
          <Bold className="h-4 w-4" />
        </ToolbarButton>
        <ToolbarButton
          onClick={() => editor.chain().focus().toggleItalic().run()}
          isActive={editor.isActive('italic')}
          title="斜體 (Ctrl+I)"
        >
          <Italic className="h-4 w-4" />
        </ToolbarButton>
        <ToolbarButton
          onClick={() => editor.chain().focus().toggleCode().run()}
          isActive={editor.isActive('code')}
          title="行內程式碼"
        >
          <Code className="h-4 w-4" />
        </ToolbarButton>

        <div className="w-px h-6 bg-border mx-1 my-auto" />

        <ToolbarButton
          onClick={() => editor.chain().focus().toggleHeading({ level: 2 }).run()}
          isActive={editor.isActive('heading', { level: 2 })}
          title="標題"
        >
          <Heading2 className="h-4 w-4" />
        </ToolbarButton>

        <div className="w-px h-6 bg-border mx-1 my-auto" />

        <ToolbarButton
          onClick={() => editor.chain().focus().toggleBulletList().run()}
          isActive={editor.isActive('bulletList')}
          title="無序列表"
        >
          <List className="h-4 w-4" />
        </ToolbarButton>
        <ToolbarButton
          onClick={() => editor.chain().focus().toggleOrderedList().run()}
          isActive={editor.isActive('orderedList')}
          title="有序列表"
        >
          <ListOrdered className="h-4 w-4" />
        </ToolbarButton>

        <div className="w-px h-6 bg-border mx-1 my-auto" />

        <ToolbarButton
          onClick={() => editor.chain().focus().toggleBlockquote().run()}
          isActive={editor.isActive('blockquote')}
          title="引用區塊"
        >
          <Quote className="h-4 w-4" />
        </ToolbarButton>
        <ToolbarButton
          onClick={() => editor.chain().focus().setHorizontalRule().run()}
          title="分隔線"
        >
          <Minus className="h-4 w-4" />
        </ToolbarButton>

        <div className="w-px h-6 bg-border mx-1 my-auto" />

        <ToolbarButton
          onClick={() => editor.chain().focus().toggleCodeBlock().run()}
          isActive={editor.isActive('codeBlock')}
          title="程式碼區塊"
        >
          <Code className="h-4 w-4" />
        </ToolbarButton>

        <div className="flex-1" />

        <ToolbarButton
          onClick={() => editor.chain().focus().undo().run()}
          disabled={!editor.can().undo()}
          title="復原 (Ctrl+Z)"
        >
          <Undo className="h-4 w-4" />
        </ToolbarButton>
        <ToolbarButton
          onClick={() => editor.chain().focus().redo().run()}
          disabled={!editor.can().redo()}
          title="重做 (Ctrl+Shift+Z)"
        >
          <Redo className="h-4 w-4" />
        </ToolbarButton>
      </div>

      <SelectionMenu editor={editor} onSave={onSave} />

      <EditorContent editor={editor} />
    </div>
  )
} 