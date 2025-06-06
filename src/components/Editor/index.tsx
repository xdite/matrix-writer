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
import { useEffect, useMemo, useRef, useState } from 'react'
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
      // 新增儲存快捷鍵
      'Mod-s': () => {
        // 觸發自定義事件
        const event = new CustomEvent('editor-save')
        window.dispatchEvent(event)
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
  const [isDirty, setIsDirty] = useState(false)

  const editor = useEditor({
    extensions: [
      StarterKit.configure({
        codeBlock: false,
        history: {
          depth: 100,
          newGroupDelay: 500
        }
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
      setIsDirty(true)
    },
  })

  // 優化 content 更新邏輯
  useEffect(() => {
    if (editor && value !== previousValueRef.current) {
      const { from, to } = editor.state.selection
      const currentContent = editor.getHTML()
      
      if (value !== currentContent) {
        editor.chain()
          .setContent(value, false)
          .setTextSelection({ from, to })
          .run()
      }
      
      previousValueRef.current = value
      setIsDirty(false)
    }
  }, [value, editor])

  // 處理儲存事件
  useEffect(() => {
    const handleSave = (e: Event) => {
      e.preventDefault()
      if (isDirty) {
        handleSaveClick()
      }
    }

    // 監聽自定義事件
    window.addEventListener('editor-save', handleSave)
    // 同時攔截瀏覽器的儲存事件
    window.addEventListener('keydown', (e) => {
      if ((e.metaKey || e.ctrlKey) && e.key === 's') {
        e.preventDefault()
      }
    })

    return () => {
      window.removeEventListener('editor-save', handleSave)
      window.removeEventListener('keydown', handleSave)
    }
  }, [isDirty])

  // 重命名 handleSave 為 handleSaveClick 以避免混淆
  const handleSaveClick = () => {
    if (editor && isDirty) {
      onChange(editor.getHTML())
      onSave?.()
      setIsDirty(false)
    }
  }

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

        <Button
          variant="outline"
          size="sm"
          onClick={handleSaveClick}
          disabled={!isDirty}
          className="flex items-center gap-2"
          title="儲存 (Ctrl+S)"
        >
          <Save className="h-4 w-4" />
          {isDirty ? "儲存" : "已儲存"}
        </Button>
      </div>

      <SelectionMenu editor={editor} onSave={onSave} />

      <EditorContent editor={editor} />
    </div>
  )
} 