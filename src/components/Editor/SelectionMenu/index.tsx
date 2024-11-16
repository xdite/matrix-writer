import { Editor, BubbleMenu } from '@tiptap/react'
import { Bold, Italic, Code, Quote } from 'lucide-react'

interface SelectionMenuProps {
  editor: Editor
}

export function SelectionMenu({ editor }: SelectionMenuProps) {
  if (!editor) {
    return null
  }

  return (
    <BubbleMenu 
      editor={editor} 
      tippyOptions={{ duration: 100 }}
      className="bg-white rounded-lg shadow-lg border p-1.5 min-w-[180px]"
    >
      <button
        className="flex items-center gap-2 w-full px-2 py-1.5 text-sm hover:bg-slate-100 rounded-sm"
        onClick={() => editor.chain().focus().toggleBold().run()}
      >
        <Bold className="w-4 h-4" />
        <span>粗體</span>
        <div className="ml-auto text-xs text-muted-foreground">⌘+B</div>
      </button>

      <button
        className="flex items-center gap-2 w-full px-2 py-1.5 text-sm hover:bg-slate-100 rounded-sm"
        onClick={() => editor.chain().focus().toggleItalic().run()}
      >
        <Italic className="w-4 h-4" />
        <span>斜體</span>
        <div className="ml-auto text-xs text-muted-foreground">⌘+I</div>
      </button>

      <button
        className="flex items-center gap-2 w-full px-2 py-1.5 text-sm hover:bg-slate-100 rounded-sm"
        onClick={() => editor.chain().focus().toggleCode().run()}
      >
        <Code className="w-4 h-4" />
        <span>程式碼</span>
        <div className="ml-auto text-xs text-muted-foreground">⌘+E</div>
      </button>

      <div className="h-px bg-border my-1" />

      <button
        className="flex items-center gap-2 w-full px-2 py-1.5 text-sm hover:bg-slate-100 rounded-sm"
        onClick={() => editor.chain().focus().toggleBlockquote().run()}
      >
        <Quote className="w-4 h-4" />
        <span>引用</span>
      </button>
    </BubbleMenu>
  )
} 