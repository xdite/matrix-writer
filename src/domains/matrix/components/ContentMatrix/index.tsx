import React from 'react'
import { useMatrix } from '../../contexts/MatrixContext'
import { Button } from '@/components/ui/button'
import { PlusCircle } from 'lucide-react'

export function ContentMatrix() {
  const { topics, styles, getIdeasForCell } = useMatrix()

  return (
    <div className="w-full overflow-x-auto">
      <table className="min-w-full border-collapse">
        <thead>
          <tr>
            <th className="p-4 border"></th>
            {styles.map(style => (
              <th key={style.id} className="p-4 border font-medium">
                {style.name}
              </th>
            ))}
          </tr>
        </thead>
        <tbody>
          {topics.map(topic => (
            <tr key={topic.id}>
              <td className="p-4 border font-medium">{topic.name}</td>
              {styles.map(style => {
                const ideas = getIdeasForCell(topic.id, style.id)
                return (
                  <td key={style.id} className="p-4 border">
                    <div className="flex flex-col gap-2">
                      <div className="text-sm text-gray-500">
                        {ideas.length} ideas
                      </div>
                      <Button
                        variant="ghost"
                        size="sm"
                        className="w-full"
                      >
                        <PlusCircle className="w-4 h-4 mr-2" />
                        Add Idea
                      </Button>
                    </div>
                  </td>
                )
              })}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  )
} 