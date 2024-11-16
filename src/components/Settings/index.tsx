import React, { useState, useEffect } from 'react'
import { Settings as SettingsIcon } from 'lucide-react'
import { Button } from '@/components/ui/button'
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
  DialogTrigger,
} from '@/components/ui/dialog'
import { Input } from '@/components/ui/input'

export function Settings() {
  const [apiKey, setApiKey] = useState('')
  const [ideaCount, setIdeaCount] = useState('10')
  const [open, setOpen] = useState(false)

  useEffect(() => {
    const savedKey = localStorage.getItem('claudeApiKey')
    const savedCount = localStorage.getItem('ideaCount')
    if (savedKey) {
      setApiKey(savedKey)
    }
    if (savedCount) {
      setIdeaCount(savedCount)
    }
  }, [])

  const handleSave = () => {
    localStorage.setItem('claudeApiKey', apiKey)
    localStorage.setItem('ideaCount', ideaCount)
    setOpen(false)
  }

  return (
    <Dialog open={open} onOpenChange={setOpen}>
      <DialogTrigger asChild>
        <Button variant="ghost" size="icon">
          <SettingsIcon className="h-5 w-5" />
        </Button>
      </DialogTrigger>
      <DialogContent>
        <DialogHeader>
          <DialogTitle>Settings</DialogTitle>
          <DialogDescription>
            Configure your Claude API settings
          </DialogDescription>
        </DialogHeader>
        <div className="grid gap-4 py-4">
          <div className="grid gap-2">
            <label htmlFor="apiKey">Claude API Key</label>
            <Input
              id="apiKey"
              type="password"
              value={apiKey}
              onChange={(e) => setApiKey(e.target.value)}
              placeholder="Enter your Claude API key"
            />
          </div>
          <div className="grid gap-2">
            <label htmlFor="ideaCount">Number of Ideas to Generate</label>
            <Input
              id="ideaCount"
              type="number"
              min="1"
              max="20"
              value={ideaCount}
              onChange={(e) => setIdeaCount(e.target.value)}
            />
          </div>
        </div>
        <DialogFooter>
          <Button onClick={handleSave}>Save</Button>
        </DialogFooter>
      </DialogContent>
    </Dialog>
  )
} 