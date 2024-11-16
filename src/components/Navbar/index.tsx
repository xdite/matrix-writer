import React from 'react'
import { Link, useLocation } from 'react-router-dom'
import { Settings } from '@/components/Settings'
import { CartButton } from '@/components/CartButton'
import { cn } from '@/lib/utils'

export function Navbar() {
  const location = useLocation()
  
  return (
    <div className="border-b bg-background">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="flex h-16 items-center justify-between">
          <div className="flex items-center">
            <Link to="/" className="text-xl font-bold">
              Matrix Writer
            </Link>
            <nav className="ml-10 flex items-center space-x-4">
              <Link
                to="/"
                className={cn(
                  "px-3 py-2 rounded-md text-sm font-medium transition-colors",
                  location.pathname === "/" 
                    ? "bg-muted" 
                    : "hover:bg-muted"
                )}
              >
                產生點子
              </Link>
              <Link
                to="/writings"
                className={cn(
                  "px-3 py-2 rounded-md text-sm font-medium transition-colors",
                  location.pathname === "/writings"
                    ? "bg-muted"
                    : "hover:bg-muted"
                )}
              >
                我的文章
              </Link>
            </nav>
          </div>
          <div className="flex items-center space-x-4">
            <CartButton />
            <Settings />
          </div>
        </div>
      </div>
    </div>
  )
} 