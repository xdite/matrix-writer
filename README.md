# Matrix Writer 內容矩陣寫作系統

一個幫助內容創作者通過主題和風格矩陣快速生成寫作想法並完成寫作的應用。

## 功能特點

- **內容矩陣生成器**：結合不同主題和寫作風格，快速生成寫作點子
- **AI 輔助**：利用 Claude AI 生成高質量的寫作點子和完整文章
- **寫作管理**：保存、編輯和管理所有寫作項目
- **多種寫作風格**：支持多種常見的寫作風格（如清單文、教學文、案例分析等）
- **現代化編輯器**：提供流暢的寫作體驗

## 技術棧

- React 18
- TypeScript
- Vite
- Tailwind CSS
- shadcn/ui 元件庫
- Zod 表單驗證
- Lucide Icons
- React Router
- 本地存儲 + IndexedDB
- Claude AI API 集成
- Tiptap 編輯器

## 安裝與使用

### 前置需求

- Node.js 16+
- Yarn 套件管理器
- Claude AI API 密鑰（用於 AI 生成功能）

### 安裝步驟

```bash
# 克隆儲存庫
git clone https://github.com/yourusername/matrix-writer.git
cd matrix-writer

# 安裝依賴
yarn install

# 啟動開發服務器
yarn dev
```

應用將在 http://localhost:5173 運行。

## 使用說明

1. **生成寫作點子**：
   - 輸入想寫的主題
   - 選擇主題分類和寫作風格
   - 點擊「生成寫作點子」按鈕

2. **管理寫作清單**：
   - 將喜歡的點子加入寫作清單
   - 在「寫作清單」頁面查看所有已保存的點子

3. **寫作與編輯**：
   - 點擊寫作清單中的項目開始寫作
   - 使用 AI 輔助功能自動生成文章
   - 在編輯器中修改和完善文章
   - 保存進度以便隨時繼續

## 項目結構

```
src/
├── domains/             # 業務邏輯領域
│   └── matrix/          # 內容矩陣相關功能
│       ├── contexts/    # 上下文狀態管理
│       ├── services/    # 服務層
│       ├── components/  # 領域特定組件
│       └── types/       # 類型定義
│
├── pages/               # 頁面組件
│   ├── ListPage/        # 寫作清單頁面
│   ├── WritingPage/     # 寫作編輯頁面
│   └── WritingsPage/    # 文章管理頁面
│
├── components/          # 共用組件
├── lib/                 # 工具函數和服務
├── styles/              # 全局樣式
└── services/            # 全局服務
```

## 貢獻

歡迎提交問題和建議！請通過 GitHub Issues 提交您發現的問題或改進建議。

## 授權

[MIT License](LICENSE) 