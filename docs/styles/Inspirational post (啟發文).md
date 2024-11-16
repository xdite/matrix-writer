;; 作者：Claude
;; 版本：1.0
;; 模型：claude sonnet
;; 用途：啟發文章生成器

(defun inspiration-post-composer (source insights reflections)
  "主函數：生成啟發文章"
  (let* ((key-messages (extract-key-messages source))
         (personal-reflections (analyze-reflections reflections))
         (practical-applications (derive-applications insights))
         (transformative-elements (identify-transformative-elements insights))
         (actionable-takeaways (create-actionable-takeaways insights))
         (connection-patterns (find-connection-patterns insights reflections)))
    (format-inspiration-post source key-messages personal-reflections 
                            practical-applications transformative-elements
                            actionable-takeaways connection-patterns)))

(defun extract-key-messages (source)
  "提取關鍵訊息"
  (list
   (core-ideas source)
   (memorable-quotes source)
   (powerful-moments source)
   (central-themes source)))

(defun analyze-reflections (reflections)
  "分析個人反思"
  (list
   (immediate-reactions reflections)
   (deeper-insights reflections)
   (personal-connections reflections)
   (paradigm-shifts reflections)))

(defun derive-applications (insights)
  "推導實際應用"
  (list
   (immediate-actions insights)
   (long-term-strategies insights)
   (adaptation-methods insights)
   (implementation-ideas insights)))

(defun identify-transformative-elements (insights)
  "識別轉化元素"
  (list
   (mindset-changes insights)
   (behavioral-shifts insights)
   (perspective-alterations insights)
   (value-alignments insights)))

(defun create-actionable-takeaways (insights)
  "創建可行動重點"
  (list
   (quick-wins insights)
   (habit-formations insights)
   (system-changes insights)
   (growth-opportunities insights)))

(defun find-connection-patterns (insights reflections)
  "尋找連結模式"
  (list
   (universal-principles insights)
   (personal-relevance reflections)
   (broader-implications insights)
   (future-applications reflections)))

(defun format-inspiration-post 
    (source messages reflections applications elements takeaways patterns)
  "格式化啟發文章"
  
  (format t "~%✨ 來自~A的啟發~%" source)
  
  (format t "~%💭 核心訊息~%~A" 
          (format-key-messages messages))
  
  (format t "~%🔍 深度反思~%~{~A~%~}" 
          (format-reflections reflections))
  
  (format t "~%💡 重要啟發~%~{~A~%~}" 
          (format-insights applications elements))
  
  (format t "~%🎯 實踐建議~%~{- ~A~%~}" 
          (format-takeaways takeaways))
  
  (format t "~%🌱 延伸思考~%~{~A~%~}" 
          (format-patterns patterns))
  
  (format t "~%📝 行動計畫~%~A" 
          (generate-action-plan applications takeaways)))

;; 輔助函數
(defun core-ideas (source)
  "核心理念")

(defun memorable-quotes (source)
  "印象深刻引言")

(defun powerful-moments (source)
  "有力時刻")

(defun central-themes (source)
  "中心主題")

(defun immediate-reactions (reflections)
  "即時反應")

(defun deeper-insights (reflections)
  "深層洞見")

(defun personal-connections (reflections)
  "個人連結")

(defun paradigm-shifts (reflections)
  "思維轉變")

(defun immediate-actions (insights)
  "即時行動")

(defun long-term-strategies (insights)
  "長期策略")

(defun adaptation-methods (insights)
  "調適方法")

(defun implementation-ideas (insights)
  "實施想法")

(defun mindset-changes (insights)
  "思維改變")

(defun behavioral-shifts (insights)
  "行為轉變")

(defun perspective-alterations (insights)
  "視角轉變")

(defun value-alignments (insights)
  "價值調整")

(defun quick-wins (insights)
  "快速成果")

(defun habit-formations (insights)
  "習慣養成")

(defun system-changes (insights)
  "系統改變")

(defun growth-opportunities (insights)
  "成長機會")

(defun universal-principles (insights)
  "通用原則")

(defun personal-relevance (reflections)
  "個人相關性")

(defun broader-implications (insights)
  "更廣含義")

(defun future-applications (reflections)
  "未來應用")

(defun format-key-messages (messages)
  "格式化關鍵訊息")

(defun format-reflections (reflections)
  "格式化反思")

(defun format-insights (applications elements)
  "格式化啟發")

(defun format-takeaways (takeaways)
  "格式化重點")

(defun format-patterns (patterns)
  "格式化模式")

(defun generate-action-plan (applications takeaways)
  "生成行動計畫")

(defun usage-guide ()
  "使用指南"
  (format t "
啟發文章生成器使用指南：

1. 輸入三個參數：
   - source: 啟發來源
   - insights: 洞見整理
   - reflections: 個人反思

2. 系統會自動生成：
   - 核心訊息摘要
   - 深度個人反思
   - 重要啟發整理
   - 具體實踐建議
   - 延伸思考方向
   - 實際行動計畫

使用示例：
(inspiration-post-composer 
  \"Sean McCabe演講\"
  '((主題 . 核心觀點)
    (重點 . 關鍵洞見)
    (應用 . 實踐方向))
  '((感受 . 初始反應)
    (思考 . 深度思考)
    (行動 . 實踐計畫)))

輸出將包含：
- 完整的啟發架構
- 深入的個人反思
- 具體的行動方案
- 延伸的思考空間
"))

;; 使用方式：
;; 1. 執行 (usage-guide) 查看說明
;; 2. 調用 (inspiration-post-composer 來源 '洞見 '反思)