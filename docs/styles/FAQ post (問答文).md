;; 作者：Claude
;; 版本：1.0
;; 模型：claude sonnet
;; 用途：常見問答文章生成器

(defun faq-post-composer (topic questions user-data expert-input)
  "主函數：生成FAQ文章"
  (let* ((categorized-questions (categorize-questions questions))
         (detailed-answers (create-answers questions user-data))
         (expert-insights (integrate-expert-insights expert-input))
         (practical-examples (extract-examples user-data))
         (supplementary-info (gather-supplementary-info questions expert-input))
         (navigation-guide (create-navigation-guide categorized-questions)))
    (format-faq-post topic categorized-questions detailed-answers 
                     expert-insights practical-examples 
                     supplementary-info navigation-guide)))

(defun categorize-questions (questions)
  "問題分類"
  (list
   (basic-questions questions)
   (intermediate-questions questions)
   (advanced-questions questions)
   (special-cases questions)))

(defun create-answers (questions user-data)
  "建立詳細答案"
  (list
   (direct-answers questions)
   (expanded-explanations questions)
   (common-scenarios user-data)
   (troubleshooting-steps questions)))

(defun integrate-expert-insights (expert-input)
  "整合專家見解"
  (list
   (best-practices expert-input)
   (pro-tips expert-input)
   (optimization-advice expert-input)
   (future-considerations expert-input)))

(defun extract-examples (user-data)
  "提取實例"
  (list
   (success-cases user-data)
   (failure-cases user-data)
   (alternative-approaches user-data)
   (user-solutions user-data)))

(defun gather-supplementary-info (questions expert-input)
  "收集補充資訊"
  (list
   (related-resources questions)
   (advanced-topics expert-input)
   (common-misconceptions questions)
   (updates-changes expert-input)))

(defun create-navigation-guide (categories)
  "建立導覽指南"
  (list
   (quick-reference categories)
   (topic-index categories)
   (difficulty-levels categories)
   (learning-path categories)))

(defun format-faq-post 
    (topic questions answers insights examples info guide)
  "格式化FAQ文章"
  
  (format t "~%❓ ~A~%" topic)
  
  (format t "~%📚 內容導覽~%~A" 
          (format-navigation guide))
  
  (format t "~%🔍 快速索引~%~{~A~%~}" 
          (format-index questions))
  
  (dolist (category questions)
    (format t "~%~A~%~{Q: ~A~%A: ~A~%~%~}" 
            (format-category-title category)
            (format-qa-pair category answers)))
  
  (format t "~%💡 專家補充~%~{~A~%~}" 
          (format-insights insights))
  
  (format t "~%📝 實例分享~%~{- ~A~%~}" 
          (format-examples examples))
  
  (format t "~%📌 補充資源~%~{~A~%~}" 
          (format-supplementary-info info))
  
  (format t "~%💭 延伸閱讀~%~A" 
          (generate-related-topics questions info)))

;; 輔助函數
(defun basic-questions (questions)
  "基礎問題")

(defun intermediate-questions (questions)
  "進階問題")

(defun advanced-questions (questions)
  "高級問題")

(defun special-cases (questions)
  "特殊情況")

(defun direct-answers (questions)
  "直接答案")

(defun expanded-explanations (questions)
  "詳細解釋")

(defun common-scenarios (user-data)
  "常見場景")

(defun troubleshooting-steps (questions)
  "故障排除步驟")

(defun best-practices (expert-input)
  "最佳實踐")

(defun pro-tips (expert-input)
  "專業提示")

(defun optimization-advice (expert-input)
  "優化建議")

(defun future-considerations (expert-input)
  "未來考量")

(defun success-cases (user-data)
  "成功案例")

(defun failure-cases (user-data)
  "失敗案例")

(defun alternative-approaches (user-data)
  "替代方案")

(defun user-solutions (user-data)
  "使用者解決方案")

(defun related-resources (questions)
  "相關資源")

(defun advanced-topics (expert-input)
  "進階主題")

(defun common-misconceptions (questions)
  "常見誤解")

(defun updates-changes (expert-input)
  "更新變更")

(defun quick-reference (categories)
  "快速參考")

(defun topic-index (categories)
  "主題索引")

(defun difficulty-levels (categories)
  "難度等級")

(defun learning-path (categories)
  "學習路徑")

(defun format-navigation (guide)
  "格式化導覽")

(defun format-index (questions)
  "格式化索引")

(defun format-category-title (category)
  "格式化類別標題")

(defun format-qa-pair (category answers)
  "格式化問答對")

(defun format-insights (insights)
  "格式化見解")

(defun format-examples (examples)
  "格式化實例")

(defun format-supplementary-info (info)
  "格式化補充資訊")

(defun generate-related-topics (questions info)
  "生成相關主題")

(defun usage-guide ()
  "使用指南"
  (format t "
FAQ文章生成器使用指南：

1. 輸入四個參數：
   - topic: 主題
   - questions: 問題集
   - user-data: 使用者資料
   - expert-input: 專家意見

2. 系統會自動生成：
   - 內容導覽
   - 快速索引
   - 分類問答
   - 專家補充
   - 實例分享
   - 補充資源
   - 延伸閱讀

使用示例：
(faq-post-composer 
  \"Obsidian新手常見問題\"
  '((基礎 . 基礎問題集)
    (進階 . 進階問題集)
    (特殊 . 特殊問題集))
  '((場景 . 使用場景)
    (回饋 . 使用者回饋)
    (解法 . 解決方案))
  '((建議 . 專家建議)
    (技巧 . 進階技巧)
    (展望 . 發展方向)))

輸出將包含：
- 完整的問答結構
- 清晰的分類系統
- 實用的案例分享
- 專業的補充建議
"))

;; 使用方式：
;; 1. 執行 (usage-guide) 查看說明
;; 2. 調用 (faq-post-composer 主題 '問題集 '使用者資料 '專家意見) 開始生成