;; 作者：Claude
;; 版本：1.0 
;; 模型：claude sonnet
;; 用途：清單文章生成器

(defun list-post-composer (topic items category)
  "主函數：生成清單文章"
  (let* ((title (generate-catchy-title topic items))
         (intro (create-compelling-intro topic category))
         (item-details (process-list-items items))
         (conclusion (write-actionable-conclusion topic item-details))
         (resources (suggest-further-resources topic category)))
    (format-list-post title intro item-details conclusion resources)))

(defun generate-catchy-title (topic items)
  "生成吸引人的標題"
  (let ((number (length items))
        (title-templates 
         '("~A 個你不能不知道的~A"
           "讓生活更簡單的 ~A 個~A"
           "新手必學的 ~A 個~A"
           "專家推薦：~A 個最強大的~A"
           "~A 個改變生活的~A秘訣")))
    (format nil 
            (nth (random (length title-templates)) title-templates)
            number topic)))

(defun create-compelling-intro (topic category)
  "創建引人入勝的開場"
  (let ((intro-elements
         (list
          (identify-pain-points category)
          (establish-relevance topic)
          (present-solution topic)
          (add-credibility category))))
    (format-introduction intro-elements)))

(defun process-list-items (items)
  "處理清單項目"
  (loop for item in items
        for i from 1
        collect
        (list
         :number i
         :title (item-title item)
         :description (expand-description item)
         :benefits (list-benefits item)
         :examples (practical-examples item)
         :tips (implementation-tips item))))

(defun write-actionable-conclusion (topic items)
  "寫出可行動的結論"
  (list
   (summarize-key-points items)
   (provide-next-steps topic)
   (encourage-action topic)
   (add-personal-note topic)))

(defun suggest-further-resources (topic category)
  "建議延伸資源"
  (list
   (related-articles topic)
   (recommended-tools category)
   (expert-recommendations topic)
   (community-resources category)))

(defun format-list-post (title intro items conclusion resources)
  "格式化清單文章"
  
  (format t "~%📝 ~A~%" title)
  
  (format t "~%📌 前言~%~A~%" intro)
  
  (format t "~%✨ 詳細內容~%")
  (loop for item in items do
        (format t "~%~A. ~A~%" 
                (getf item :number)
                (getf item :title))
        (format t "~A~%" 
                (getf item :description))
        (format t "🔑 主要優點：~%~{- ~A~%~}" 
                (getf item :benefits))
        (format t "💡 實際案例：~%~{- ~A~%~}" 
                (getf item :examples))
        (format t "📋 使用建議：~%~{- ~A~%~}" 
                (getf item :tips)))
  
  (format t "~%🎯 總結~%~{~A~%~}" conclusion)
  
  (format t "~%📚 延伸閱讀~%~{- ~A~%~}" resources))

;; 輔助函數
(defun identify-pain-points (category)
  "識別痛點")

(defun establish-relevance (topic)
  "建立相關性")

(defun present-solution (topic)
  "提出解決方案")

(defun add-credibility (category)
  "增加可信度")

(defun item-title (item)
  "項目標題")

(defun expand-description (item)
  "展開描述")

(defun list-benefits (item)
  "列舉好處")

(defun practical-examples (item)
  "實際例子")

(defun implementation-tips (item)
  "實施建議")

(defun summarize-key-points (items)
  "總結重點")

(defun provide-next-steps (topic)
  "提供下一步")

(defun encourage-action (topic)
  "鼓勵行動")

(defun add-personal-note (topic)
  "加入個人註記")

(defun related-articles (topic)
  "相關文章")

(defun recommended-tools (category)
  "推薦工具")

(defun expert-recommendations (topic)
  "專家建議")

(defun community-resources (category)
  "社群資源")

(defun format-introduction (elements)
  "格式化介紹")

(defun usage-guide ()
  "使用指南"
  (format t "
清單文章生成器使用指南：

1. 輸入三個參數：
   - topic: 文章主題
   - items: 清單項目
   - category: 文章類別

2. 系統會自動生成：
   - 吸引人的標題
   - 引人入勝的開場
   - 詳細的清單內容
   - 實用的總結建議
   - 延伸閱讀資源

使用示例：
(list-post-composer 
  \"生產力工具\"
  '((工具1 . 描述1) 
    (工具2 . 描述2)
    (工具3 . 描述3))
  '個人效率)

輸出將包含：
- 完整的文章架構
- 詳細的項目說明
- 實用的操作建議
- 延伸的參考資源
"))

;; 使用方式：
;; 1. 執行 (usage-guide) 查看說明
;; 2. 調用 (list-post-composer 主題 '清單項目 '類別)