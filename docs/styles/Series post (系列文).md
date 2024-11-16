;; 作者：Claude
;; 版本：1.0
;; 模型：claude sonnet
;; 用途：系列文章生成器

(defun series-post-composer (series-info content-plan post-details learning-path)
  "主函數：生成系列文章"
  (let* ((series-title (generate-series-title series-info))
         (series-intro (create-series-intro series-info))
         (content-structure (plan-content-structure content-plan))
         (articles (generate-article-outlines post-details))
         (progression (design-learning-progression learning-path))
         (resources (compile-resources series-info content-plan))
         (navigation (create-series-navigation articles)))
    (format-series-posts series-title series-intro content-structure 
                        articles progression resources navigation)))

(defun generate-series-title (series-info)
  "生成系列標題"
  (let ((title-templates
         '("最完整的~A系列教學"
           "從零開始學習~A：完整系列指南"
           "~A進階指南：從入門到精通"
           "~A完全攻略：系統化學習指南"
           "深入淺出~A：階段式學習系列")))
    (format nil
            (nth (random (length title-templates)) title-templates)
            (getf series-info :topic))))

(defun create-series-intro (series-info)
  "創建系列介紹"
  (list
   (series-overview series-info)
   (learning-objectives series-info)
   (target-audience series-info)
   (series-structure series-info)))

(defun plan-content-structure (content-plan)
  "規劃內容結構"
  (list
   (knowledge-hierarchy content-plan)
   (topic-relationships content-plan)
   (difficulty-progression content-plan)
   (content-dependencies content-plan)))

(defun generate-article-outlines (post-details)
  "生成文章大綱"
  (loop for post in post-details
        collect
        (list
         :number (post-number post)
         :title (article-title post)
         :objectives (learning-goals post)
         :prerequisites (prerequisites post)
         :content (content-outline post)
         :exercises (practice-exercises post)
         :resources (article-resources post))))

(defun design-learning-progression (learning-path)
  "設計學習進程"
  (list
   (skill-progression learning-path)
   (milestone-definition learning-path)
   (progress-tracking learning-path)
   (mastery-criteria learning-path)))

(defun compile-resources (series-info content-plan)
  "整理資源"
  (list
   (reference-materials series-info)
   (supplementary-content content-plan)
   (practice-materials content-plan)
   (community-resources series-info)))

(defun create-series-navigation (articles)
  "創建系列導航"
  (list
   (table-of-contents articles)
   (reading-order articles)
   (related-articles articles)
   (quick-references articles)))

(defun format-series-posts 
    (title intro structure articles progression resources navigation)
  "格式化系列文章"
  
  (format t "~%📚 ~A~%" title)
  
  (format t "~%📌 系列介紹~%~{~A~%~}" intro)
  
  (format t "~%🗺️ 內容地圖~%")
  (loop for item in structure do
        (format t "~A~%" item))
  
  (format t "~%📖 系列文章~%")
  (loop for article in articles do
        (format t "~%第 ~A 篇：~A~%" 
                (getf article :number)
                (getf article :title))
        (format t "學習目標：~%~{- ~A~%~}" 
                (getf article :objectives))
        (format t "預備知識：~%~{- ~A~%~}" 
                (getf article :prerequisites))
        (format t "內容大綱：~%~{~A~%~}" 
                (getf article :content))
        (format t "練習作業：~%~{- ~A~%~}" 
                (getf article :exercises))
        (format t "參考資源：~%~{- ~A~%~}" 
                (getf article :resources)))
  
  (format t "~%📈 學習進程~%~{~A~%~}" progression)
  
  (format t "~%📚 學習資源~%~{~A~%~}" resources)
  
  (format t "~%🧭 系列導航~%~{~A~%~}" navigation))

;; 輔助函數
(defun series-overview (series-info)
  "系列概覽")

(defun learning-objectives (series-info)
  "學習目標")

(defun target-audience (series-info)
  "目標讀者")

(defun series-structure (series-info)
  "系列結構")

(defun knowledge-hierarchy (content-plan)
  "知識層級")

(defun topic-relationships (content-plan)
  "主題關聯")

(defun difficulty-progression (content-plan)
  "難度進程")

(defun content-dependencies (content-plan)
  "內容相依")

(defun post-number (post)
  "文章編號")

(defun article-title (post)
  "文章標題")

(defun learning-goals (post)
  "學習目標")

(defun prerequisites (post)
  "預備知識")

(defun content-outline (post)
  "內容大綱")

(defun practice-exercises (post)
  "練習作業")

(defun article-resources (post)
  "文章資源")

(defun skill-progression (learning-path)
  "技能進程")

(defun milestone-definition (learning-path)
  "里程碑定義")

(defun progress-tracking (learning-path)
  "進度追蹤")

(defun mastery-criteria (learning-path)
  "精通標準")

(defun reference-materials (series-info)
  "參考資料")

(defun supplementary-content (content-plan)
  "補充內容")

(defun practice-materials (content-plan)
  "練習材料")

(defun community-resources (series-info)
  "社群資源")

(defun table-of-contents (articles)
  "目錄")

(defun reading-order (articles)
  "閱讀順序")

(defun related-articles (articles)
  "相關文章")

(defun quick-references (articles)
  "快速參考")

(defun usage-guide ()
  "使用指南"
  (format t "
系列文章生成器使用指南：

1. 輸入四個參數：
   - series-info: 系列資訊
   - content-plan: 內容規劃
   - post-details: 文章細節
   - learning-path: 學習路徑

2. 系統會自動生成：
   - 系列標題與介紹
   - 內容結構規劃
   - 完整文章大綱
   - 學習進程設計
   - 資源整理
   - 系列導航

使用示例：
(series-post-composer 
  '(:topic \"Obsidian\"
    :level \"從入門到進階\"
    :duration \"完整系列\")
  '((基礎知識 . 規劃)
    (核心功能 . 內容)
    (進階應用 . 主題))
  '((文章1 . 細節)
    (文章2 . 細節)
    (文章3 . 細節))
  '((入門 . 路徑)
    (進階 . 規劃)
    (精通 . 標準)))

輸出將包含：
- 完整系列架構
- 詳細文章大綱
- 學習路徑設計
- 配套學習資源
- 系列文導航
"))

;; 使用方式：
;; 1. 執行 (usage-guide) 查看說明
;; 2. 調用 (series-post-composer 系列資訊 '內容規劃 '文章細節 '學習路徑)