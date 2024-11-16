;; 作者：Claude
;; 版本：1.0
;; 模型：claude sonnet
;; 用途：教學文章生成器

(defun tutorial-post-composer (tutorial-info content-plan learning-path examples)
  "主函數：生成教學文章"
  (let* ((title (generate-tutorial-title tutorial-info))
         (intro (create-tutorial-intro tutorial-info))
         (prerequisites (list-prerequisites tutorial-info))
         (concepts (explain-key-concepts content-plan))
         (steps (design-learning-steps learning-path))
         (practical-examples (compile-examples examples))
         (resources (suggest-learning-resources tutorial-info)))
    (format-tutorial-post title intro prerequisites concepts 
                         steps practical-examples resources)))

(defun generate-tutorial-title (tutorial-info)
  "生成教學標題"
  (let ((title-templates
         '("新手指南：如何使用~A"
           "完整教學：~A入門到進階"
           "實用教程：~A使用全攻略"
           "零基礎學習~A：完整教學指南"
           "從入門到精通：~A完整教程")))
    (format nil
            (nth (random (length title-templates)) title-templates)
            (getf tutorial-info :subject))))

(defun create-tutorial-intro (tutorial-info)
  "創建教學介紹"
  (list
   (topic-introduction tutorial-info)
   (learning-objectives tutorial-info)
   (target-audience tutorial-info)
   (expected-outcomes tutorial-info)))

(defun list-prerequisites (tutorial-info)
  "列出預備知識"
  (list
   (required-knowledge tutorial-info)
   (required-tools tutorial-info)
   (required-setup tutorial-info)
   (preparation-steps tutorial-info)))

(defun explain-key-concepts (content-plan)
  "解釋核心概念"
  (loop for concept in content-plan
        collect
        (list
         :topic (concept-topic concept)
         :explanation (concept-explanation concept)
         :importance (concept-importance concept)
         :illustrations (concept-illustrations concept)
         :common-misconceptions (concept-misconceptions concept))))

(defun design-learning-steps (learning-path)
  "設計學習步驟"
  (loop for step in learning-path
        collect
        (list
         :title (step-title step)
         :objective (step-objective step)
         :instructions (step-instructions step)
         :tips (step-tips step)
         :troubleshooting (step-troubleshooting step))))

(defun compile-examples (examples)
  "整理實作範例"
  (loop for example in examples
        collect
        (list
         :scenario (example-scenario example)
         :setup (example-setup example)
         :solution (example-solution example)
         :code (example-code example)
         :notes (example-notes example))))

(defun suggest-learning-resources (tutorial-info)
  "建議學習資源"
  (list
   (reference-materials tutorial-info)
   (practice-exercises tutorial-info)
   (additional-readings tutorial-info)
   (community-resources tutorial-info)))

(defun format-tutorial-post 
    (title intro prerequisites concepts steps examples resources)
  "格式化教學文章"
  
  (format t "~%📚 ~A~%" title)
  
  (format t "~%📌 教學簡介~%~{~A~%~}" intro)
  
  (format t "~%🎯 預備知識~%")
  (loop for item in prerequisites do
        (format t "~A~%" item))
  
  (format t "~%💡 核心概念~%")
  (loop for concept in concepts do
        (format t "~%~A：~%" 
                (getf concept :topic))
        (format t "概念說明：~A~%" 
                (getf concept :explanation))
        (format t "重要性：~A~%" 
                (getf concept :importance))
        (format t "圖解說明：~%~{- ~A~%~}" 
                (getf concept :illustrations))
        (format t "常見誤解：~%~{- ~A~%~}" 
                (getf concept :common-misconceptions)))
  
  (format t "~%📝 學習步驟~%")
  (loop for step in steps do
        (format t "~%~A：~%" 
                (getf step :title))
        (format t "學習目標：~A~%" 
                (getf step :objective))
        (format t "操作說明：~%~{- ~A~%~}" 
                (getf step :instructions))
        (format t "實用技巧：~%~{- ~A~%~}" 
                (getf step :tips))
        (format t "問題排除：~%~{- ~A~%~}" 
                (getf step :troubleshooting)))
  
  (format t "~%💻 實作範例~%")
  (loop for example in examples do
        (format t "~%情境：~A~%" 
                (getf example :scenario))
        (format t "準備工作：~%~{- ~A~%~}" 
                (getf example :setup))
        (format t "解決方案：~%~A~%" 
                (getf example :solution))
        (format t "示例代碼：~%~A~%" 
                (getf example :code))
        (format t "補充說明：~%~{- ~A~%~}" 
                (getf example :notes)))
  
  (format t "~%📚 延伸資源~%~{~A~%~}" resources))

;; 輔助函數
(defun topic-introduction (tutorial-info)
  "主題介紹")

(defun learning-objectives (tutorial-info)
  "學習目標")

(defun target-audience (tutorial-info)
  "目標讀者")

(defun expected-outcomes (tutorial-info)
  "預期成果")

(defun required-knowledge (tutorial-info)
  "必備知識")

(defun required-tools (tutorial-info)
  "必備工具")

(defun required-setup (tutorial-info)
  "必要設定")

(defun preparation-steps (tutorial-info)
  "準備步驟")

(defun concept-topic (concept)
  "概念主題")

(defun concept-explanation (concept)
  "概念解釋")

(defun concept-importance (concept)
  "概念重要性")

(defun concept-illustrations (concept)
  "概念圖解")

(defun concept-misconceptions (concept)
  "概念誤解")

(defun step-title (step)
  "步驟標題")

(defun step-objective (step)
  "步驟目標")

(defun step-instructions (step)
  "步驟說明")

(defun step-tips (step)
  "步驟技巧")

(defun step-troubleshooting (step)
  "問題排除")

(defun example-scenario (example)
  "範例情境")

(defun example-setup (example)
  "範例設置")

(defun example-solution (example)
  "範例解決方案")

(defun example-code (example)
  "範例代碼")

(defun example-notes (example)
  "範例註釋")

(defun reference-materials (tutorial-info)
  "參考資料")

(defun practice-exercises (tutorial-info)
  "練習作業")

(defun additional-readings (tutorial-info)
  "延伸閱讀")

(defun community-resources (tutorial-info)
  "社群資源")

(defun usage-guide ()
  "使用指南"
  (format t "
教學文章生成器使用指南：

1. 輸入四個參數：
   - tutorial-info: 教學資訊
   - content-plan: 內容規劃
   - learning-path: 學習路徑
   - examples: 實作範例

2. 系統會自動生成：
   - 教學標題
   - 課程介紹
   - 預備知識
   - 核心概念
   - 學習步驟
   - 實作範例
   - 延伸資源

使用示例：
(tutorial-post-composer 
  '(:subject \"Obsidian\"
    :level \"入門到進階\"
    :scope \"完整教學\")
  '((基礎概念 . 說明)
    (核心功能 . 講解)
    (實用技巧 . 分享))
  '((環境設置 . 步驟)
    (基礎操作 . 教學)
    (進階應用 . 指導))
  '((範例1 . 說明)
    (範例2 . 示範)
    (範例3 . 實作)))

輸出將包含：
- 完整教學架構
- 詳細步驟說明
- 實用範例展示
- 問題解決指引
"))

;; 使用方式：
;; 1. 執行 (usage-guide) 查看說明
;; 2. 調用 (tutorial-post-composer 教學 '內容 '步驟 '範例)