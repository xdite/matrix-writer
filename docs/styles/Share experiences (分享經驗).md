;; 作者：Claude
;; 版本：1.0
;; 模型：claude sonnet
;; 用途：經驗分享文章生成器

(defun experience-sharing-composer (experience-info process learnings outcomes)
  "主函數：生成經驗分享文章"
  (let* ((title (generate-sharing-title experience-info))
         (intro (create-experience-intro experience-info))
         (background (provide-background experience-info))
         (journey (document-journey process))
         (insights (analyze-learnings learnings))
         (results (summarize-outcomes outcomes))
         (advice (compile-advice experience-info learnings)))
    (format-experience-post title intro background journey 
                          insights results advice)))

(defun generate-sharing-title (experience-info)
  "生成分享標題"
  (let ((title-templates
         '("親身經歷：我的~A完整心得分享"
           "從零到一：~A全過程紀實"
           "實戰分享：~A經驗與建議"
           "真實體驗：我的~A歷程"
           "一手分享：~A完整心得報告")))
    (format nil
            (nth (random (length title-templates)) title-templates)
            (getf experience-info :topic))))

(defun create-experience-intro (experience-info)
  "創建經驗介紹"
  (list
   (experience-context experience-info)
   (motivation-sharing experience-info)
   (sharing-objectives experience-info)
   (key-takeaways experience-info)))

(defun provide-background (experience-info)
  "提供背景脈絡"
  (list
   (personal-background experience-info)
   (situation-context experience-info)
   (initial-preparation experience-info)
   (goals-setting experience-info)))

(defun document-journey (process)
  "記錄歷程"
  (loop for stage in process
        collect
        (list
         :phase (journey-phase stage)
         :activities (key-activities stage)
         :challenges (encountered-challenges stage)
         :solutions (applied-solutions stage)
         :reflections (stage-reflections stage))))

(defun analyze-learnings (learnings)
  "分析學習心得"
  (loop for learning in learnings
        collect
        (list
         :category (learning-category learning)
         :insights (key-insights learning)
         :mistakes (common-mistakes learning)
         :improvements (suggested-improvements learning)
         :tips (practical-tips learning))))

(defun summarize-outcomes (outcomes)
  "總結成果"
  (list
   (achieved-results outcomes)
   (unexpected-findings outcomes)
   (personal-growth outcomes)
   (future-implications outcomes)))

(defun compile-advice (experience-info learnings)
  "整理建議"
  (list
   (preparation-advice experience-info learnings)
   (process-suggestions experience-info learnings)
   (resource-recommendations experience-info learnings)
   (success-strategies experience-info learnings)))

(defun format-experience-post 
    (title intro background journey insights results advice)
  "格式化經驗分享文章"
  
  (format t "~%📝 ~A~%" title)
  
  (format t "~%📌 前言分享~%~{~A~%~}" intro)
  
  (format t "~%🎯 背景說明~%")
  (loop for item in background do
        (format t "~A~%" item))
  
  (format t "~%🚀 歷程紀實~%")
  (loop for stage in journey do
        (format t "~%~A 階段：~%" 
                (getf stage :phase))
        (format t "主要活動：~%~{- ~A~%~}" 
                (getf stage :activities))
        (format t "遇到挑戰：~%~{- ~A~%~}" 
                (getf stage :challenges))
        (format t "解決方案：~%~{- ~A~%~}" 
                (getf stage :solutions))
        (format t "階段心得：~%~{- ~A~%~}" 
                (getf stage :reflections)))
  
  (format t "~%💡 重要心得~%")
  (loop for insight in insights do
        (format t "~%~A：~%" 
                (getf insight :category))
        (format t "關鍵發現：~%~{- ~A~%~}" 
                (getf insight :insights))
        (format t "常見錯誤：~%~{- ~A~%~}" 
                (getf insight :mistakes))
        (format t "改善建議：~%~{- ~A~%~}" 
                (getf insight :improvements))
        (format t "實用技巧：~%~{- ~A~%~}" 
                (getf insight :tips)))
  
  (format t "~%✨ 最終成果~%~{~A~%~}" results)
  
  (format t "~%📋 建議分享~%~{~A~%~}" advice))

;; 輔助函數
(defun experience-context (experience-info)
  "經驗脈絡")

(defun motivation-sharing (experience-info)
  "動機分享")

(defun sharing-objectives (experience-info)
  "分享目的")

(defun key-takeaways (experience-info)
  "重要收穫")

(defun personal-background (experience-info)
  "個人背景")

(defun situation-context (experience-info)
  "情境脈絡")

(defun initial-preparation (experience-info)
  "初始準備")

(defun goals-setting (experience-info)
  "目標設定")

(defun journey-phase (stage)
  "歷程階段")

(defun key-activities (stage)
  "關鍵活動")

(defun encountered-challenges (stage)
  "遇到挑戰")

(defun applied-solutions (stage)
  "採用解決方案")

(defun stage-reflections (stage)
  "階段反思")

(defun learning-category (learning)
  "學習類別")

(defun key-insights (learning)
  "關鍵洞見")

(defun common-mistakes (learning)
  "常見錯誤")

(defun suggested-improvements (learning)
  "建議改進")

(defun practical-tips (learning)
  "實用技巧")

(defun achieved-results (outcomes)
  "達成結果")

(defun unexpected-findings (outcomes)
  "意外發現")

(defun personal-growth (outcomes)
  "個人成長")

(defun future-implications (outcomes)
  "未來啟示")

(defun preparation-advice (experience-info learnings)
  "準備建議")

(defun process-suggestions (experience-info learnings)
  "過程建議")

(defun resource-recommendations (experience-info learnings)
  "資源建議")

(defun success-strategies (experience-info learnings)
  "成功策略")

(defun usage-guide ()
  "使用指南"
  (format t "
經驗分享文章生成器使用指南：

1. 輸入四個參數：
   - experience-info: 經驗資訊
   - process: 過程細節
   - learnings: 學習心得
   - outcomes: 成果總結

2. 系統會自動生成：
   - 分享標題
   - 背景說明
   - 過程紀錄
   - 心得分析
   - 成果總結
   - 建議分享

使用示例：
(experience-sharing-composer 
  '(:topic \"產品經理面試\"
    :duration \"3個月經驗\"
    :scope \"10家公司\")
  '((準備階段 . 細節)
    (面試階段 . 過程)
    (總結階段 . 反思))
  '((技術面 . 心得)
    (產品面 . 經驗)
    (管理面 . 學習))
  '((成功案例 . 分析)
    (失敗教訓 . 檢討)
    (整體收穫 . 總結)))

輸出將包含：
- 完整經驗分享
- 詳細過程紀錄
- 重要心得整理
- 實用建議分享
"))

;; 使用方式：
;; 1. 執行 (usage-guide) 查看說明
;; 2. 調用 (experience-sharing-composer 經驗資訊 '過程 '心得 '成果)