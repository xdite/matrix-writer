;; 作者：Claude
;; 版本：1.0
;; 模型：claude sonnet
;; 用途：幕後分享文章生成器
;; Behind the scene post (幕後文，例如《日更之旅完成！第一輪「365 天每日寫作計畫」圓滿落幕》)

(defun behind-scenes-composer (project journey-data reflection)
  "主函數：生成幕後分享文章"
  (let* ((journey-narrative (construct-journey-narrative journey-data))
         (challenges (analyze-challenges journey-data))
         (learnings (extract-learnings journey-data reflection))
         (stats (compile-statistics journey-data))
         (personal-growth (analyze-growth reflection))
         (future-plans (plan-next-steps reflection)))
    (format-behind-scenes project journey-narrative challenges 
                         learnings stats personal-growth future-plans)))

(defun construct-journey-narrative (journey-data)
  "建構歷程敘事"
  (list
   (initial-motivation journey-data)
   (key-milestones journey-data)
   (pivotal-moments journey-data)
   (emotional-journey journey-data)))

(defun analyze-challenges (journey-data)
  "分析挑戰"
  (list
   (expected-difficulties journey-data)
   (unexpected-obstacles journey-data)
   (adaptation-strategies journey-data)
   (breakthrough-moments journey-data)))

(defun extract-learnings (journey-data reflection)
  "提取學習"
  (list
   (practical-skills journey-data)
   (personal-insights reflection)
   (workflow-improvements journey-data)
   (mindset-shifts reflection)))

(defun compile-statistics (journey-data)
  "統計數據"
  (list
   (quantitative-metrics journey-data)
   (progress-patterns journey-data)
   (performance-analysis journey-data)
   (resource-investment journey-data)))

(defun analyze-growth (reflection)
  "分析成長"
  (list
   (skill-development reflection)
   (perspective-changes reflection)
   (habit-formation reflection)
   (identity-evolution reflection)))

(defun plan-next-steps (reflection)
  "規劃下一步"
  (list
   (short-term-goals reflection)
   (long-term-vision reflection)
   (improvement-areas reflection)
   (future-experiments reflection)))

(defun format-behind-scenes 
    (project narrative challenges learnings stats growth plans)
  "格式化幕後分享文"
  (format t "~%🎯 計畫回顧：~A~%" project)
  
  (format t "~%💫 起心動念~%~A" 
          (format-motivation narrative))
  
  (format t "~%📊 數據分享~%~{- ~A~%~}" 
          (format-statistics stats))
  
  (format t "~%🎢 心路歷程~%~{~A~%~}" 
          (format-journey narrative challenges))
  
  (format t "~%💡 重要發現~%~{~A~%~}" 
          (format-learnings learnings))
  
  (format t "~%🌱 個人成長~%~{- ~A~%~}" 
          (format-growth growth))
  
  (format t "~%🔄 改進之處~%~{- ~A~%~}" 
          (format-improvements challenges learnings))
  
  (format t "~%🚀 未來展望~%~A" 
          (format-future-plans plans))
  
  (format t "~%🤝 感謝分享~%~A" 
          (generate-gratitude narrative growth)))

;; 輔助函數
(defun initial-motivation (journey-data)
  "初始動機")

(defun key-milestones (journey-data)
  "關鍵里程碑")

(defun pivotal-moments (journey-data)
  "關鍵時刻")

(defun emotional-journey (journey-data)
  "情感歷程")

(defun expected-difficulties (journey-data)
  "預期困難")

(defun unexpected-obstacles (journey-data)
  "意外障礙")

(defun adaptation-strategies (journey-data)
  "調適策略")

(defun breakthrough-moments (journey-data)
  "突破時刻")

(defun practical-skills (journey-data)
  "實務技能")

(defun personal-insights (reflection)
  "個人洞見")

(defun workflow-improvements (journey-data)
  "工作流程改進")

(defun mindset-shifts (reflection)
  "思維轉變")

(defun quantitative-metrics (journey-data)
  "量化指標")

(defun progress-patterns (journey-data)
  "進展模式")

(defun performance-analysis (journey-data)
  "表現分析")

(defun resource-investment (journey-data)
  "資源投入")

(defun skill-development (reflection)
  "技能發展")

(defun perspective-changes (reflection)
  "視角改變")

(defun habit-formation (reflection)
  "習慣養成")

(defun identity-evolution (reflection)
  "身份演變")

(defun short-term-goals (reflection)
  "短期目標")

(defun long-term-vision (reflection)
  "長期願景")

(defun improvement-areas (reflection)
  "改進領域")

(defun future-experiments (reflection)
  "未來實驗")

(defun format-motivation (narrative)
  "格式化動機")

(defun format-statistics (stats)
  "格式化統計")

(defun format-journey (narrative challenges)
  "格式化歷程")

(defun format-learnings (learnings)
  "格式化學習")

(defun format-growth (growth)
  "格式化成長")

(defun format-improvements (challenges learnings)
  "格式化改進")

(defun format-future-plans (plans)
  "格式化未來計畫")

(defun generate-gratitude (narrative growth)
  "生成感謝")

(defun usage-guide ()
  "使用指南"
  (format t "
幕後分享文章生成器使用指南：

1. 輸入三個參數：
   - project: 專案/計畫名稱
   - journey-data: 歷程資料
   - reflection: 個人反思

2. 系統會自動生成：
   - 計畫回顧概述
   - 起心動念說明
   - 數據化成果
   - 心路歷程分享
   - 重要發現整理
   - 個人成長記錄
   - 改進點分析
   - 未來計畫展望
   - 感謝與分享

使用示例：
(behind-scenes-composer 
  \"365天每日寫作計畫\"
  '((開始日期 . \"2023-01-01\")
    (完成文章 . 365)
    (總字數 . 730000)
    (堅持指數 . 98))
  '((最大挑戰 . \"時間管理\")
    (重要收穫 . \"寫作習慣\")
    (關鍵成長 . \"表達能力\")))

輸出將包含：
- 完整的幕後故事架構
- 數據化的成果展示
- 個人化的經驗分享
- 深度的反思內容
- 具體的未來規劃
"))

;; 使用方式：
;; 1. 執行 (usage-guide) 查看說明
;; 2. 調用 (behind-scenes-composer 計畫名稱 '歷程資料 '反思內容) 開始生成