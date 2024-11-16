;; 作者：Claude
;; 版本：1.0
;; 模型：claude sonnet
;; 用途：群眾洞察文章生成器

(defun crowdsource-insight-composer (topic user-data patterns solutions)
  "主函數：生成群眾洞察文章"
  (let* ((user-insights (analyze-user-insights user-data))
         (common-issues (identify-common-issues patterns))
         (misconceptions (find-misconceptions user-data))
         (solution-map (create-solution-map solutions))
         (expert-advice (compile-expert-advice user-data solutions))
         (implementation-guide (create-implementation-guide solutions)))
    (format-crowdsource-post topic user-insights common-issues 
                            misconceptions solution-map 
                            expert-advice implementation-guide)))

(defun analyze-user-insights (data)
  "分析使用者洞察"
  (list
   (pain-points data)
   (user-questions data)
   (usage-scenarios data)
   (user-feedback data)))

(defun identify-common-issues (patterns)
  "識別常見問題"
  (list
   (recurring-problems patterns)
   (difficulty-levels patterns)
   (usage-barriers patterns)
   (failure-patterns patterns)))

(defun find-misconceptions (data)
  "找出常見誤解"
  (list
   (common-myths data)
   (misunderstandings data)
   (incorrect-usage data)
   (expectation-gaps data)))

(defun create-solution-map (solutions)
  "建立解決方案地圖"
  (list
   (categorize-solutions solutions)
   (solution-principles solutions)
   (implementation-steps solutions)
   (success-criteria solutions)))

(defun compile-expert-advice (data solutions)
  "整理專家建議"
  (list
   (best-practices solutions)
   (expert-tips data)
   (optimization-suggestions solutions)
   (advanced-techniques data)))

(defun create-implementation-guide (solutions)
  "建立實施指南"
  (list
   (getting-started solutions)
   (common-pitfalls solutions)
   (progress-metrics solutions)
   (adaptation-guidelines solutions)))

(defun format-crowdsource-post 
    (topic insights issues misconceptions solutions advice guide)
  "格式化群眾洞察文"
  
  (format t "~%🔍 ~A：群眾經驗大彙整~%" topic)
  
  (format t "~%💭 常見疑惑~%~{~A~%~}" 
          (format-user-questions insights))
  
  (format t "~%❌ 迷思破解~%~{- ~A~%~}" 
          (format-misconceptions misconceptions))
  
  (format t "~%🎯 核心問題~%~{~A~%~}" 
          (format-core-issues issues))
  
  (format t "~%💡 解決方案~%~{~A~%~}" 
          (format-solutions solutions))
  
  (format t "~%👥 案例分享~%~{- ~A~%~}" 
          (format-case-studies insights))
  
  (format t "~%✨ 專家建議~%~{~A~%~}" 
          (format-expert-advice advice))
  
  (format t "~%📝 實踐指南~%~A" 
          (format-implementation-guide guide))
  
  (format t "~%🤝 經驗交流~%~A" 
          (generate-discussion-prompts insights)))

;; 輔助函數
(defun pain-points (data)
  "痛點分析")

(defun user-questions (data)
  "使用者問題")

(defun usage-scenarios (data)
  "使用場景")

(defun user-feedback (data)
  "使用者回饋")

(defun recurring-problems (patterns)
  "重複問題")

(defun difficulty-levels (patterns)
  "困難程度")

(defun usage-barriers (patterns)
  "使用障礙")

(defun failure-patterns (patterns)
  "失敗模式")

(defun common-myths (data)
  "常見迷思")

(defun misunderstandings (data)
  "誤解")

(defun incorrect-usage (data)
  "錯誤使用")

(defun expectation-gaps (data)
  "期望差距")

(defun categorize-solutions (solutions)
  "解決方案分類")

(defun solution-principles (solutions)
  "解決原則")

(defun implementation-steps (solutions)
  "實施步驟")

(defun success-criteria (solutions)
  "成功標準")

(defun best-practices (solutions)
  "最佳實踐")

(defun expert-tips (data)
  "專家提示")

(defun optimization-suggestions (solutions)
  "優化建議")

(defun advanced-techniques (data)
  "進階技巧")

(defun getting-started (solutions)
  "入門指南")

(defun common-pitfalls (solutions)
  "常見陷阱")

(defun progress-metrics (solutions)
  "進度指標")

(defun adaptation-guidelines (solutions)
  "調適指南")

(defun usage-guide ()
  "使用指南"
  (format t "
群眾洞察文章生成器使用指南：

1. 輸入四個參數：
   - topic: 主題
   - user-data: 使用者資料
   - patterns: 模式分析
   - solutions: 解決方案

2. 系統會自動生成：
   - 常見疑惑整理
   - 迷思破解
   - 核心問題分析
   - 解決方案建議
   - 案例分享
   - 專家建議
   - 實踐指南
   - 經驗交流

使用示例：
(crowdsource-insight-composer 
  \"卡片盒筆記法使用指南\"
  '((問題 . 使用者問題集)
    (回饋 . 使用者回饋)
    (場景 . 使用場景))
  '((模式 . 使用模式)
    (困難 . 常見困難)
    (誤區 . 常見誤區))
  '((方法 . 解決方法)
    (步驟 . 實施步驟)
    (建議 . 專家建議)))

輸出將包含：
- 完整的問題分析
- 系統化的解決方案
- 實際的案例分享
- 專業的指導建議
"))

;; 使用方式：
;; 1. 執行 (usage-guide) 查看說明
;; 2. 調用 (crowdsource-insight-composer 主題 '使用者資料 '模式分析 '解決方案) 開始生成