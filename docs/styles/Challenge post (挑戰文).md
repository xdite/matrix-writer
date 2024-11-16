;; 作者：Claude
;; 版本：1.0
;; 模型：claude sonnet
;; 用途：挑戰宣言文章生成器
;; Challenge post (挑戰文，例如《挑戰 365 天連續日更》)

(defun challenge-post-composer (challenge-details commitment rules)
  "主函數：生成挑戰宣言文"
  (let* ((motivation (analyze-motivation challenge-details))
         (challenge-structure (define-challenge-structure challenge-details rules))
         (preparation (plan-preparation challenge-details))
         (tracking-system (design-tracking-system rules))
         (accountability (create-accountability-system commitment))
         (risk-management (analyze-risks challenge-details)))
    (format-challenge-post motivation challenge-structure preparation 
                          tracking-system accountability risk-management)))

(defun analyze-motivation (details)
  "分析動機"
  (list
   (personal-why details)
   (target-outcome details)
   (inspiration-source details)
   (timing-reason details)))

(defun define-challenge-structure (details rules)
  "定義挑戰架構"
  (list
   (challenge-scope details rules)
   (success-criteria rules)
   (failure-conditions rules)
   (challenge-phases details)))

(defun plan-preparation (details)
  "規劃準備工作"
  (list
   (resource-requirements details)
   (preliminary-setup details)
   (skill-requirements details)
   (time-allocation details)))

(defun design-tracking-system (rules)
  "設計追蹤系統"
  (list
   (progress-metrics rules)
   (recording-method rules)
   (review-frequency rules)
   (adjustment-mechanism rules)))

(defun create-accountability-system (commitment)
  "建立責任制度"
  (list
   (public-commitment commitment)
   (support-system commitment)
   (consequence-setup commitment)
   (milestone-celebration commitment)))

(defun analyze-risks (details)
  "分析風險"
  (list
   (potential-obstacles details)
   (mitigation-strategies details)
   (backup-plans details)
   (adaptation-guidelines details)))

(defun format-challenge-post 
    (motivation structure preparation tracking accountability risks)
  "格式化挑戰宣言文"
  (format t "~%🎯 挑戰宣告~%~A" 
          (format-challenge-intro motivation))
  
  (format t "~%💪 挑戰內容~%~{- ~A~%~}" 
          (format-challenge-details structure))
  
  (format t "~%📋 規則設定~%~{~A~%~}" 
          (format-rules structure))
  
  (format t "~%🎬 準備工作~%~{- ~A~%~}" 
          (format-preparation preparation))
  
  (format t "~%📊 追蹤方式~%~{~A~%~}" 
          (format-tracking tracking))
  
  (format t "~%🤝 承諾機制~%~{- ~A~%~}" 
          (format-accountability accountability))
  
  (format t "~%⚠️ 風險控管~%~{- ~A~%~}" 
          (format-risks risks))
  
  (format t "~%🌟 期待成果~%~A" 
          (format-expected-outcomes motivation structure))
  
  (format t "~%📢 邀請參與~%~A" 
          (generate-invitation motivation)))

;; 輔助函數
(defun personal-why (details)
  "個人動機")

(defun target-outcome (details)
  "目標成果")

(defun inspiration-source (details)
  "靈感來源")

(defun timing-reason (details)
  "時機原因")

(defun challenge-scope (details rules)
  "挑戰範圍")

(defun success-criteria (rules)
  "成功標準")

(defun failure-conditions (rules)
  "失敗條件")

(defun challenge-phases (details)
  "挑戰階段")

(defun resource-requirements (details)
  "資源需求")

(defun preliminary-setup (details)
  "前期準備")

(defun skill-requirements (details)
  "技能需求")

(defun time-allocation (details)
  "時間分配")

(defun progress-metrics (rules)
  "進度指標")

(defun recording-method (rules)
  "記錄方法")

(defun review-frequency (rules)
  "檢視頻率")

(defun adjustment-mechanism (rules)
  "調整機制")

(defun public-commitment (commitment)
  "公開承諾")

(defun support-system (commitment)
  "支持系統")

(defun consequence-setup (commitment)
  "後果設定")

(defun milestone-celebration (commitment)
  "里程碑慶祝")

(defun potential-obstacles (details)
  "潛在障礙")

(defun mitigation-strategies (details)
  "緩解策略")

(defun backup-plans (details)
  "備用計畫")

(defun adaptation-guidelines (details)
  "調適指南")

(defun format-challenge-intro (motivation)
  "格式化挑戰介紹")

(defun format-challenge-details (structure)
  "格式化挑戰細節")

(defun format-rules (structure)
  "格式化規則")

(defun format-preparation (preparation)
  "格式化準備工作")

(defun format-tracking (tracking)
  "格式化追蹤方式")

(defun format-accountability (accountability)
  "格式化責任制度")

(defun format-risks (risks)
  "格式化風險")

(defun format-expected-outcomes (motivation structure)
  "格式化預期成果")

(defun generate-invitation (motivation)
  "生成邀請")

(defun usage-guide ()
  "使用指南"
  (format t "
挑戰宣言文章生成器使用指南：

1. 輸入三個參數：
   - challenge-details: 挑戰細節
   - commitment: 承諾方式
   - rules: 規則設定

2. 系統會自動生成：
   - 挑戰宣告
   - 挑戰內容
   - 規則設定
   - 準備工作
   - 追蹤方式
   - 承諾機制
   - 風險控管
   - 期待成果
   - 參與邀請

使用示例：
(challenge-post-composer 
  '((名稱 . \"365天連續日更\")
    (期間 . \"一年\")
    (強度 . \"每日必發\")
    (主題 . \"個人成長\"))
  '((公開 . \"社群平台\")
    (追蹤 . \"進度表格\")
    (夥伴 . \"讀者督促\"))
  '((每日 . \"至少500字\")
    (主題 . \"自選方向\")
    (補償 . \"公開道歉\")))

輸出將包含：
- 完整的挑戰架構
- 清晰的規則制度
- 具體的執行方案
- 明確的追蹤機制
- 完善的支持系統
"))

;; 使用方式：
;; 1. 執行 (usage-guide) 查看說明
;; 2. 調用 (challenge-post-composer 挑戰細節 '承諾方式 '規則設定) 開始生成