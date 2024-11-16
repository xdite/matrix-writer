;; 作者：Claude
;; 版本：1.0
;; 模型：claude sonnet
;; 用途：服務評測文章生成器

(defun service-review-composer (service experience quality cost-benefit)
  "主函數：生成服務評測文章"
  (let* ((title (generate-service-review-title service))
         (intro (create-service-intro service))
         (overview (service-overview service))
         (user-experience (analyze-user-experience experience))
         (quality-analysis (analyze-service-quality quality))
         (value-analysis (analyze-cost-benefit cost-benefit))
         (recommendations (make-recommendations service experience quality))
         (conclusion (draw-conclusions service experience cost-benefit)))
    (format-service-review title intro overview user-experience 
                          quality-analysis value-analysis 
                          recommendations conclusion)))

(defun generate-service-review-title (service)
  "生成服務評測標題"
  (let ((title-templates
         '("實際體驗：~A完整評測報告"
           "~A評測：一個月實際使用心得"
           "深度體驗~A：優缺點完整分析"
           "~A服務評測：性價比與使用體驗"
           "親身體驗~A：完整服務評測報告")))
    (format nil
            (nth (random (length title-templates)) title-templates)
            (getf service :name))))

(defun create-service-intro (service)
  "創建服務介紹"
  (list
   (service-background service)
   (service-positioning service)
   (target-users service)
   (review-methodology service)))

(defun service-overview (service)
  "服務概覽"
  (list
   (basic-information service)
   (service-features service)
   (pricing-plans service)
   (service-process service)))

(defun analyze-user-experience (experience)
  "分析使用體驗"
  (loop for aspect in experience
        collect
        (list
         :category (experience-category aspect)
         :process (experience-process aspect)
         :highlights (experience-highlights aspect)
         :pain-points (experience-pain-points aspect)
         :rating (calculate-experience-rating aspect))))

(defun analyze-service-quality (quality)
  "分析服務品質"
  (loop for metric in quality
        collect
        (list
         :aspect (quality-aspect metric)
         :standards (quality-standards metric)
         :performance (quality-performance metric)
         :consistency (quality-consistency metric)
         :score (calculate-quality-score metric))))

(defun analyze-cost-benefit (cost-benefit)
  "分析成本效益"
  (list
   (pricing-analysis cost-benefit)
   (value-comparison cost-benefit)
   (benefit-analysis cost-benefit)
   (cost-effectiveness cost-benefit)))

(defun make-recommendations (service experience quality)
  "提供建議"
  (list
   (suitable-users service experience)
   (usage-suggestions experience)
   (optimization-tips quality)
   (alternatives-comparison service)))

(defun draw-conclusions (service experience cost-benefit)
  "得出結論"
  (list
   (overall-assessment service experience)
   (highlight-advantages service experience)
   (address-limitations service experience)
   (final-recommendations cost-benefit)))

(defun format-service-review 
    (title intro overview experience quality value recommendations conclusion)
  "格式化服務評測文章"
  
  (format t "~%🔍 ~A~%" title)
  
  (format t "~%📌 服務簡介~%~{~A~%~}" intro)
  
  (format t "~%📋 服務概覽~%")
  (loop for item in overview do
        (format t "~A~%" item))
  
  (format t "~%💫 使用體驗~%")
  (loop for exp in experience do
        (format t "~%~A 體驗：~%" 
                (getf exp :category))
        (format t "使用流程：~A~%" 
                (getf exp :process))
        (format t "亮點：~%~{- ~A~%~}" 
                (getf exp :highlights))
        (format t "痛點：~%~{- ~A~%~}" 
                (getf exp :pain-points))
        (format t "評分：~A/10~%" 
                (getf exp :rating)))
  
  (format t "~%⭐ 服務品質~%")
  (loop for qual in quality do
        (format t "~%~A：~%" 
                (getf qual :aspect))
        (format t "品質標準：~A~%" 
                (getf qual :standards))
        (format t "表現：~A~%" 
                (getf qual :performance))
        (format t "一致性：~A~%" 
                (getf qual :consistency))
        (format t "評分：~A/10~%" 
                (getf qual :score)))
  
  (format t "~%💰 性價比分析~%~{~A~%~}" value)
  
  (format t "~%💡 使用建議~%~{~A~%~}" recommendations)
  
  (format t "~%🎯 總結評價~%~{~A~%~}" conclusion))

;; 輔助函數
(defun service-background (service)
  "服務背景")

(defun service-positioning (service)
  "服務定位")

(defun target-users (service)
  "目標用戶")

(defun review-methodology (service)
  "評測方法")

(defun basic-information (service)
  "基本信息")

(defun service-features (service)
  "服務特色")

(defun pricing-plans (service)
  "價格方案")

(defun service-process (service)
  "服務流程")

(defun experience-category (aspect)
  "體驗類別")

(defun experience-process (aspect)
  "體驗過程")

(defun experience-highlights (aspect)
  "體驗亮點")

(defun experience-pain-points (aspect)
  "體驗痛點")

(defun calculate-experience-rating (aspect)
  "計算體驗評分")

(defun quality-aspect (metric)
  "品質面向")

(defun quality-standards (metric)
  "品質標準")

(defun quality-performance (metric)
  "品質表現")

(defun quality-consistency (metric)
  "品質一致性")

(defun calculate-quality-score (metric)
  "計算品質分數")

(defun pricing-analysis (cost-benefit)
  "價格分析")

(defun value-comparison (cost-benefit)
  "價值比較")

(defun benefit-analysis (cost-benefit)
  "效益分析")

(defun cost-effectiveness (cost-benefit)
  "成本效益")

(defun suitable-users (service experience)
  "適合用戶")

(defun usage-suggestions (experience)
  "使用建議")

(defun optimization-tips (quality)
  "優化建議")

(defun alternatives-comparison (service)
  "替代方案")

(defun overall-assessment (service experience)
  "整體評估")

(defun highlight-advantages (service experience)
  "優勢重點")

(defun address-limitations (service experience)
  "限制說明")

(defun final-recommendations (cost-benefit)
  "最終建議")

(defun usage-guide ()
  "使用指南"
  (format t "
服務評測文章生成器使用指南：

1. 輸入四個參數：
   - service: 服務資訊
   - experience: 體驗數據
   - quality: 品質評估
   - cost-benefit: 性價比分析

2. 系統會自動生成：
   - 評測標題
   - 服務介紹
   - 使用體驗
   - 品質分析
   - 性價比評估
   - 建議總結

使用示例：
(service-review-composer 
  '(:name \"Engoo\"
    :type \"線上英文教學\"
    :category \"教育服務\")
  '((預約體驗 . 評價)
    (上課體驗 . 評價)
    (學習效果 . 評價))
  '((教師品質 . 評估)