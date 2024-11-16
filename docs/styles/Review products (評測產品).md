;; 作者：Claude
;; 版本：1.0
;; 模型：claude sonnet
;; 用途：產品評測文章生成器

(defun product-review-composer (product features testing comparison)
  "主函數：生成產品評測文章"
  (let* ((title (generate-review-title product))
         (intro (create-product-intro product))
         (overview (product-overview product features))
         (test-results (analyze-test-results testing))
         (comparisons (competitive-analysis comparison))
         (verdict (final-verdict product testing comparison))
         (recommendations (user-recommendations product features testing)))
    (format-product-review title intro overview test-results 
                          comparisons verdict recommendations)))

(defun generate-review-title (product)
  "生成評測標題"
  (let ((title-templates
         '("深度評測：~A完整功能解析"
           "~A評測：值得入手嗎？"
           "實測~A：優缺點完整分析"
           "~A完整評測：功能特色與使用體驗"
           "一個月使用心得：~A全方位測評")))
    (format nil
            (nth (random (length title-templates)) title-templates)
            (getf product :name))))

(defun create-product-intro (product)
  "創建產品介紹"
  (list
   (product-positioning product)
   (target-audience product)
   (key-selling-points product)
   (testing-methodology product)))

(defun product-overview (product features)
  "產品概覽"
  (list
   (specifications product)
   (feature-breakdown features)
   (setup-experience product)
   (interface-analysis product)))

(defun analyze-test-results (testing)
  "分析測試結果"
  (loop for test in testing
        collect
        (list
         :category (test-category test)
         :procedure (test-procedure test)
         :results (test-results test)
         :observations (test-observations test)
         :scores (calculate-scores test))))

(defun competitive-analysis (comparison)
  "競品分析"
  (loop for competitor in comparison
        collect
        (list
         :product (competitor-name competitor)
         :strengths (competitor-strengths competitor)
         :weaknesses (competitor-weaknesses competitor)
         :price-value (price-comparison competitor)
         :target-users (target-comparison competitor))))

(defun final-verdict (product testing comparison)
  "最終評價"
  (list
   (overall-rating product testing)
   (key-advantages product testing)
   (main-drawbacks product testing)
   (value-assessment product comparison)))

(defun user-recommendations (product features testing)
  "使用者建議"
  (list
   (ideal-users product features)
   (use-cases product features)
   (setup-tips product testing)
   (alternative-suggestions product testing)))

(defun format-product-review 
    (title intro overview test-results comparisons verdict recommendations)
  "格式化產品評測文章"
  
  (format t "~%📱 ~A~%" title)
  
  (format t "~%📌 產品簡介~%~{~A~%~}" intro)
  
  (format t "~%🔍 產品概覽~%")
  (loop for item in overview do
        (format t "~A~%" item))
  
  (format t "~%⚡ 測試結果~%")
  (loop for result in test-results do
        (format t "~%~A 測試：~%" 
                (getf result :category))
        (format t "測試方法：~A~%" 
                (getf result :procedure))
        (format t "測試結果：~%~{- ~A~%~}" 
                (getf result :results))
        (format t "使用觀察：~%~{- ~A~%~}" 
                (getf result :observations))
        (format t "評分：~A/10~%" 
                (getf result :scores)))
  
  (format t "~%📊 競品比較~%")
  (loop for comp in comparisons do
        (format t "~%與 ~A 比較：~%" 
                (getf comp :product))
        (format t "優勢：~%~{- ~A~%~}" 
                (getf comp :strengths))
        (format t "劣勢：~%~{- ~A~%~}" 
                (getf comp :weaknesses))
        (format t "價值比：~A~%" 
                (getf comp :price-value))
        (format t "目標用戶：~A~%" 
                (getf comp :target-users)))
  
  (format t "~%🎯 最終評價~%~{~A~%~}" verdict)
  
  (format t "~%💡 使用建議~%~{~A~%~}" recommendations))

;; 輔助函數
(defun product-positioning (product)
  "產品定位")

(defun target-audience (product)
  "目標用戶")

(defun key-selling-points (product)
  "主要賣點")

(defun testing-methodology (product)
  "測試方法")

(defun specifications (product)
  "規格說明")

(defun feature-breakdown (features)
  "功能分析")

(defun setup-experience (product)
  "設置體驗")

(defun interface-analysis (product)
  "介面分析")

(defun test-category (test)
  "測試類別")

(defun test-procedure (test)
  "測試程序")

(defun test-results (test)
  "測試結果")

(defun test-observations (test)
  "測試觀察")

(defun calculate-scores (test)
  "計算評分")

(defun competitor-name (competitor)
  "競品名稱")

(defun competitor-strengths (competitor)
  "競品優勢")

(defun competitor-weaknesses (competitor)
  "競品劣勢")

(defun price-comparison (competitor)
  "價格比較")

(defun target-comparison (competitor)
  "目標比較")

(defun overall-rating (product testing)
  "整體評分")

(defun key-advantages (product testing)
  "主要優勢")

(defun main-drawbacks (product testing)
  "主要缺點")

(defun value-assessment (product comparison)
  "價值評估")

(defun ideal-users (product features)
  "理想用戶")

(defun use-cases (product features)
  "使用場景")

(defun setup-tips (product testing)
  "設置建議")

(defun alternative-suggestions (product testing)
  "替代建議")

(defun usage-guide ()
  "使用指南"
  (format t "
產品評測文章生成器使用指南：

1. 輸入四個參數：
   - product: 產品資訊
   - features: 功能特色
   - testing: 測試數據
   - comparison: 競品比較

2. 系統會自動生成：
   - 吸引人的標題
   - 產品介紹概述
   - 功能特色分析
   - 完整測試報告
   - 競品比較分析
   - 最終使用建議

使用示例：
(product-review-composer 
  '(:name \"Raycast\"
    :type \"Mac Launcher\"
    :version \"1.0\")
  '((核心功能 . \"快速啟動\")
    (特色功能 . \"擴充系統\")
    (使用體驗 . \"界面設計\"))
  '((性能測試 . 結果)
    (功能測試 . 數據)
    (穩定性 . 評估))
  '((競品A . 比較)
    (競品B . 分析)
    (競品C . 評估)))

輸出將包含：
- 完整評測架構
- 詳細測試數據
- 競品比較分析
- 使用者建議
"))

;; 使用方式：
;; 1. 執行 (usage-guide) 查看說明
;; 2. 調用 (product-review-composer 產品 '功能 '測試 '競品)