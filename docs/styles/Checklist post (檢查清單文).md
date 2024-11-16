;; 作者：Claude
;; 版本：1.0
;; 模型：claude sonnet
;; 用途：檢查清單文章生成器
;; Checklist post (檢查清單文，例如《這 50 家台北必吃餐廳，你都吃過了嗎？》)

(defun checklist-post-composer (topic items criteria categories)
  "主函數：生成檢查清單文章"
  (let* ((introduction (create-introduction topic criteria))
         (categorized-items (categorize-items items categories))
         (item-details (expand-items items))
         (highlights (extract-highlights items))
         (statistics (generate-statistics items))
         (sharing-elements (create-sharing-elements topic items)))
    (format-checklist-post introduction categorized-items item-details 
                          highlights statistics sharing-elements)))

(defun create-introduction (topic criteria)
  "建立開場介紹"
  (list
   (hook-statement topic)
   (selection-criteria criteria)
   (list-significance topic)
   (usage-guide topic)))

(defun categorize-items (items categories)
  "項目分類"
  (list
   (group-by-category items categories)
   (category-descriptions categories)
   (category-highlights items categories)
   (cross-category-patterns items)))

(defun expand-items (items)
  "展開項目細節"
  (list
   (basic-information items)
   (unique-features items)
   (practical-tips items)
   (expert-recommendations items)))

(defun extract-highlights (items)
  "提取亮點"
  (list
   (must-try-items items)
   (hidden-gems items)
   (trending-items items)
   (classic-favorites items)))

(defun generate-statistics (items)
  "生成統計資料"
  (list
   (numerical-breakdown items)
   (distribution-analysis items)
   (popularity-metrics items)
   (comparative-data items)))

(defun create-sharing-elements (topic items)
  "建立分享元素"
  (list
   (social-hooks topic)
   (sharing-prompts items)
   (challenge-elements topic)
   (community-engagement topic)))

(defun format-checklist-post 
    (intro categories details highlights stats sharing)
  "格式化檢查清單文"
  
  (format t "~%✨ ~A~%" 
          (format-title intro))
  
  (format t "~%📝 清單說明~%~A" 
          (format-introduction intro))
  
  (format t "~%🎯 打卡重點~%~{- ~A~%~}" 
          (format-highlights highlights))
  
  (dolist (category categories)
    (format t "~%~A~%~{~A~%~}" 
            (format-category-title category)
            (format-category-items category)))
  
  (format t "~%📊 統計分析~%~{~A~%~}" 
          (format-statistics stats))
  
  (format t "~%💡 實用建議~%~{- ~A~%~}" 
          (format-tips details))
  
  (format t "~%🤝 分享挑戰~%~A" 
          (format-sharing sharing)))

;; 輔助函數
(defun hook-statement (topic)
  "開場鉤子")

(defun selection-criteria (criteria)
  "選擇標準")

(defun list-significance (topic)
  "清單重要性")

(defun usage-guide (topic)
  "使用指南")

(defun group-by-category (items categories)
  "依類別分組")

(defun category-descriptions (categories)
  "類別描述")

(defun category-highlights (items categories)
  "類別亮點")

(defun cross-category-patterns (items)
  "跨類別模式")

(defun basic-information (items)
  "基本信息")

(defun unique-features (items)
  "特色特點")

(defun practical-tips (items)
  "實用建議")

(defun expert-recommendations (items)
  "專家推薦")

(defun must-try-items (items)
  "必試項目")

(defun hidden-gems (items)
  "隱藏寶藏")

(defun trending-items (items)
  "趨勢項目")

(defun classic-favorites (items)
  "經典最愛")

(defun numerical-breakdown (items)
  "數據分析")

(defun distribution-analysis (items)
  "分佈分析")

(defun popularity-metrics (items)
  "熱門指標")

(defun comparative-data (items)
  "比較數據")

(defun social-hooks (topic)
  "社交鉤子")

(defun sharing-prompts (items)
  "分享提示")

(defun challenge-elements (topic)
  "挑戰元素")

(defun community-engagement (topic)
  "社群互動")

(defun format-title (intro)
  "格式化標題")

(defun format-introduction (intro)
  "格式化介紹")

(defun format-highlights (highlights)
  "格式化亮點")

(defun format-category-title (category)
  "格式化類別標題")

(defun format-category-items (category)
  "格式化類別項目")

(defun format-statistics (stats)
  "格式化統計")

(defun format-tips (details)
  "格式化建議")

(defun format-sharing (sharing)
  "格式化分享")

(defun usage-guide ()
  "使用指南"
  (format t "
檢查清單文章生成器使用指南：

1. 輸入四個參數：
   - topic: 清單主題
   - items: 項目列表
   - criteria: 選擇標準
   - categories: 分類方式

2. 系統會自動生成：
   - 引人注目的標題
   - 清單說明
   - 分類項目
   - 重點推薦
   - 統計分析
   - 實用建議
   - 社群互動

使用示例：
(checklist-post-composer 
  \"台北必吃餐廳50選\"
  '((名稱 . 餐廳清單)
    (特色 . 特色說明)
    (位置 . 地點資訊))
  '((評選 . \"在地推薦\")
    (標準 . \"獨特性\")
    (驗證 . \"實地走訪\"))
  '((區域 . 行政區)
    (風格 . 料理類型)
    (預算 . 消費區間)))

輸出將包含：
- 完整的清單架構
- 分類整理的內容
- 實用的補充資訊
- 互動分享元素
- 統計分析數據
"))

;; 使用方式：
;; 1. 執行 (usage-guide) 查看說明
;; 2. 調用 (checklist-post-composer 主題 '項目 '標準 '分類) 開始生成