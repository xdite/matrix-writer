;; 作者：Claude
;; 版本：1.0
;; 模型：claude sonnet
;; 用途：故事文章生成器

(defun story-post-composer (story-info story-arc emotional-journey reflections)
  "主函數：生成故事性文章"
  (let* ((title (generate-story-title story-info))
         (hook (create-story-hook story-info))
         (background (set-story-background story-info))
         (narrative (craft-narrative-arc story-arc))
         (emotions (document-emotional-journey emotional-journey))
         (insights (extract-key-insights reflections))
         (conclusion (craft-story-conclusion story-info reflections)))
    (format-story-post title hook background narrative 
                      emotions insights conclusion)))

(defun generate-story-title (story-info)
  "生成故事標題"
  (let ((title-templates
         '("親身經歷：我如何克服~A"
           "從迷惘到突破：我的~A故事"
           "一段真實的~A心路歷程"
           "走過低谷：我與~A的故事"
           "蛻變之路：我的~A成長故事")))
    (format nil
            (nth (random (length title-templates)) title-templates)
            (getf story-info :challenge))))

(defun create-story-hook (story-info)
  "創建故事開場"
  (list
   (opening-scene story-info)
   (central-conflict story-info)
   (emotional-stakes story-info)
   (narrative-promise story-info)))

(defun set-story-background (story-info)
  "設定故事背景"
  (list
   (personal-context story-info)
   (situational-setup story-info)
   (key-characters story-info)
   (initial-mindset story-info)))

(defun craft-narrative-arc (story-arc)
  "打造故事情節"
  (loop for scene in story-arc
        collect
        (list
         :chapter (scene-chapter scene)
         :events (key-events scene)
         :conflicts (scene-conflicts scene)
         :decisions (key-decisions scene)
         :turning-points (scene-turning-points scene))))

(defun document-emotional-journey (emotional-journey)
  "記錄情感歷程"
  (loop for stage in emotional-journey
        collect
        (list
         :phase (emotional-phase stage)
         :feelings (emotional-states stage)
         :struggles (internal-struggles stage)
         :growth (emotional-growth stage)
         :breakthroughs (emotional-breakthroughs stage))))

(defun extract-key-insights (reflections)
  "提取關鍵洞見"
  (list
   (personal-revelations reflections)
   (life-lessons reflections)
   (mindset-changes reflections)
   (value-shifts reflections)))

(defun craft-story-conclusion (story-info reflections)
  "編織故事結論"
  (list
   (transformation-summary story-info reflections)
   (future-outlook story-info reflections)
   (message-to-readers story-info reflections)
   (closing-thoughts story-info reflections)))

(defun format-story-post 
    (title hook background narrative emotions insights conclusion)
  "格式化故事文章"
  
  (format t "~%📖 ~A~%" title)
  
  (format t "~%🎬 故事開場~%~{~A~%~}" hook)
  
  (format t "~%📍 背景鋪陳~%")
  (loop for item in background do
        (format t "~A~%" item))
  
  (format t "~%📚 故事歷程~%")
  (loop for scene in narrative do
        (format t "~%~A：~%" 
                (getf scene :chapter))
        (format t "關鍵事件：~%~{- ~A~%~}" 
                (getf scene :events))
        (format t "面臨衝突：~%~{- ~A~%~}" 
                (getf scene :conflicts))
        (format t "重要決定：~%~{- ~A~%~}" 
                (getf scene :decisions))
        (format t "轉捩點：~%~{- ~A~%~}" 
                (getf scene :turning-points)))
  
  (format t "~%💭 心路歷程~%")
  (loop for stage in emotions do
        (format t "~%~A：~%" 
                (getf stage :phase))
        (format t "情感狀態：~%~{- ~A~%~}" 
                (getf stage :feelings))
        (format t "內心掙扎：~%~{- ~A~%~}" 
                (getf stage :struggles))
        (format t "成長收穫：~%~{- ~A~%~}" 
                (getf stage :growth))
        (format t "突破時刻：~%~{- ~A~%~}" 
                (getf stage :breakthroughs)))
  
  (format t "~%💡 人生感悟~%~{~A~%~}" insights)
  
  (format t "~%🌟 故事結語~%~{~A~%~}" conclusion))

;; 輔助函數
(defun opening-scene (story-info)
  "開場場景")

(defun central-conflict (story-info)
  "核心衝突")

(defun emotional-stakes (story-info)
  "情感賭注")

(defun narrative-promise (story-info)
  "故事承諾")

(defun personal-context (story-info)
  "個人脈絡")

(defun situational-setup (story-info)
  "情境設定")

(defun key-characters (story-info)
  "關鍵角色")

(defun initial-mindset (story-info)
  "初始心態")

(defun scene-chapter (scene)
  "場景章節")

(defun key-events (scene)
  "關鍵事件")

(defun scene-conflicts (scene)
  "場景衝突")

(defun key-decisions (scene)
  "關鍵決定")

(defun scene-turning-points (scene)
  "場景轉折點")

(defun emotional-phase (stage)
  "情感階段")

(defun emotional-states (stage)
  "情感狀態")

(defun internal-struggles (stage)
  "內在掙扎")

(defun emotional-growth (stage)
  "情感成長")

(defun emotional-breakthroughs (stage)
  "情感突破")

(defun personal-revelations (reflections)
  "個人領悟")

(defun life-lessons (reflections)
  "人生教訓")

(defun mindset-changes (reflections)
  "思維轉變")

(defun value-shifts (reflections)
  "價值轉變")

(defun transformation-summary (story-info reflections)
  "轉變總結")

(defun future-outlook (story-info reflections)
  "未來展望")

(defun message-to-readers (story-info reflections)
  "讀者寄語")

(defun closing-thoughts (story-info reflections)
  "結束思考")

(defun usage-guide ()
  "使用指南"
  (format t "
故事文章生成器使用指南：

1. 輸入四個參數：
   - story-info: 故事資訊
   - story-arc: 故事情節
   - emotional-journey: 情感歷程
   - reflections: 反思感悟

2. 系統會自動生成：
   - 故事標題
   - 開場引子
   - 背景鋪陳
   - 情節發展
   - 情感歷程
   - 反思感悟
   - 結語寄語

使用示例：
(story-post-composer 
  '(:challenge \"轉換工作的心理疙瘩\"
    :duration \"半年經歷\"
    :theme \"職涯成長\")
  '((掙扎期 . 事件)
    (行動期 . 過程)
    (突破期 . 轉折))
  '((迷惘 . 情感)
    (恐懼 . 心理)
    (成長 . 蛻變))
  '((領悟 . 思考)
    (學習 . 收穫)
    (展望 . 期待)))

輸出將包含：
- 完整故事架構
- 細膩情感描寫
- 深刻反思感悟
- 真摯讀者分享
"))

;; 使用方式：
;; 1. 執行 (usage-guide) 查看說明
;; 2. 調用 (story-post-composer 故事 '情節 '情感 '反思)