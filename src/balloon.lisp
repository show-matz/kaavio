#|
#|ASD|#                (:file "balloon"                   :depends-on ("kaavio"
#|ASD|#                                                                "constants"
#|ASD|#                                                                "arc"
#|ASD|#                                                                "polygon"
#|ASD|#                                                                "filter"
#|ASD|#                                                                "text-shape"))
#|EXPORT|#                ;balloon.lisp
 |#

(in-package :kaavio)

#|
#|EXPORT|#                :*default-balloon-round*
#|EXPORT|#                :*default-balloon-align*
#|EXPORT|#                :*default-balloon-valign*
#|EXPORT|#                :*default-balloon-margin*
#|EXPORT|#                :*default-balloon-font*
#|EXPORT|#                :*default-balloon-fill*
#|EXPORT|#                :*default-balloon-stroke*
#|EXPORT|#                :*default-balloon-filter*
#|EXPORT|#                :*default-balloon-layer*
 |#
(defparameter *default-balloon-round*  10)
(defparameter *default-balloon-align*  :center)
(defparameter *default-balloon-valign* :center)
(defparameter *default-balloon-margin* 10)
(defparameter *default-balloon-font*   nil)
(defparameter *default-balloon-fill*   nil)
(defparameter *default-balloon-stroke* nil)
(defparameter *default-balloon-filter* nil)
(defparameter *default-balloon-layer*  nil)



(defun balloon-anchor-priority (w h pt pos)
  (with-point (x y) pt
    (let ((w2 (/ w 2))
          (h2 (/ h 2)))
      (cond
        ((eq pos    :top) (if (<=  x w2)  5 15))
        ((eq pos  :right) (if (<=  y h2) 25 35))
        ((eq pos :bottom) (if (<  w2  x) 45 55))
        ((eq pos   :left) (if (<  h2  y) 65 75))
        (t nil)))))

(defun balloon-make-path (w h r anchor pt pos)
  (let ((points nil)
        (pri (balloon-anchor-priority w h pt pos)))
    ;; pathの先頭末尾を追加
    (push `( 0 (:move-to (,r 0))) points)
    (push '( 1 :absolute) points)
    (push '(99 :close-path) points)
    ;; round 指定があるなら角の丸い部分を追加
    (when (< 0 r)
      (push `(20 (:arc-to ,r ,r 1 0 1 (,w       ,r      ))) points)
      (push `(40 (:arc-to ,r ,r 1 0 1 (,(- w r) ,h      ))) points)
      (push `(60 (:arc-to ,r ,r 1 0 1 (0        ,(- h r)))) points)
      (push `(80 (:arc-to ,r ,r 1 0 1 (,r       0       ))) points))
    ;; upper side
    (when (= pri 5)
      (push `(5 (:line-to ,anchor)     ) points)
      (push `(6 (:line-to (,(/ w 3) 0))) points))
    (if (/= pri 15)
        (push `(10 (:line-to (,(- w r) 0))) points)
        (progn
          (push `(10 (:line-to (,(* 2 (/ w 3)) 0))) points)
          (push `(15 (:line-to ,anchor)     ) points)
          (push `(16 (:line-to (,(- w r) 0))) points)))
    ;; right side
    (when (= pri 25)
      (push `(25 (:line-to ,anchor)      ) points)
      (push `(26 (:line-to (,w ,(/ h 2)))) points))
    (if (/= pri 35)
        (push `(30 (:line-to (,w       ,(- h r)))) points)
        (progn
          (push `(30 (:line-to (,w ,(/ h 2)))) points)
          (push `(35 (:line-to ,anchor)      ) points)
          (push `(36 (:line-to (,w ,(- h r)))) points)))
    ;; bottom side
    (when (= pri 45)
      (push `(45 (:line-to ,anchor)      ) points)
      (push `(46 (:line-to (,(* 2 (/ w 3)) ,h))) points))
    (if (/= pri 55)
        (push `(50 (:line-to (,r ,h))) points)
        (progn
          (push `(50 (:line-to (,(/ w 3) ,h))) points)
          (push `(55 (:line-to ,anchor)      ) points)
          (push `(56 (:line-to (,r       ,h))) points)))
    ;; left side
    (when (= pri 65)
      (push `(65 (:line-to ,anchor)     ) points)
      (push `(66 (:line-to (0 ,(/ h 2)))) points))
    (if (/= pri 75)
        (push `(70 (:line-to (0 ,r))) points)
        (progn
          (push `(70 (:line-to (0 ,(/ h 2)))) points)
          (push `(75 (:line-to ,anchor)     ) points)
          (push `(76 (:line-to (0 ,r      ))) points)))
    ;; 最後にソートして先頭の優先順位を除去して返す
    (mapcar (lambda (lst) (cadr lst))
            (sort points (lambda (e1 e2)
                           (< (car e1) (car e2)))))))


;;------------------------------------------------------------------------------
;;
;; class balloon
;;
;;------------------------------------------------------------------------------
(defclass balloon (text-shape)
  ((round   :initform nil :initarg :round)    ; number
   (anchor  :initform nil :initarg :anchor)   ; point
   (filter  :initform nil :initarg :filter))) ; (or nil keyword)
  
(defmethod initialize-instance :after ((bln balloon) &rest initargs)
  (declare (ignore initargs))
  (with-slots (layer filter) bln
    (setf layer  (if (eq layer :none)
                     nil
                     (or layer *default-balloon-layer* *default-layer*)))
    (setf filter (if (eq filter :none)
                     nil
                     (or filter *default-balloon-filter* *default-filter*))))
  bln)
   
(defmethod check ((bln balloon) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (filter) bln
    (check-member filter    :nullable   t :types keyword))
  nil)

;; override of group::draw-group
(defmethod draw-group ((box balloon) writer)
  (let* ((canvas (group-get-canvas box))
         (width  (canvas-width  canvas))
         (height (canvas-height canvas)))
    (macrolet ((register-entity (entity)
                 `(check-and-draw-local-entity ,entity canvas writer)))
      (with-slots (round anchor fill stroke filter) box
        (labels ((abs2rel (pt)
                   (let ((topleft (canvas-topleft canvas)))
                     (make-point (- (point-x pt) (point-x topleft))
                                 (- (point-y pt) (point-y topleft))))))
          ;; draw box
          (multiple-value-bind (pt pos)
              (rectangle-connect-point-C (attribute-center box) width height anchor)
            (let ((points (balloon-make-path width height (or round 0)
                                            (abs2rel anchor) (abs2rel pt) pos)))
              (path points :fill fill :stroke stroke :filter filter)))))))
  ;; draw text
  (call-next-method))

;; no override.
;(defmethod text-shape-calc-size ((box balloon))
;  (call-next-method))

;; no override.
;(defmethod text-shape-paragraph-area ((box balloon))
;  (call-next-method))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro balloon
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{balloon}} position text anchor ${KEY} pivot width height round align valign margin font fill stroke link rotate layer id filter contents
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `text` ---- 内部に描画するテキストを文字列で指定します。改行は "~%" で表現します。
;;* `anchor` ---- 引き出し線の位置を指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `pivot` ---- 基準点が吹き出しのどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `width` ---- 幅を自動決定せず、明示的に指定したい場合に数値で指定します。
;;* `height` ---- 高さを自動決定せず、明示的に指定したい場合に数値で指定します。
;;* `round` ---- 角を丸くしたい場合に、角の半径を数値で指定します。省略した場合のデフォルト値は 0（つまり角を丸くしない）です。
;;* `align` ---- テキストの水平方向のアライメントを `:left :center :right` のいずれかで指定します。省略した場合のデフォルト値は `:center` です。
;;* `valign` ---- テキストの垂直方向のアライメントを `:top :center :bottom` のいずれかで指定します。省略した場合のデフォルト値は `:center` です。
;;* `margin` ---- テキスト描画における「余白」のサイズです。省略した場合のデフォルト値は 10 です。
;;* `font` ---- フォントを指定します。
;;* `fill` ---- 内部の塗り潰しを指定します。
;;* `stroke` ---- 外枠を描画するストロークを指定します
;;* `link` ---- リンクにする場合、リンク先を指定します。
;;* `rotate` ---- 全体を回転させたい場合に、その角度を指定します。
;;* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
;;* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します
;;* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
;;* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。
;;
;;${DESCRIPTION}
;;
;;　吹き出しを描画します。複数の吹き出しでスタイルを統一したい場合、with-balloon-options マクロを
;;使うことができます。
;;
;;${SEE_ALSO}
;;
;;* 吹き出し
;;* with-balloon-options マクロ
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :balloon
 |#
(defmacro balloon (position text anchor &key pivot width height round align valign margin
                                           font fill stroke link rotate layer id filter contents)
  (let ((code `(register-entity (make-instance 'balloon
                                               :anchor ,anchor
                                               :round  (or ,round  *default-balloon-round*)
                                               :position ,position :pivot ,pivot
                                               :width ,width :height ,height
                                               :text ,text
                                               :align  (or ,align  *default-balloon-align*)
                                               :valign (or ,valign *default-balloon-valign*)
                                               :margin (or ,margin *default-balloon-margin*)
                                               :font   (or ,font   *default-balloon-font*)
                                               :fill   (or ,fill   *default-balloon-fill*)
                                               :stroke (or ,stroke *default-balloon-stroke*)
                                               :link ,link :rotate ,rotate
                                               :filter ,filter :layer ,layer :id ,id))))
    (if (null contents)
        code
        (let ((g-obj (gensym "OBJ")))
          `(let* ((,g-obj ,code)
                  (canvas (kaavio:shape-get-subcanvas ,g-obj)))
             (declare (special canvas))
             ,@contents)))))


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro with-balloon-options
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{with-balloon-options}} (${KEY} round align valign margin font fill stroke filter layer) ${BODY} body
;;
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　balloon  マクロで描画される吹き出しのデフォルトオプションを変更します。キーワードパラメータ
;;群の説明は balloon マクロを参照してください。
;;
;;${SEE_ALSO}
;;
;;* 吹き出し
;;* balloon マクロ
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :with-balloon-options
 |#
(defmacro with-balloon-options ((&key round align valign margin
                                      font fill stroke filter layer) &rest body)
  (labels ((impl (params acc)
             (if (null params)
                 acc
                 (let ((value  (car  params))
                       (symbol (cadr params)))
                   (impl (cddr params)
                         (if (null value)
                             acc
                             (push (list symbol value) acc)))))))
    (let ((lst (impl (list round  '*default-balloon-round*
                           align  '*default-balloon-align*
                           valign '*default-balloon-valign*
                           margin '*default-balloon-margin*
                           font   '*default-balloon-font*
                           fill   '*default-balloon-fill*
                           stroke '*default-balloon-stroke*
                           filter '*default-balloon-filter*
                           layer  '*default-balloon-layer*) nil)))
      `(let ,lst
         ,@body))))
