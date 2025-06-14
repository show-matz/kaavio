#|
#|ASD|#                (:file "brace"                     :depends-on ("kaavio"
#|ASD|#                                                                "constants"
#|ASD|#                                                                "clipping"
#|ASD|#                                                                "path"))
#|EXPORT|#                ;brace.lisp
 |#

(in-package :kaavio)

#|
#|EXPORT|#                :*default-brace-font*
#|EXPORT|#                :*default-brace-stroke*
#|EXPORT|#                :*default-brace-filter*
#|EXPORT|#                :*default-brace-layer*
 |#
(defparameter *default-brace-font*         nil)
(defparameter *default-brace-stroke*       nil)
(defparameter *default-brace-filter*       nil)
(defparameter *default-brace-layer*        nil)

(defun brace-make-path-left (w h r point)
  (let ((r (or r (/ w 3)))
        (point (or point (/ h 2))))
    (let ((rx (/ w 2))
          (ry (min r (/ point 2) (/ (- h point) 2))))
      (values point
              `((:move-to (0 0))
                (:arc-to ,rx ,ry 1 0 1 (,rx ,ry))
                (:line-to (,rx ,(- point ry)))
                (:arc-to ,rx ,ry 0 0 0 (,w ,point))
                (:arc-to ,rx ,ry 0 0 0 (,rx ,(+ point ry)))
                (:line-to (,rx ,(- h ry)))
                (:arc-to ,rx ,ry 1 0 1 (0 ,h)))))))

(defun brace-make-path-right (w h r point)
  (let ((r (or r (/ w 3)))
        (point (or point (/ h 2))))
    (let ((rx (/ w 2))
          (ry (min r (/ point 2) (/ (- h point) 2))))
      (values point
              `((:move-to (,w 0))
                (:arc-to ,rx ,ry 0 0 0 (,rx ,ry))
                (:line-to (,rx ,(- point ry)))
                (:arc-to ,rx ,ry 1 0 1 (0 ,point))
                (:arc-to ,rx ,ry 1 0 1 (,rx ,(+ point ry)))
                (:line-to (,rx ,(- h ry)))
                (:arc-to ,rx ,ry 0 0 0 (,w ,h)))))))

(defun brace-make-path-upper (w h r point)
  (let ((r (or r (/ h 3)))
        (point (or point (/ w 2))))
    (let ((ry (/ h 2))
          (rx (min r (/ point 2) (/ (- w point) 2))))
      (values point
              `((:move-to (0 0))
                (:arc-to ,rx ,ry 0 0 0 (,rx ,ry))
                (:line-to (,(- point rx) ,ry))
                (:arc-to ,rx ,ry 1 0 1 (,point ,h))
                (:arc-to ,rx ,ry 1 0 1 (,(+ point rx) ,ry))
                (:line-to (,(- w rx) ,ry))
                (:arc-to ,rx ,ry 0 0 0 (,w 0)))))))

(defun brace-make-path-bottom (w h r point)
  (let ((r (or r (/ h 3)))
        (point (or point (/ w 2))))
    (let ((ry (/ h 2))
          (rx (min r (/ point 2) (/ (- w point) 2))))
      (values point
              `((:move-to (0 ,h))
                (:arc-to ,rx ,ry 1 0 1 (,rx ,ry))
                (:line-to (,(- point rx) ,ry))
                (:arc-to ,rx ,ry 0 0 0 (,point 0))
                (:arc-to ,rx ,ry 0 0 0 (,(+ point rx) ,ry))
                (:line-to (,(- w rx) ,ry))
                (:arc-to ,rx ,ry 1 0 1 (,w ,h)))))))

;;------------------------------------------------------------------------------
;;
;; class brace
;;
;;------------------------------------------------------------------------------
(defclass brace (group)
  ((direction   :initform nil :initarg :direction)   ; keyword(:upper,:bottom,:left,:right)
   (r           :initform nil :initarg :r)           ; number
   (point       :initform nil :initarg :point)       ; number
   (text        :initform nil :initarg :text)        ; (or keyword string)
   (font        :initform nil :initarg :font)        ; (or nil font-info)
   (stroke      :initform nil :initarg :stroke)      ; (or nil stroke-info)
   (clip-path   :initform nil :initarg :clip-path)   ; (or nil symbol)
   (filter      :initform nil :initarg :filter)))    ; (or nil keyword)
  
(defmethod initialize-instance :after ((brc brace) &rest initargs)
  (declare (ignore initargs))
  (with-slots (font stroke filter layer) brc
    (setf font   (make-font   (or font   *default-brace-font*   *default-font*)))
    (setf stroke (make-stroke (or stroke *default-brace-stroke* *default-stroke* :none)))
    (setf filter (if (eq filter :none)
                     nil
                     (or filter *default-brace-filter* *default-filter*)))
    (setf layer  (if (eq layer :none)
                     nil
                     (or layer *default-brace-layer* *default-layer*))))
  brc)

(defmethod check ((brc brace) canvas dict)
  (with-slots (direction r point text font stroke filter clip-path) brc
    (check-keywords direction :upper :bottom :left :right)
    (check-member   r       :nullable t :types number)
    (check-member   point   :nullable t :types number)
    (check-member   text    :nullable t :types (or keyword string))
    (check-object   font    canvas dict :nullable   t :class   font-info)
    (check-object   stroke  canvas dict :nullable nil :class stroke-info)
    (check-member clip-path :nullable   t :types symbol)
    (check-member filter    :nullable   t :types keyword)
    (when text
      (setf text (fix-name text))))
  ;; this method must call super class' one.
  (call-next-method))


;; override of group::draw-group
(defmethod draw-group ((brc brace) writer)
  (let ((canvas (group-get-canvas brc)))
    (let ((width  (canvas-width  canvas))
          (height (canvas-height canvas)))
      (macrolet ((register-entity (entity)
                   `(check-and-draw-local-entity ,entity canvas writer)))
        (with-slots (direction r point text font stroke filter clip-path) brc
          (multiple-value-bind (point data)
              (case direction
                ((:upper)  (brace-make-path-upper  width height r point))
                ((:bottom) (brace-make-path-bottom width height r point))
                ((:left)   (brace-make-path-left   width height r point))
                ((:right)  (brace-make-path-right  width height r point)))
            (let ((*current-clip-path* clip-path))
              ;; draw brace
              (path data :stroke stroke :fill :none :filter filter)
              ;; draw text
              (when text
                (with-slots (size) font
                  (multiple-value-bind (align pos)
                      (case direction
                        ((:upper)  (values :center `(,point ,(+ height size 5))))
                        ((:bottom) (values :center `(,point -5)))
                        ((:left)   (values :left   `(,(+ width 5) ,(+ point (/ size 2)))))
                        ((:right)  (values :right  `(-5 ,(+ point (/ size 2))))))
                    (text pos text :align align :font font))))))))))
  nil)


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro brace
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{brace}} position direction width height ${KEY} pivot r point text font stroke layer filter id
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `direction` ---- 波括弧の向きを `:upper :bottom :left :right` のいずれかで指定します。
;;* `width` ---- 幅を数値で指定します。
;;* `height` ---- 高さを数値で指定します。
;;* `pivot` ---- 基準点が波括弧のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `r` ---- 曲線部分の半径を数値で指定します。
;;* `point` ---- 中央の「つまみ」の曲線部分の端からの距離を数値で指定します。これは水平の波括弧の場合は左から、垂直の波括弧の場合は上からの距離です。
;;* `text` ---- 描画するテキストを文字列で指定します。改行は "~%" で表現します。
;;* `font` ---- フォントを指定します。
;;* `stroke` ----  外枠を描画する線を指定します。
;;* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
;;* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
;;* `id` ---- ID を付与したい場合、その名前をキーワードで指定します。
;;
;;${DESCRIPTION}
;;
;;　波括弧を描画します。複数の波括弧でスタイルを統一したい場合、with-brace-options マクロを
;;使うことができます。
;;
;;${SEE_ALSO}
;;
;;* 波括弧
;;* with-brace-options マクロ
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :brace
 |#
(defmacro brace (position direction width height
                        &key pivot r point text font stroke layer filter id)
  `(register-entity (make-instance 'kaavio:brace
                                   :position ,position :pivot ,pivot
                                   :direction ,direction :width ,width :height ,height
                                   :r ,r :point ,point
                                   :text ,text :font ,font :stroke ,stroke
                                   :clip-path *current-clip-path*
                                   :link nil :layer ,layer :filter ,filter :id ,id)))


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro with-brace-options
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{with-brace-options}} (${KEY} font stroke filter layer) ${BODY} body
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　brace マクロで描画される波括弧のデフォルトオプションを変更します。キーワードパラメータ
;;群の説明は brace マクロを参照してください。
;;
;;${SEE_ALSO}
;;
;;* 波括弧
;;* brace マクロ
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :with-brace-options
 |#
(defmacro with-brace-options ((&key (font   nil font-p)
                                    (stroke nil stroke-p)
                                    (filter nil filter-p)
                                    (layer  nil layer-p)) &rest body)
  (let ((bindings nil))
    (labels ((impl (arg-p binding)
               (when arg-p (push binding bindings))))
      (impl font-p   `(*default-brace-font*   (make-font2   *default-brace-font*   ,font)))
      (impl stroke-p `(*default-brace-stroke* (make-stroke2 *default-brace-stroke* ,stroke)))
      (impl filter-p `(*default-brace-filter* ,filter))
      (impl layer-p  `(*default-brace-layer*  ,layer)))
    `(let ,(nreverse bindings)
       ,@body)))
