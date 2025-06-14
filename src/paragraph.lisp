#|
#|ASD|#                (:file "paragraph"                 :depends-on ("kaavio"
#|ASD|#                                                                "constants"
#|ASD|#                                                                "text"
#|ASD|#                                                                "shape"
#|ASD|#                                                                "font-info"
#|ASD|#                                                                "link-info"
#|ASD|#                                                                "point"
#|ASD|#                                                                "clipping"
#|ASD|#                                                                "writer"))
#|EXPORT|#                ;paragraph.lisp
 |#


(in-package :kaavio)

(defun caluculate-paragraph-shapesize (font text)
  (with-slots (size
               (spice width-spice)
               (spacing line-spacing)) font
    (let ((line-count (length text)))
      (values (apply #'max (mapcar (lambda (line)
                                     (* (length line) size spice)) text))
              (+ (* size line-count)
                 (* spacing (1- line-count)))))))

;;------------------------------------------------------------------------------
;;
;; class paragraph
;;
;;------------------------------------------------------------------------------
(defclass paragraph (shape)
  ((position  :initform nil :initarg :position)    ; point
   (text      :initform nil :initarg :text)        ; string -> list
   (align     :initform nil :initarg :align)       ; keyword
   (valign    :initform nil :initarg :valign)      ; keyword
   (font      :initform nil :initarg :font)        ; (or nil font-info)
   (width     :initform nil :initarg :width)       ; number
   (height    :initform nil :initarg :height)      ; number
   (clip-path :initform nil :initarg :clip-path))) ; (or nil symbol)


(defmethod initialize-instance :after ((shp paragraph) &rest initargs)
  (declare (ignore initargs))
  (with-slots (align valign font layer) shp
    (setf align  (or align  *default-paragraph-align*))
    (setf valign (or valign *default-paragraph-valign*))
    (setf font   (make-font (or font *default-font*)))
    (setf layer  (if (eq layer :none)
                     nil
                     (or layer *default-paragraph-layer* *default-layer*))))
  shp)

(defmethod check ((shp paragraph) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (position text align valign font width height layer clip-path) shp
    (check-member   text   :nullable nil :types string)
    (check-member   align  :nullable nil :types keyword)
    (check-member   valign :nullable nil :types keyword)
    (check-object   font   canvas dict :nullable nil :class font-info)
    (check-keywords align  :left :center :right)
    (check-keywords valign :top  :center :bottom)
    (check-member clip-path :nullable  t :types symbol)
    (setf text (string/split (fix-name text) #\newline))
    (setf position (canvas-fix-point canvas position))
    (multiple-value-bind (w h) (caluculate-paragraph-shapesize font text)
      (setf width  w)
      (setf height h))
    (setf layer  (if (eq layer :none)
                     nil
                     (or layer *default-layer*))))
  nil)

(defmethod attribute-width ((shp paragraph))
  (slot-value shp 'width))

(defmethod attribute-height ((shp paragraph))
  (slot-value shp 'height))

(defmethod attribute-center ((shp paragraph))
  (with-slots (position width height align valign) shp
    (point/xy+ position
               (ecase align
                 ((:left)   (/ width 2))
                 ((:center) 0)
                 ((:right)  (- (/ width 2))))
               (ecase valign
                 ((:top)    (/ height 2))
                 ((:center) 0)
                 ((:bottom) (- (/ height 2)))))))

(defmethod entity-composition-p ((shp paragraph))
  (with-slots (text) shp
    (or (< 1 (length text))
        (call-next-method))))
  
(defmethod pre-draw ((shp paragraph) writer)
  (call-next-method)
  (with-slots (text) shp
    (when (< 1 (length text))
      (with-slots (align font clip-path) shp
        (writer-write writer "<g "
                      "text-anchor='" (ecase align
                                        ((:left)   "start")
                                        ((:center) "middle")
                                        ((:right)  "end")) "' "
                      (to-property-strings font)
                      (write-when clip-path "clip-path='url(#" it ")' ")
                      ">"))
      (writer-incr-level writer))))

(defmethod post-draw ((shp paragraph) writer)
  (with-slots (text) shp
    (when (< 1 (length text))
      (writer-decr-level writer)
      (writer-write writer "</g>")))
  (call-next-method))

;;MEMO : use impelementation of shape...
;;(defmethod shape-get-subcanvas ((shp rectangle)) ...)

(defmethod draw-entity ((shp paragraph) writer)
  (with-slots (position align font text id clip-path) shp
    (let ((x (point-x position))
          (y (point-y (attribute-top shp))))
      (with-slots ((font-size size) line-spacing) font
        (pre-draw shp writer)
        (if (= 1 (length text))
            (write-text-tag x (+ y font-size) (car text) writer
                            :id id :align align
                            :font (to-property-strings font) :clip-path clip-path)
            (dolist (line text)
              (incf y font-size)
              (write-text-tag x y line writer)
              (incf y line-spacing)))
        (post-draw shp writer))))
  nil)
                      


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro paragraph
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{paragraph}} position text ${KEY} align valign rotate font link layer id
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `position` ---- 描画するパラグラフの基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `text` ---- 描画するパラグラフを文字列で指定します。改行は "~%" で表現します。
;;* `align` ---- パラグラフの水平方向のアライメントを `:left :center :right` のいずれかで指定します。
;;* `valign` ---- パラグラフの垂直方向のアライメントを `:top :center :bottom` のいずれかで指定します。
;;* `font` ---- フォントを指定します。
;;* `link` ---- リンクにする場合、リンク先を指定します。
;;* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
;;* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します
;;
;;${DESCRIPTION}
;;
;;　複数行に渡るパラグラフを描画します。単一行のテキストであれば text マクロを、テキストボックス
;;を使いたい場合は textbox マクロを使うことができます。
;;
;;${SEE_ALSO}
;;
;;* パラグラフ
;;* テキスト
;;* テキストボックス
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :paragraph
 |#
(defmacro paragraph (position text
                     &key align valign rotate font link layer id)
  `(register-entity (make-instance 'kaavio:paragraph
                                   :position ,position :text ,text
                                   :align ,align :valign ,valign :rotate ,rotate
                                   :clip-path *current-clip-path*
                                   :font (or ,font *default-paragraph-font*)
                                   :link ,link :layer ,layer :id ,id)))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro with-paragraph-options
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{with-paragraph-options}} (${KEY} align valign font layer) ${BODY} body
;;
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　paragraph マクロで描画されるドキュメントのデフォルトオプションを変更します。キーワード
;;パラメータ群の説明は paragraph マクロを参照してください。
;;
;;${SEE_ALSO}
;;
;;* ドキュメント
;;* paragraph マクロ
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :with-paragraph-options
 |#
(defmacro with-paragraph-options ((&key (align  nil align-p)
                                        (valign nil valign-p)
                                        (font   nil font-p)
                                        (layer  nil layer-p)) &rest body)
  (let ((bindings nil))
    (labels ((impl (arg-p binding)
               (when arg-p (push binding bindings))))
      (impl align-p  `(*default-paragraph-align*  ,align))
      (impl valign-p `(*default-paragraph-valign* ,valign))
      (impl font-p   `(*default-paragraph-font*   (make-font2 *default-paragraph-font* ,font)))
      (impl layer-p  `(*default-paragraph-layer*  ,layer)))
    `(let ,(nreverse bindings)
       ,@body)))
