#|
#|ASD|#                (:file "defpattern"                :depends-on ("kaavio"
#|ASD|#                                                                "constants"
#|ASD|#                                                                "definition"
#|ASD|#                                                                "layer-manager"
#|ASD|#                                                                "dictionary"
#|ASD|#                                                                "point"
#|ASD|#                                                                "canvas"
#|ASD|#                                                                "font-info"
#|ASD|#                                                                "stroke-info"
#|ASD|#                                                                "writer"))
#|EXPORT|#                ;defpattern.lisp
 |#

(in-package :kaavio)


;;-------------------------------------------------------------------------------
;;
;; class pattern-definition
;;
;;-------------------------------------------------------------------------------
#|
#|EXPORT|#                :pattern-definition
 |#
(defclass pattern-definition (definition)
  ((data           :initform  "" :initarg :data)            ; string
   (x              :initform   0 :initarg :x)               ; number
   (y              :initform   0 :initarg :y)               ; number
   (width          :initform   0 :initarg :width)           ; number
   (height         :initform   0 :initarg :height)          ; number
   (href           :initform nil :initarg :href)            ; keyword
   (units          :initform nil :initarg :units)           ; keyword
   (content-units  :initform nil :initarg :content-units)   ; keyword
   (view-box       :initform nil :initarg :view-box)        ; list
   (transform      :initform nil :initarg :transform)))     ; string


(defmethod check ((ptn pattern-definition) canvas dict)
  (declare (ignore canvas dict))
  (with-slots (data x y width height href units
               content-units view-box transform) ptn
    (check-member data   :nullable nil :types string)
    (check-member x      :nullable   t :types number)
    (check-member y      :nullable   t :types number)
    (check-member width  :nullable   t :types (or number string))
    (check-member height :nullable   t :types (or number string))
    (check-member href   :nullable   t :types keyword)
    (check-member units  :nullable   t :types keyword)
    (when units
      (check-keywords units :userSpaceOnUse :objectBoundingBox))
    (check-member content-units :nullable t :types keyword)
    (when content-units
      (check-keywords content-units :userSpaceOnUse :objectBoundingBox))
    (check-member view-box  :nullable t :types list)
    (check-member transform :nullable t :types string))
  ;; this method must call super class' one.
  (call-next-method))

(defmethod entity-composition-p ((ptn pattern-definition))
  (declare (ignore ptn))
  t)

(defmethod pre-draw ((ptn pattern-definition) writer)
  (when (entity-composition-p ptn)
    (with-slots (id x y width height href units
                 content-units view-box transform) ptn
      (when id
        (labels ((units-tag (kwd)
                   (if (eq kwd :userSpaceOnUse)
                       "userSpaceOnUse"
                       "objectBoundingBox")))
          (writer-write writer
                        "<pattern "
                            "id='" id "' "
                            (write-when x      "x='"      it "' ")
                            (write-when y      "y='"      it "' ")
                            (write-when width  "width='"  it "' ")
                            (write-when height "height='" it "' ")
                            (write-when href   "xlink:href='#" it "' ")
                            (write-when units
                                "patternUnits='" (units-tag it) "' ")
                            (write-when content-units
                                "patternContentUnits='" (units-tag it) "' ")
                            (write-when view-box
                                "viewBox='" (format nil "~{ ~A~}" it) "' ")
                            (write-when transform
                                "patternTransform='" it "' ")
                            ">"))
        (writer-incr-level writer)))))

(defmethod draw-entity ((ptn pattern-definition) writer)
  (with-slots (id data width height) ptn
    (writer-write writer "<defs>")
    (writer-incr-level writer)
    (pre-draw ptn writer)
    ; data を 改行コードで区切ってリストにして、順番に出力
    (dolist (line (string/split data #\newline))
      (writer-write writer line))
    (post-draw ptn writer)
    (writer-decr-level writer)
    (writer-write writer "</defs>"))
  nil)

(defmethod post-draw ((ptn pattern-definition) writer)
  (when (entity-composition-p ptn)
    (when (slot-value ptn 'id)
      (writer-decr-level writer)
      (writer-write writer "</pattern>"))))


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro defpattern
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{defpattern}} (id ${KEY} x y width height href units content-units view-box transform) ${BODY} body
;;
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　defpattern マクロはパターンを定義します。パターンの詳細は「[](#パターン)」を参照してください。
;;説明不足ですが、基本的に SVG 規格に沿っていますので必要に応じて書籍や規格にあたってください。
;;マクロシグネチャは以下の通りです。なお、現在、preserveAspectRatio 属性には対応していません。
;;将来対応する可能性はありますが、未確定です。
;;
;;${BLANK_PARAGRAPH}
;;
;;Table. defpattern マクロのパラメータ
;;| parameter       | description                               |
;;|:================|:------------------------------------------|
;;| `id`            | ID をキーワードで指定します。              |
;;| `x`             |   |
;;| `y`             |   |
;;| `width`         | 幅を数値で指定します。                     |
;;| `height`        | 高さを数値で指定します。                   |
;;| `href`          | 高さを数値で指定します。                   |
;;| `units`         | patternUnits 属性を `:userSpaceOnUse` または `:objectBoundingBox` から<br> \
;;選択します。 |
;;| `content-units` | patternContentUnits 属性を `:userSpaceOnUse` または `:objectBoundingBox` <br> \
;;から選択します。 |
;;| `view-box`      | viewBox 属性を 4 つの数値からなるリストで指定します。 |
;;| `transform`     | patternTtransform 属性を指定する場合、文字列で指定します。  |
;;
;;
;;${BLANK_PARAGRAPH}
;;
;;${NO_SEE_ALSO}
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :defpattern
 |#
(defmacro defpattern ((id &key x y width height href units
                          content-units view-box transform) &rest body)
  (let ((g-layer-mgr (gensym "LAYER-MGR"))
        (g-writer    (gensym "WRITER"))
        (g-entity    (gensym "ENTITY"))
        (g-entities  (gensym "ENTITIES"))
        (g-dict      (gensym "DICT")))
    `(let ((data (let ((,g-layer-mgr (layer-create-manager))
                       (,g-entities  nil)
                       (,g-dict      (dict-create *default-history-count*))
                       (canvas       (make-canvas (make-point 0 0 :absolute) ,width ,height)))
                   (declare (special canvas))
                   (labels ((layer (name &optional (display :inline))
                              (layer-register ,g-layer-mgr name display))
                            (register-entity (,g-entity)
                              (unless (typep ,g-entity 'entity)
                                (throw-exception "Can't register ~A to dictionary : NOT entity." ,g-entity))
                              (when (or (typep ,g-entity 'use)
                                        (typep ,g-entity 'definition))
                                (throw-exception "Can't register ~A to dictionary in defpattern." ,g-entity))
                              (push ,g-entity ,g-entities)
                              (check ,g-entity canvas ,g-dict)
                              (dict-register ,g-dict ,g-entity)))
                     (declare (ignorable #'layer #'register-entity))
                     (let ((*default-font*   (or *default-font*   (make-font)))
                           (*default-fill*   (or *default-fill*   (make-fill)))
                           (*default-stroke* (or *default-stroke* (make-stroke))))
                       (with-dictionary ,g-dict
                         ,@body)))
                   ;; stable sort by priority of layer
                   (setf ,g-entities
                         (stable-sort (nreverse ,g-entities)
                                      (lambda (e1 e2)
                                        (< (layer-get-priority ,g-layer-mgr (slot-value e1 'layer))
                                           (layer-get-priority ,g-layer-mgr (slot-value e2 'layer))))))
                   (let ((,g-writer (create-svg-writer)))
                     (dolist (,g-entity ,g-entities)
                       (layer-change ,g-layer-mgr (slot-value ,g-entity 'layer) ,g-writer)
                       (write-header ,g-entity ,g-writer)
                       (draw-entity  ,g-entity ,g-writer))
                     (layer-change ,g-layer-mgr nil ,g-writer)
                     (writer-close ,g-writer)))))
       (register-entity (make-instance 'kaavio::pattern-definition
                                       :id ,id :data data :x ,x :y ,y
                                       :width  ,width :height ,height
                                       :href ,href :units ,units
                                       :content-units ,content-units
                                       :view-box ,view-box :transform ,transform)))))
                                       
