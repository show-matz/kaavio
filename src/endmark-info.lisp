#|
#|ASD|#                (:file "endmark-info"              :depends-on ("kaavio"
#|ASD|#                                                                "constants"
#|ASD|#                                                                "mathutil"
#|ASD|#                                                                "point"
#|ASD|#                                                                "canvas"
#|ASD|#                                                                "dictionary"
#|ASD|#                                                                "fill-info"
#|ASD|#                                                                "stroke-info"
#|ASD|#                                                                "writer"))
#|EXPORT|#                ;endmark-info.lisp
 |#


(in-package :kaavio)


;;------------------------------------------------------------------------------
;;
;; internal functions
;;
;;------------------------------------------------------------------------------
(defun __draw-endmark-arrow (points size stroke fill writer)
  (declare (ignore fill))
  (symbol-macrolet ((ARROW_DEGREE1 210)
                    (ARROW_DEGREE2 150))
    (let* ((pt1 (car points))
           (pt2 (cdr points))
           (x1 (* size (math/cos3 pt1 pt2 ARROW_DEGREE1)))
           (y1 (* size (math/sin3 pt1 pt2 ARROW_DEGREE1)))
           (x2 (* size (math/cos3 pt1 pt2 ARROW_DEGREE2)))
           (y2 (* size (math/sin3 pt1 pt2 ARROW_DEGREE2))))
      (writer-write writer
                    "<polyline "
                    "fill='none' "
                    (when stroke
                      (to-property-strings stroke))
                    "points='"  (+ (point-x pt2) x2) "," (+ (point-y pt2) y2) " "
                                (point-x pt2)        "," (point-y pt2)        " "
                                (+ (point-x pt2) x1) "," (+ (point-y pt2) y1) "' "
                    "/>"))))

(defun __draw-endmark-triangle (points size stroke fill writer)
  (symbol-macrolet ((ARROW_DEGREE1 205)
                    (ARROW_DEGREE2 155))
    (let* ((pt1 (car points))
           (pt2 (cdr points))
           (x1 (* size (math/cos3 pt1 pt2 ARROW_DEGREE1)))
           (y1 (* size (math/sin3 pt1 pt2 ARROW_DEGREE1)))
           (x2 (* size (math/cos3 pt1 pt2 ARROW_DEGREE2)))
           (y2 (* size (math/sin3 pt1 pt2 ARROW_DEGREE2))))
      (writer-write writer
                    "<path "
                    (when fill
                      (to-property-strings fill))
                    (when stroke
                      (to-property-strings stroke))
                    "d='M " (point-x pt2) " " (point-y pt2) " "
                       "l " x1            " " y1            " "
                       "l " (- x2 x1)     " " (- y2 y1)     " z' "
                    "/>"))))

(defun __draw-endmark-diamond (points size stroke fill writer)
  (symbol-macrolet ((ARROW_DEGREE1 210)
                    (ARROW_DEGREE2 150))
    (let* ((pt1 (car points))
           (pt2 (cdr points))
           (x1 (point-x pt2))
           (y1 (point-y pt2))
           (x2 (+ x1 (* size (math/cos3 pt1 pt2 ARROW_DEGREE1))))
           (y2 (+ y1 (* size (math/sin3 pt1 pt2 ARROW_DEGREE1))))
           (x4 (+ x1 (* size (math/cos3 pt1 pt2 ARROW_DEGREE2))))
           (y4 (+ y1 (* size (math/sin3 pt1 pt2 ARROW_DEGREE2))))
           (x3 (+ x2 (- x4 x1)))
           (y3 (+ y2 (- y4 y1))))
      (writer-write writer
                    "<path "
                    (when fill
                      (to-property-strings fill))
                    (when stroke
                      (to-property-strings stroke))
                    "d='M " x1 " " y1 " L " x2 " " y2 " "
                       "L " x3 " " y3 " L " x4 " " y4 " z' "
                    "/>"))))

(defun __draw-endmark-circle (points size stroke fill writer)
  (let ((pt (cdr points)))
    (writer-write writer
                  "<circle "
                  "cx='" (point-x pt) "' "
                  "cy='" (point-y pt) "' "
                  "r='" (/ size 2) "' "
                  (when fill
                    (to-property-strings fill))
                  (when stroke
                    (to-property-strings stroke))
                  "/>")))

(defun __draw-endmark-rectangle (points size stroke fill writer)
  (let* ((pt1  (car points))
         (pt2  (cdr points))
         (r    (/ size (sqrt 2)))
         (cx   (point-x pt2))
         (cy   (point-y pt2)))
    (writer-write writer
                  "<path "
                  (when fill
                    (to-property-strings fill))
                  (when stroke
                    (to-property-strings stroke))
                  "d='M " (+ cx (* r (math/cos3 pt1 pt2  45)))
                  " "     (+ cy (* r (math/sin3 pt1 pt2  45)))
                  " L "   (+ cx (* r (math/cos3 pt1 pt2 135)))
                  " "     (+ cy (* r (math/sin3 pt1 pt2 135)))
                  " L "   (+ cx (* r (math/cos3 pt1 pt2 225)))
                  " "     (+ cy (* r (math/sin3 pt1 pt2 225)))
                  " L "   (+ cx (* r (math/cos3 pt1 pt2 315)))
                  " "     (+ cy (* r (math/sin3 pt1 pt2 315))) " z' />")))

;;------------------------------------------------------------------------------
;;
;; class endmark-info
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#                :endmark-info
 |#
(defclass endmark-info ()
  ((type    :initform nil :initarg :type)       ; (or keyword function)
                                                ; :none|:arrow|:triangle|:diamond|:circle|:rect
   (size    :initform nil :initarg :size)       ; (or keyword number)
                                                ; :small|:medium|:large|:xlarge
   (fill    :initform nil :initarg :fill)       ; (or nil fill-info)    ; nil means same as stroke
   (stroke  :initform nil :initarg :stroke)))   ; (or nil stroke-info)


(defmethod initialize-instance :after ((mark endmark-info) &rest initargs)
  (declare (ignore initargs))
  (with-slots (type size fill stroke) mark
    (setf type   (or type *default-endmark-type*))
    (setf size   (or size *default-endmark-size*))
    (when (or fill *default-endmark-fill*)
      (setf fill (make-fill (or fill *default-endmark-fill*))))
    (when stroke
      (setf stroke (make-stroke stroke))))
  mark)


(defmethod check ((mark endmark-info) canvas dict)
  (with-slots (type size fill stroke) mark
    (check-member type   :nullable nil :types (or keyword function))
    (check-member size   :nullable nil :types (or keyword number))
    (check-object fill   canvas dict :nullable t :class   fill-info)
    (check-object stroke canvas dict :nullable t :class stroke-info)
    (when (keywordp type)
      (check-keywords type :arrow :triangle :diamond :circle :rect))
    (when (keywordp size)
      (check-keywords size :small :medium :large :xlarge)))
  t)

(defun draw-endmark (mark points stroke writer)
  ;; RULE : 1. mark 自体に stroke/fill が設定されていればそれを使う
  ;;        2. stroke が mark に設定されていない場合、パラメータの stroke を使う
  ;;        3. fill が mark に設定されていない場合、stroke の color/url を使う
  (with-slots (type size fill) mark
    (let* ((sz (if (numberp size)
                   size
                   (ecase size
                     ((:small)  10.0)
                     ((:medium) 15.0)
                     ((:large)  20.0)
                     ((:xlarge) 30.0))))
           (st (or (slot-value mark 'stroke)
                   (make-stroke :dasharray nil :base stroke)))
           (fl (or fill (make-fill :color   (slot-value st 'color)
                                   :url     (slot-value st 'url)
                                   :opacity (slot-value st 'opacity))))
           (drawer (if (functionp type)
                       type
                       (ecase type
                         ((:arrow)    #'__draw-endmark-arrow)
                         ((:triangle) #'__draw-endmark-triangle)
                         ((:diamond)  #'__draw-endmark-diamond)
                         ((:circle)   #'__draw-endmark-circle)
                         ((:rect)     #'__draw-endmark-rectangle)))))
      (funcall drawer points sz st fl writer))))
  

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function make-endmark
;;
;;　make-endmark 関数は終端マーク情報を生成します。終端マーク情報の詳細は「[](#終端マーク)」を
;;参照してください。関数シグネチャは以下の通りです。
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{make-endmark}} ${REST} params
;;
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　上記は簡潔な記述で柔軟な終端マーク情報の生成を可能にするためのもので、 `params` として渡される
;;パラメータ数に応じて以下のことをします。
;;
;;* パラメータ数が 0 の場合
;;    * nil を返します
;;* パラメータ数が 1 の場合
;;    * 終端マーク情報が渡された場合、それをそのまま返します
;;    * 数値 N が渡された場合、 `(make-endmark :size N)` を返します
;;    * リスト lst が渡された場合、 `(apply #'make-endmark lst)` を返します
;;    * 上記のいずれでもない prm の場合、 `(make-endmark :type prm)` を返します
;;* パラメータ数が 2 以上の場合
;;    * 後述します
;;
;;　パラメータ数が 2 以上の場合、make-endmark 関数は実質的に以下の関数であるかのように振舞います。
;;
;;<!-- stack:push li class='syntax' -->
;;
;;* ${{B}{make-endmark}} ${KEY} type size stroke fill
;;
;;<!-- stack:pop li -->
;;
;;Table. make-endmark 関数のパラメータ
;;| parameter   | description                      |
;;|:============|:---------------------------------|
;;| `type`      | 終端マークの形状を指定します。 `:arrow :triangle :diamond :circle :rect` のいずれか、または \
;;カスタム描画関数を指定します。 |
;;| `size`      | 終端マークのサイズを指定します。 `:small :medium :large :xlarge` のいずれか、または数値を指定します。 |
;;| `stroke`    | 終端マークを描画する線を指定します。詳細は「[](#ストローク)」を参照してください。省略した場合、実際に \
;;終端マークが描画される対象（直線やコネクタ）のストロークが使用されます。ただしその場合、そのストロークの `:dasharray`  \
;;など一部のパラメータは引き継がれません。 |
;;| `fill`      | 終端マーク内部の塗り潰しを指定します。詳細は「[](#フィル)」を参照してください。省略した場合、ストローク \
;;と同じ色で塗り潰されます。 |
;;
;;
;;${BLANK_PARAGRAPH}
;;
;;　`type` パラメータでキーワードのかわりに指定するカスタム描画関数ですが、以下のシグネチャの関数を
;;指定する必要があります。これは現状では暫定的で undocumented な状態です。詳細はコードを参照して
;;ください。
;;
;;```lisp
;;(lambda (points size stroke fill writer) ...)
;;```
;;
;;${NO_SEE_ALSO}
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :make-endmark
 |#
(defun make-endmark (&rest params)
  (if (= 1 (length params))
      (let ((param (car params)))
        (cond
          ((typep param 'endmark-info) param)
          ((keywordp param) (make-endmark :type param))
          ((numberp  param) (make-endmark :size param))
          ((listp    param) (apply #'make-endmark param))
          (t                (make-endmark :type param))))
      (if (null params)
          nil
          (destructuring-bind (&key type size fill stroke) params
            (make-instance 'endmark-info
                           :type   type
                           :size   size
                           :fill   fill
                           :stroke stroke)))))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro with-endmark-options
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{with-endmark-options}} (${KEY} type size fill end1 end2) ${BODY} body
;;
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　${{TODO}{まだ記述されていません。}}
;;
;;${NO_SEE_ALSO}
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :with-endmark-options
 |#
(defmacro with-endmark-options ((&key type size fill end1 end2) &rest body)
  (labels ((impl (params acc)
             (if (null params)
                 acc
                 (let ((value  (car  params))
                       (symbol (cadr params)))
                   (impl (cddr params)
                         (if (null value)
                             acc
                             (push (list symbol value) acc)))))))
    (let ((lst (impl (list type '*default-endmark-type*
                           size '*default-endmark-size*
                           fill '*default-endmark-fill*
                           end1 '*default-endmark-1*
                           end2 '*default-endmark-2*) nil)))
      `(let ,lst
         ,@body))))
