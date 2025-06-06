#|
#|ASD|#                (:file "polygon"                   :depends-on ("kaavio"
#|ASD|#                                                                "constants"
#|ASD|#                                                                "fill-info"
#|ASD|#                                                                "stroke-info"
#|ASD|#                                                                "link-info"
#|ASD|#                                                                "entity"
#|ASD|#                                                                "clipping"
#|ASD|#                                                                "filter"
#|ASD|#                                                                "writer"))
#|EXPORT|#                ;polygon.lisp
 |#

(in-package :kaavio)

;;------------------------------------------------------------------------------
;;
;; class polygon
;;
;;------------------------------------------------------------------------------
(defclass polygon (entity)
  ((points  :initform nil :initarg :points)  ; list
   (fill    :initform nil :initarg :fill)    ; (or nil fill-info)
   (stroke  :initform nil :initarg :stroke)  ; (or nil stroke-info)
   (clip-path :initform nil :initarg :clip-path) ; (or nil symbol)
   (filter  :initform nil :initarg :filter)  ; (or nil keyword)
   (link    :initform nil :initarg :link)))  ; (or nil link-info)


(defmethod initialize-instance :after ((ent polygon) &rest initargs)
  (declare (ignore initargs))
  (with-slots (points fill stroke link filter layer) ent
    (setf points (copy-list points))
    (setf fill   (make-fill   (or fill   *default-fill*   :none)))
    (setf stroke (make-stroke (or stroke *default-stroke* :none)))
    (setf link   (make-link   link))
    (setf filter (if (eq filter :none)
                     nil
                     (or filter *default-filter*)))
    (setf layer  (if (eq layer :none)
                     nil
                     (or layer *default-layer*))))
  ent)
  
(defmethod check ((ent polygon) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (points fill stroke clip-path filter link) ent
    (check-member points :nullable nil :types list)
    (check-object fill   canvas dict :nullable nil :class fill-info)
    (check-object stroke canvas dict :nullable nil :class stroke-info)
    (check-member clip-path :nullable  t :types symbol)
    (check-member filter :nullable   t :types keyword)
    (check-object link   canvas dict :nullable   t :class link-info)
    (unless (<= 3 (length points))
      (throw-exception "Less than 3 points of polygon."))
    (dolist (pt points)
      (unless (point-p pt)
        (throw-exception "Invalid value '~A' in points of polygon." pt)))
    (setf points (mapcar (lambda (pt)
                           (canvas-fix-point canvas pt)) points)))
  nil)

(defmethod entity-composition-p ((ent polygon))
  (not (null (slot-value ent 'link))))

(defmethod pre-draw ((ent polygon) writer)
  (call-next-method)
  (when (entity-composition-p ent)
    (let ((lnk (slot-value ent 'link)))
      (when lnk
        (write-link-open lnk writer)))))

(defmethod post-draw ((ent polygon) writer)
  (when (entity-composition-p ent)
    (let ((lnk (slot-value ent 'link)))
      (when lnk
        (write-link-close lnk writer))))
  (call-next-method))

(defmethod draw-entity ((ent polygon) writer)
  (labels ((format-points (pts)
             (with-output-to-string (stream)
               (do ((idx 0 (incf idx)))
                   ((null pts) nil)
                 (unless (zerop idx)
                   (princ #\space stream))
                 (format stream "~A,~A"
                         (coerce (point-x (car pts)) 'single-float)
                         (coerce (point-y (car pts)) 'single-float))
                 (setf pts (cdr pts))))))
    (with-slots (points fill stroke clip-path filter) ent
      (let ((id  (and (not (entity-composition-p ent))
                      (slot-value ent 'id))))
        (pre-draw ent writer)
        (writer-write writer
                      "<polygon "
                      (write-when (keywordp id) "id='" id "' ")
                      (to-property-strings fill)
                      (to-property-strings stroke)
                      "points='" (format-points points) "' "
                      (write-when clip-path "clip-path='url(#" it ")' ")
                      (write-when filter "filter='url(#" it ")' ")
                      "/>")
        (pre-draw ent writer)))))



;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro polygon
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{polygon}} points ${KEY} fill stroke link layer filter id
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `points` ---- 多角形を構成する点のリストを指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `fill` ---- 内部の塗り潰しを指定します。
;;* `stroke` ---- 線を描画するストロークを指定します。
;;* `link` ---- リンクにする場合、リンク先を指定します。
;;* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します。
;;* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します。
;;* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します。
;;
;;${DESCRIPTION}
;;
;;　多角形を描画します。複数の基本要素でスタイルを統一したい場合、with-options マクロを
;;使うことができます。
;;
;;${SEE_ALSO}
;;
;;* 多角形
;;* 正多角形
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :polygon
 |#
(defmacro polygon (points &key fill stroke link layer filter id)
  `(register-entity (make-instance 'kaavio:polygon
                                   :points ,points :fill ,fill :stroke ,stroke
                                   :clip-path *current-clip-path*
                                   :link ,link :layer ,layer :filter ,filter :id ,id)))

