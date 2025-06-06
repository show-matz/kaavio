#|
#|ASD|#                (:file "raw-svg"                   :depends-on ("kaavio"
#|ASD|#                                                                "constants"
#|ASD|#                                                                "entity"
#|ASD|#                                                                "writer"))
#|EXPORT|#                ;raw-svg.lisp
 |#

(in-package :kaavio)

;;------------------------------------------------------------------------------
;;
;; class raw-svg
;;
;;------------------------------------------------------------------------------
(defclass raw-svg (entity)
  ((data :initform nil :initarg :svgdata)))    ; string

(defmethod initialize-instance :after ((obj raw-svg) &rest initargs)
  (declare (ignore initargs))
  (with-slots (layer) obj
    (setf layer  (if (eq layer :none)
                     nil
                     (or layer *default-layer*))))
  obj)

(defmethod check ((ent raw-svg) canvas dict)
  (declare (ignorable canvas dict))
  (with-slots (data) ent
    (check-member data :nullable nil :types string)
    (setf data (fix-name data)))
  ;; this method must call super class' one.
  (call-next-method))

(defmethod draw-entity ((ent raw-svg) writer)
  (dolist (line (string/split (slot-value ent 'data) #\newline))
    (writer-write writer line)))



;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro raw-svg
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{raw-svg}} svgdata ${KEY} (layer nil)
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `svgdata` ---- 挿入する SVG コード片を文字列で指定します。
;;* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
;;
;;${DESCRIPTION}
;;
;;　SVG 図面の中に任意の SVG コードを挿入します。
;;
;;${SEE_ALSO}
;;
;;* 生の SVG コード片の挿入
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :raw-svg
 |#
(defmacro raw-svg (svgdata &key (layer nil))
  `(register-entity (make-instance 'kaavio:raw-svg
                                   :svgdata ,svgdata :layer ,layer)))

