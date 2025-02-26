#|
#|ASD|#                (:file "shadow-filter"             :depends-on ("kaavio"
#|ASD|#                                                                "filter"
#|ASD|#                                                                "writer"))
#|EXPORT|#                ;shadow-filter.lisp
 |#

(in-package :kaavio)


;;------------------------------------------------------------------------------
;;
;; class shadow-filter
;;
;;------------------------------------------------------------------------------
(defclass shadow-filter (filter)
  ((color-matrix :initform nil :initarg :color-matrix) ; nil or list of number
   (deviation    :initform nil :initarg :deviation)    ; number
   (dx           :initform nil :initarg :dx)           ; number
   (dy           :initform nil :initarg :dy)))         ; number


(defmethod write-filter ((filt shadow-filter) writer)
  (with-slots (id color-matrix deviation dx dy) filt
    (writer-write writer "<filter id='" id "'>")
    (writer-incr-level writer)
    (let ((in nil))
      (if color-matrix
        (writer-write writer "<feColorMatrix type='matrix' values='"
                                  (format nil "~{ ~A~}" color-matrix) "' />")
        (setf in " in='SourceAlpha'"))
      (writer-write writer "<feGaussianBlur" in " stdDeviation='" deviation "' result='blur' />")
      (setf in " in='blur'")
      (when dx
        (writer-write writer "<feOffset" in " dx='" dx "' dy='" dy "' result='offsetBlur' />")
        (setf in " in='offsetBlur'"))
      (writer-write writer "<feMerge>")
      (writer-incr-level writer)
      (writer-write writer "<feMergeNode" in " />")
      (writer-write writer "<feMergeNode in='SourceGraphic'/>")
      (writer-decr-level writer)
      (writer-write writer "</feMerge>")
      (writer-decr-level writer)
      (writer-write writer "</filter>"))))


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro drop-shadow
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{drop-shadow}} ${KEY} id color-matrix deviation dx dy
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `id` ---- ID をキーワードで指定します。省略した場合のデフォルト値は `:drop-shadow` です。
;;* `color-matrix` ---- `<feColorMatrix>` の values 値を数値のリストで指定します。詳細は SVG 規格を参照してください。省略した場合のデフォルト値は `'(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.4 0)` です。
;;* `deviation` ---- `<feGaussianBlur>` における `stdDeviation` 値を指定します。詳細は SVG 規格を参照してください。省略した場合のデフォルト値は 2 です。
;;* `dx` ---- `<feOffset>` における `dx` 値を指定します。詳細は SVG 規格を参照してください。省略した場合のデフォルト値は 4 です。
;;* `dy` ---- `<feOffset>` における `dy` 値を指定します。詳細は SVG 規格を参照してください。省略した場合のデフォルト値は 4 です。
;;
;;${DESCRIPTION}
;;
;;　生成画像にドロップシャドウを導入します。詳細は SVG 規格を参照してください。
;;
;;${SEE_ALSO}
;;
;;* [](#フィルタ)
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :drop-shadow
 |#
(defmacro drop-shadow (&key id color-matrix deviation dx dy)
  `(register-filter (make-instance 'kaavio::shadow-filter
                                   :id (or ,id :drop-shadow)
                                   :color-matrix (or ,color-matrix '(0 0 0 0   0
                                                                     0 0 0 0   0
                                                                     0 0 0 0   0
                                                                     0 0 0 0.4 0))
                                   :deviation (or ,deviation 2)
                                   :dx (or ,dx 4) :dy (or ,dy 4))))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro glow-shadow
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{glow-shadow}} ${KEY} id color-matrix deviation
;;
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `id` ---- ID をキーワードで指定します。省略した場合のデフォルト値は `:glow-shadow` です。
;;* `color-matrix` ---- `<feColorMatrix>` の values 値を数値のリストで指定します。詳細は SVG 規格を参照してください。省略した場合のデフォルト値は `'(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0)` です。
;;* `deviation` ---- `<feGaussianBlur>` における `stdDeviation` 値を指定します。詳細は SVG 規格を参照してください。省略した場合のデフォルト値は 3 です。
;;
;;${DESCRIPTION}
;;
;;　生成画像にグローシャドウを導入します。詳細は SVG 規格を参照してください。
;;
;;${SEE_ALSO}
;;
;;* [](#フィルタ)
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :glow-shadow
 |#
(defmacro glow-shadow (&key id color-matrix deviation)
  `(register-filter (make-instance 'kaavio::shadow-filter
                                   :id (or ,id :glow-shadow)
                                   :color-matrix (or ,color-matrix '(0 0 0 0 0
                                                                     0 0 0 0 0
                                                                     0 0 0 0 0
                                                                     0 0 0 1 0))
                                   :deviation (or ,deviation 3))))
