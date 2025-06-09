#|
#|ASD|#                (:file "fill-info"                 :depends-on ("kaavio"
#|ASD|#                                                                "colormap"))
#|EXPORT|#                ;fill-info.lisp
 |#


(in-package :kaavio)

;; default parameter for fill
#|
#|EXPORT|#                :*default-fill*
#|EXPORT|#                :*mute-fill*
 |#
(defparameter *default-fill* nil)
(defparameter *mute-fill*    nil)

;;------------------------------------------------------------------------------
;;
;; class fill-info
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#                :fill-info
 |#
(defclass fill-info ()
  ((color   :initform nil :initarg :color)      ; (or keyword string)
   (opacity :initform nil :initarg :opacity)    ; number
   (rule    :initform nil :initarg :rule)       ; (or nil keyword)
   (url     :initform nil :initarg :url)))      ; (or nil keyword)


(defmethod initialize-instance :after ((fill fill-info) &rest initargs)
  (declare (ignore initargs))
  fill)


(defmethod check ((fill fill-info) canvas dict)
  (declare (ignore canvas dict))
  (with-slots (color opacity rule url) fill
    (check-member color   :nullable t :types (or string keyword))
    (when color
      (setf color (colormap-fix color)))
    (check-member opacity :nullable t :types number)
    (check-member rule    :nullable t :types keyword)
    (when rule
      (check-keywords rule :nonzero :evenodd))
    (check-member url    :nullable t :types keyword))
  t)

(defmethod to-property-strings ((fill fill-info))
  (unless *mute-fill*
    (let ((url (slot-value fill 'url)))
      (if url
          (format-string "fill='url(#" url ")' ")
          (let ((color   (slot-value fill 'color))
                (opacity (slot-value fill 'opacity))
                (rule    (slot-value fill 'rule)))
            (when color
              (setf color (format-string "fill='" color "' ")))
            (when opacity
              (setf opacity (format-string "fill-opacity='" opacity "' ")))
            (when rule
              (setf rule (format-string "fill-rule='" rule "' ")))
            (concatenate 'string color opacity rule))))))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function make-fill
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{make-fill}} ${REST} params
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　指定されたパラメータでフィル情報を生成します。フィル情報の詳細は「[](#フィル)」を参照して
;;ください。上記は簡潔な記述で柔軟なフィル情報の生成を可能にするためのもので、 `params` として
;;渡されるパラメータ数に応じて以下のことをします。
;;
;;* パラメータ数が 0 の場合
;;    * デフォルトのフィル情報を返します
;;* パラメータ数が 1 の場合
;;    * フィル情報が渡された場合、それをそのまま返します
;;    * リスト lst が渡された場合、 `(apply #'make-fill lst)` を返します
;;    * 上記のいずれでもない prm の場合、 `(make-fill :color prm)` を返します
;;* パラメータ数が 2 以上の場合
;;    * 後述します
;;
;;　パラメータ数が 2 以上の場合、make-fill 関数は実質的に以下の関数であるかのように振舞います。
;;
;;<!-- stack:push li class='syntax' -->
;;
;;* ${{B}{make-fill}} ${KEY} color opacity rule url base
;;
;;<!-- stack:pop li -->
;;
;;　各パラメータの意味は以下の通りです。詳細は [$@ 節](#フィル)を参照してください。
;;
;;* `color` ---- 塗り潰しの色を指定します。色の指定方法については [$@ 節](#色の指定)を、色の名前については [$@ 節](#色の名前)を参照してください。
;;* `opacity` ---- 塗り潰しの不透明度を 0.0 ～ 1.0 の数値で指定します。省略した場合のデフォルト値は 1.0 です。
;;* `rule` ---- 塗りつぶしのルールを `:nonzero` または `:evenodd` で指定します。
;;* `url` ---- パターンやグラデーションを使用する場合、その ID をキーワードシンボルで指定します。詳細は [$@ 節](#フィルにおけるパターンとグラデーションの指定)を参照してください。
;;* `base` ---- フィル情報の作成においてベースとする他のフィル情報があれば指定します。
;;
;;${SEE_ALSO}
;;
;;* [](#フィル)
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :make-fill
 |#
(defun make-fill (&rest params)
  (if (= 1 (length params))
      (let ((param (car params)))
        (cond
          ((typep param 'fill-info) param)
          ((listp param) (apply #'make-fill param))
          (t             (make-fill :color param))))
      (if (and (null params) *default-fill*)
          *default-fill*
          (destructuring-bind (&key (color   nil   color-p)
                                    (opacity nil opacity-p)
                                    (rule    nil    rule-p)
                                    (url     nil     url-p) base) params
            (let ((base (or base *default-fill*)))
              (when base
                (setf base (make-fill base)))
              (labels ((fixval (val-p val slot-sym default)
                         (if val-p
                             val
                             (if base
                                 (slot-value base slot-sym) default))))
                (make-instance 'fill-info
                               :color   (fixval color-p   color   'color  :none)
                               :opacity (fixval opacity-p opacity 'opacity  nil)
                               :rule    (fixval rule-p    rule    'rule     nil)
                               :url     (fixval url-p     url     'url      nil))))))))


#|
#|EXPORT|#                :make-fill2
 |#
(defun make-fill2 (base &rest params)
  (if (= 1 (length params))
      (let ((param (car params)))
        (cond
          ((typep param 'fill-info) param)
          ((listp param) (apply #'make-fill      :base base param))
          (t             (make-fill :color param :base base))))
      (let ((base (or base *default-fill*)))
        (apply #'make-fill :base base params))))


(setf *default-fill* (make-fill :color :none
                                :opacity nil
                                :rule    nil
                                :url     nil))

;;(to-property-strings (make-fill))
;;(to-property-strings (make-fill :red))
;;(to-property-strings (make-fill 3))
;;(to-property-strings (make-fill :color :red :opacity 0.2))
;;(to-property-strings (make-fill '(:color :red :opacity 0.2)))

