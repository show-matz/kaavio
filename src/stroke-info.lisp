#|
#|ASD|#                (:file "stroke-info"               :depends-on ("kaavio"
#|ASD|#                                                                "colormap"))
#|EXPORT|#                ;stroke-info.lisp
 |#


(in-package :kaavio)

;; default parameter for stroke-info
#|
#|EXPORT|#                :*default-stroke*
#|EXPORT|#                :*mute-stroke*
 |#
(defparameter *default-stroke* nil)
(defparameter *mute-stroke*    nil)

;;------------------------------------------------------------------------------
;;
;; class stroke-info
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#                :stroke-info
 |#
(defclass stroke-info ()
  ((color       :initform nil :initarg :color)          ; (or keyword string)
   (width       :initform nil :initarg :width)          ; number
   (opacity     :initform nil :initarg :opacity)        ; number
   (linecap     :initform nil :initarg :linecap)        ; (or nil keyword)
   (linejoin    :initform nil :initarg :linejoin)       ; (or nil keyword)
   (miterlimit  :initform nil :initarg :miterlimit)     ; number
   (dasharray   :initform nil :initarg :dasharray)      ; list
   (dashoffset  :initform nil :initarg :dashoffset)     ; number
   (url         :initform nil :initarg :url)))          ; (or nil keyword)

(defmethod initialize-instance :after ((stroke stroke-info) &rest initargs)
  (declare (ignore initargs))
  stroke)


(defmethod check ((ent stroke-info) canvas dict)
  (declare (ignore canvas dict))
  (with-slots (color width opacity linecap linejoin
                     miterlimit dasharray dashoffset url) ent
    (check-member color      :nullable t :types (or string keyword))
    (when color
      (setf color (colormap-fix color)))
    (check-member width      :nullable t :types number)
    (check-member opacity    :nullable t :types number)
    (check-member linecap    :nullable t :types keyword)
    (check-member linejoin   :nullable t :types keyword)
    (check-member miterlimit :nullable t :types number)
    (check-member dasharray  :nullable t :types list)
    (check-member dashoffset :nullable t :types number)
    (when linecap
      (check-keywords linecap :butt :round :square))
    (when linejoin
      (check-keywords linejoin :miter :round :bevel))
    (check-member url        :nullable t :types keyword))
  t)

(defmethod to-property-strings ((stroke stroke-info))
  (unless *mute-stroke*
    (let ((color      (slot-value stroke      'color))
          (width      (slot-value stroke      'width))
          (opacity    (slot-value stroke    'opacity))
          (linecap    (slot-value stroke    'linecap))
          (linejoin   (slot-value stroke   'linejoin))
          (miterlimit (slot-value stroke 'miterlimit))
          (dasharr    (slot-value stroke  'dasharray))
          (dashoffset (slot-value stroke 'dashoffset))
          (url        (slot-value stroke        'url)))
      (if url
          (setf color (format-string "stroke='url(#" url ")' "))
          (when color
            (setf color (format-string "stroke='" color "' "))))
      (when width      (setf width      (format-string "stroke-width='" width "' ")))
      (when opacity    (setf opacity    (format-string "stroke-opacity='" opacity "' ")))
      (when linecap    (setf linecap    (format-string "stroke-linecap='" linecap "' ")))
      (when linejoin   (setf linejoin   (format-string "stroke-linejoin='" linejoin "' ")))
      (when miterlimit (setf miterlimit (format-string "stroke-miterlimit='" miterlimit "' ")))
      (when dasharr    (setf dasharr    (format nil "stroke-dasharray='~{~A ~}' " dasharr)))
      (when dashoffset (setf dashoffset (format-string "stroke-dashoffset='" dashoffset "' ")))
      (concatenate 'string color width opacity linecap linejoin miterlimit dasharr dashoffset))))


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function make-stroke
;;
;;　make-stroke 関数はストローク情報を生成します。ストローク情報の詳細は「[](#ストローク)」を
;;参照してください。関数シグネチャは以下の通りです。
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{make-stroke}} ${REST} params
;;
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　上記は簡潔な記述で柔軟なストローク情報の生成を可能にするためのもので、 `params` として渡される
;;パラメータ数に応じて以下のことをします。
;;
;;* パラメータ数が 0 の場合
;;    * デフォルトのストローク情報を返します
;;* パラメータ数が 1 の場合
;;    * ストローク情報が渡された場合、それをそのまま返します
;;    * 数値 N が渡された場合、 `(make-stroke :width N)` を返します
;;    * リスト lst が渡された場合、 `(apply #'make-stroke lst)` を返します
;;    * 上記のいずれでもない prm の場合、 `(make-stroke :color prm)` を返します
;;* パラメータ数が 2 以上の場合
;;    * 後述します
;;
;;　パラメータ数が 2 以上の場合、make-stroke 関数は実質的に以下の関数であるかのように振舞います。
;;
;;<!-- stack:push li class='syntax' -->
;;
;;* ${{B}{make-stroke}} ${KEY} color width opacity linecap linejoin miterlimit dasharray dashoffset url base)
;;
;;<!-- stack:pop li -->
;;
;;　各パラメータの意味は以下の通りです。詳細は「[](#ストローク)」を参照してください。
;;
;;Table. make-stroke 関数のパラメータ
;;| parameter    | description          |
;;|:=============|:---------------------|
;;| `color`      | 線の色を指定します。色の指定方法については [$@ 節](#色の指定)を、色の名前については<br> \
;;[$@ 節](#色の名前)を参照してください。  |
;;| `width`      | 線の幅を数値で指定します。                     |
;;| `opacity`    | 線の不透明度を 0.0 ～ 1.0 の数値で指定します。  |
;;| `linecap`    | 線の両端の形状を `:butt, :round, :square` から指定します。  |
;;| `linejoin`   | 線が折れ曲ってできる角の形状を `:miter, :round, :bevel` から指定します。  |
;;| `miterlimit` | `linejoin` が `:miter` の場合の、結合される線の太さに対する結合部の長さの<br> \
;;比率を数値で指定します。デフォルト値は 4 です。 |
;;| `dasharray`  | 点線や破線を描画したい場合に、繰り返される線の長さと間隔の長さをリストで<br> \
;;指定します。デフォルト値は nil で、直線になります。 |
;;| `dashoffset` | `dasharray` を指定する場合に、線の開始を `dasharray` のどこから始めるかの<br> \
;;オフセットを数値で指定します。  |
;;| `url`        | パターンやグラデーションの ID を指定します。詳細は [$@ 節](#ストロークにおけるパターンとグラデーションの指定)を参照してください。 |
;;| `base`       | ${{TODO}{まだ記述されていません}} |
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
#|EXPORT|#                :make-stroke
 |#
(defun make-stroke (&rest params)
  (if (= 1 (length params))
      (let ((param (car params)))
        (cond
          ((typep param 'stroke-info) param)
          ((numberp param) (make-stroke :width param))
          ((listp   param) (apply #'make-stroke param))
          (t               (make-stroke :color param))))
      (if (and (null params) *default-stroke*)
          *default-stroke*
          (destructuring-bind (&key (color      nil      color-p)
                                    (width      nil      width-p)
                                    (opacity    nil    opacity-p)
                                    (linecap    nil    linecap-p)
                                    (linejoin   nil   linejoin-p)
                                    (miterlimit nil miterlimit-p)
                                    (dasharray  nil  dasharray-p)
                                    (dashoffset nil dashoffset-p)
                                    (url        nil        url-p) base) params
            (let ((base (or base *default-stroke*)))
              (labels ((fixval (val-p val slot-sym default)
                         (if val-p
                             val
                             (if base
                                 (slot-value base slot-sym) default))))
                (make-instance 'stroke-info
                               :color      (fixval color-p      color      'color   :black)
                               :width      (fixval width-p      width      'width        1)
                               :opacity    (fixval opacity-p    opacity    'opacity    nil)
                               :linecap    (fixval linecap-p    linecap    'linecap    nil)
                               :linejoin   (fixval linejoin-p   linejoin   'linejoin   nil)
                               :miterlimit (fixval miterlimit-p miterlimit 'miterlimit nil)
                               :dasharray  (fixval dasharray-p  dasharray  'dasharray  nil)
                               :dashoffset (fixval dashoffset-p dashoffset 'dashoffset nil)
                               :url        (fixval url-p        url        'url        nil))))))))



(setf *default-stroke* (make-stroke :color   :black
                                    :width        1
                                    :opacity    nil
                                    :linecap    nil
                                    :linejoin   nil
                                    :miterlimit nil
                                    :dasharray  nil
                                    :dashoffset nil))

;;(to-property-strings (make-stroke))
;;(to-property-strings (make-stroke :red))
;;(to-property-strings (make-stroke 3))
;;(to-property-strings (make-stroke :color :red :width 4 :opacity 0.2))
;;(to-property-strings (make-stroke '(:color :red :width 4 :opacity 0.2)))

