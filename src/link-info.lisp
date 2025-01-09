#|
#|ASD|#                (:file "link-info"                 :depends-on ("kaavio"
#|ASD|#                                                                "constants"
#|ASD|#                                                                "writer"))
#|EXPORT|#                ;link-info.lisp
 |#

(in-package :kaavio)

;;------------------------------------------------------------------------------
;;
;; link-info
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#                :link-info
 |#
(defclass link-info ()
  ((url     :initform nil :initarg :url)        ; string
   (target  :initform nil :initarg :target)))   ; keyword - :replace :self :parent :top :blank

(defmethod initialize-instance :after ((link link-info) &rest initargs)
  (declare (ignore initargs))
  (with-slots (target) link
    (setf target (or target *default-link-target*)))
  link)

(defmethod check ((ent link-info) canvas dict)
  (declare (ignore canvas dict))
  (with-slots (url target) ent
    (check-member url    :nullable nil :types string)
    (check-member target :nullable   t :types keyword)
    (when target
      (check-keywords target :replace :self :parent :top :blank)))
  nil)

#|
#|EXPORT|#                :write-link-open
 |#
(defun write-link-open (link writer)
  (when link
    (with-slots (url target) link
      (writer-write writer "<a xlink:href='" url "'"
                              (write-when target " target='_" it "'") ">"))
    (writer-incr-level writer)))

#|
#|EXPORT|#                :write-link-close
 |#
(defun write-link-close (link writer)
  (when link
    (writer-decr-level writer)
    (writer-write writer "</a>")))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function make-link
;;
;;　make-link 関数はリンク情報を生成します。リンク情報の詳細は「[](#リンク)」を参照してください。
;;関数シグネチャは以下の通りです。
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{make-link}} ${REST} params
;;
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　上記は簡潔な記述で柔軟なリンク情報の生成を可能にするためのもので、 `params` として渡される
;;パラメータ数に応じて以下のことをします。
;;
;;* パラメータ数が 0 の場合
;;    * nil を返します
;;* パラメータ数が 1 の場合
;;    * リンク情報が渡された場合、それをそのまま返します
;;    * リスト lst が渡された場合、 `(apply #'make-link lst)` を返します
;;    * 上記のいずれでもない prm の場合、 `(make-link :url prm)` を返します
;;* パラメータ数が 2 以上の場合
;;    * 後述します
;;
;;　パラメータ数が 2 以上の場合、make-link 関数は実質的に以下の関数であるかのように振舞います。
;;
;;<!-- stack:push li class='syntax' -->
;;
;;* ${{B}{make-link}} ${KEY} url target
;;
;;<!-- stack:pop li -->
;;
;;　各パラメータの意味は以下の通りです。詳細は「[](#リンク)」を参照してください。
;;
;;Table. make-link 関数のパラメータ
;;| parameter    | description                          |
;;|:=============|:-------------------------------------|
;;| `url`        | リンク先の URL を文字列で指定します。  |
;;| `target`     | a タグにおける target をキーワード `:replace :self :parent :top :blank` の<br> \
;;いずれかで指定します。省略可能です。 |
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
#|EXPORT|#                :make-link
 |#
(defun make-link (&rest params)
  (if (= 1 (length params))
      (let ((param (car params)))
        (cond
          ((typep param 'link-info) param)
          ((listp param) (apply #'make-link param))
          (t             (make-link :url param))))
      (if (null params)
          nil
          (destructuring-bind (&key url target) params
            (make-instance 'link-info :url url :target target)))))


;;(make-link)
;;(make-link "http://www.google.co.jp/"))
;;(make-link :url "http://www.google.co.jp/" :target :top)
;;(make-link '(:url "http://www.google.co.jp/" :target :top))

