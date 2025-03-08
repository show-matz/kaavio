#|
#|ASD|#                (:file "clipping"                  :depends-on ("kaavio"
#|ASD|#                                                                "writer"))
#|EXPORT|#                ;clipping.lisp
 |#

(in-package :kaavio)


#|
#|EXPORT|#                :*current-clip-path*
 |#
(defparameter *current-clip-path* nil)


;;------------------------------------------------------------------------------
;;
;; abstract class clipper
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#                :clipper
 |#
(defclass clipper ()
  ((path-id  :initform nil :initarg :path-id)))  ; symbol

(defgeneric write-clipper (clppr writer))

;;------------------------------------------------------------------------------
;;
;; class clipper-ref
;;
;;------------------------------------------------------------------------------
(defclass clipper-ref (clipper)
  ((ref-id  :initform nil :initarg :ref-id)))  ; keyword

(defmethod write-clipper ((clppr clipper-ref) writer)
  (with-slots (path-id ref-id) clppr
    (writer-write writer
                  "<defs><clipPath id='" path-id "'>"
                  "<use xlink:href='#" ref-id "' />"
                  "</clipPath></defs>")))

;;------------------------------------------------------------------------------
;;
;; class clipper-canvas
;;
;;------------------------------------------------------------------------------
(defclass clipper-canvas (clipper)
  ((topleft :initform nil :initarg :topleft)   ; point
   (width   :initform nil :initarg :width)     ; number
   (height  :initform nil :initarg :height)))  ; number

(defmethod write-clipper ((clppr clipper-canvas) writer)
  (with-slots (path-id topleft width height) clppr
    (writer-write writer
                  "<defs><clipPath id='" path-id "'>"
                  "<rect x='" (car  topleft) "' "
                        "y='" (cadr topleft) "' "
                        "width='"  width "' "
                        "height='" height "' fill='none' stroke='none' />"
                  "</clipPath></defs>")))


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro with-clipping-use
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{with-clipping-use}} (id) ${BODY} body
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `id` ---- クリッピングパスとして扱う既出の図形要素の ID をキーワードシンボルで指定します。
;;* `body` ---- クリッピングの対象となる描画コードを記述します。
;;
;;${DESCRIPTION}
;;
;;　既出の図形要素の ID を指定し、その描画パスを使ってクリッピングを行ないます。
;;
;;${SEE_ALSO}
;;
;;* クリッピング
;;* with-clipping-current-canvas マクロ
;;
;;${NOTES}
;;
;;　`id` で指定する図形要素の ID は、その図形要素の記述において明示的に指定されていなければ
;;なりません。たとえばコネクタなどでは `$1.id` といった記述で ID が明示的に指定されていない
;;図形要素を指定できますが、 with-clipping-use マクロではそれはできません
;;{{fn: これは、省略時に自動的に付与される ID は SVG コード上には現れないからです。しかし、この挙動は \
;;将来変更される可能性があります。}}。
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :with-clipping-use
 |#
(defmacro with-clipping-use ((id) &body body)
  (let ((g-clippath-id (gensym "CLIPPATH")))
    `(locally
       (register-clipper (make-instance 'kaavio::clipper-ref
                                        :path-id ',g-clippath-id :ref-id ,id))
       (let ((*current-clip-path* ',g-clippath-id))
         ,@body))))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro with-clipping-current-canvas
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{with-clipping-current-canvas}} ${BODY} body
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `body` ---- クリッピングの対象となる描画コードを記述します。
;;
;;${DESCRIPTION}
;;
;;　現在のキャンバスを使ってクリッピングを行ないます。
;;
;;${SEE_ALSO}
;;
;;* クリッピング
;;* with-clipping-use マクロ
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :with-clipping-current-canvas
 |#
(defmacro with-clipping-current-canvas (&body body)
  (let ((g-clippath-id (gensym "CLIPPATH")))
    `(locally
       (register-clipper (make-instance 'kaavio::clipper-canvas
                                        :path-id ',g-clippath-id
                                        :topleft (canvas-topleft canvas)
                                        :width   (canvas-width   canvas)
                                        :height  (canvas-height  canvas)))
       (let ((*current-clip-path* ',g-clippath-id))
         ,@body))))
  
