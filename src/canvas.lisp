#|
#|ASD|#                (:file "canvas"                    :depends-on ("kaavio"
#|ASD|#                                                                "point"))
#|EXPORT|#                ;canvas.lisp
 |#

(in-package :kaavio)

;;------------------------------------------------------------------------------
;;
;; canvas
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#                :canvas
#|EXPORT|#                :make-canvas
#|EXPORT|#                :copy-canvas
#|EXPORT|#                :canvas-p
 |#
(defun make-canvas (top-left width height)
  (cons top-left (cons width height)))

(defun copy-canvas (canvas)
  (copy-tree canvas))

(defun canvas-p (canvas)
  (and (consp         canvas)
       (point-p (car  canvas))
       (numberp (cadr canvas))
       (numberp (cddr canvas))))

#|
#|EXPORT|#                :canvas-topleft
#|EXPORT|#                :canvas-left
#|EXPORT|#                :canvas-top
#|EXPORT|#                :canvas-right
#|EXPORT|#                :canvas-bottom
 |#
(defun canvas-topleft (canvas) (car canvas))
(defun canvas-left    (canvas) (point-x (car canvas)))
(defun canvas-top     (canvas) (point-y (car canvas)))
(defun (setf canvas-left) (val canvas) (setf (point-x (car canvas)) val))
(defun (setf canvas-top)  (val canvas) (setf (point-y (car canvas)) val))
(defun canvas-right   (canvas) (+ (point-x (car canvas)) (cadr canvas)))
(defun canvas-bottom  (canvas) (+ (point-y (car canvas)) (cddr canvas)))

#|
#|EXPORT|#                :canvas-width
#|EXPORT|#                :canvas-height
 |#
(defun canvas-width  (canvas) (cadr canvas))
(defun canvas-height (canvas) (cddr canvas))
(defun (setf canvas-width)  (val canvas) (setf (cadr canvas) val))
(defun (setf canvas-height) (val canvas) (setf (cddr canvas) val))

#|
#|EXPORT|#                :canvas-fix-point
 |#
(defun canvas-fix-point (canvas pt)
  (if (point-absolute-p pt)
      pt
      (point+ (canvas-topleft canvas) pt)))


;; for expansion in 'with-dictionary'
(defun canvas-dict-width       (canv) (cadr canv))
(defun canvas-dict-height      (canv) (cddr canv))
(defun canvas-dict-topleft     (canv) (car  canv))
(defun canvas-dict-top         (canv) (point/x+  (car canv) (/ (cadr canv) 2)))
(defun canvas-dict-topright    (canv) (point/x+  (car canv)    (cadr canv)))
(defun canvas-dict-left        (canv) (point/y+  (car canv) (/ (cddr canv) 2)))
(defun canvas-dict-center      (canv) (point/xy+ (car canv) (/ (cadr canv) 2) (/ (cddr canv) 2)))
(defun canvas-dict-right       (canv) (point/xy+ (car canv)    (cadr canv)    (/ (cddr canv) 2)))
(defun canvas-dict-bottomleft  (canv) (point/y+  (car canv)    (cddr canv)))
(defun canvas-dict-bottom      (canv) (point/xy+ (car canv) (/ (cadr canv) 2)    (cddr canv)))
(defun canvas-dict-bottomright (canv) (point/xy+ (car canv)    (cadr canv)       (cddr canv)))

(defun canvas-dict-topleft.X     (canv) (point-x (canvas-dict-topleft     canv)))
(defun canvas-dict-topleft.Y     (canv) (point-y (canvas-dict-topleft     canv)))
(defun canvas-dict-top.X         (canv) (point-x (canvas-dict-top         canv)))
(defun canvas-dict-top.Y         (canv) (point-y (canvas-dict-top         canv)))
(defun canvas-dict-topright.X    (canv) (point-x (canvas-dict-topright    canv)))
(defun canvas-dict-topright.Y    (canv) (point-y (canvas-dict-topright    canv)))
(defun canvas-dict-left.X        (canv) (point-x (canvas-dict-left        canv)))
(defun canvas-dict-left.Y        (canv) (point-y (canvas-dict-left        canv)))
(defun canvas-dict-center.X      (canv) (point-x (canvas-dict-center      canv)))
(defun canvas-dict-center.Y      (canv) (point-y (canvas-dict-center      canv)))
(defun canvas-dict-right.X       (canv) (point-x (canvas-dict-right       canv)))
(defun canvas-dict-right.Y       (canv) (point-y (canvas-dict-right       canv)))
(defun canvas-dict-bottomleft.X  (canv) (point-x (canvas-dict-bottomleft  canv)))
(defun canvas-dict-bottomleft.Y  (canv) (point-y (canvas-dict-bottomleft  canv)))
(defun canvas-dict-bottom.X      (canv) (point-x (canvas-dict-bottom      canv)))
(defun canvas-dict-bottom.Y      (canv) (point-y (canvas-dict-bottom      canv)))
(defun canvas-dict-bottomright.X (canv) (point-x (canvas-dict-bottomright canv)))
(defun canvas-dict-bottomright.Y (canv) (point-y (canvas-dict-bottomright canv)))

(defun canvas-dict-W           (canv) (cadr canv))
(defun canvas-dict-H           (canv) (cddr canv))
(defun canvas-dict-TL          (canv) (car  canv))
(defun canvas-dict-TC          (canv) (point/x+  (car canv) (/ (cadr canv) 2)))
(defun canvas-dict-TR          (canv) (point/x+  (car canv)    (cadr canv)))
(defun canvas-dict-CL          (canv) (point/y+  (car canv) (/ (cddr canv) 2)))
(defun canvas-dict-CC          (canv) (point/xy+ (car canv) (/ (cadr canv) 2) (/ (cddr canv) 2)))
(defun canvas-dict-CR          (canv) (point/xy+ (car canv)    (cadr canv)    (/ (cddr canv) 2)))
(defun canvas-dict-BL          (canv) (point/y+  (car canv)    (cddr canv)))
(defun canvas-dict-BC          (canv) (point/xy+ (car canv) (/ (cadr canv) 2)    (cddr canv)))
(defun canvas-dict-BR          (canv) (point/xy+ (car canv)    (cadr canv)       (cddr canv)))

(defun canvas-dict-TL.X (canv) (point-x (canvas-dict-TL canv)))
(defun canvas-dict-TL.Y (canv) (point-y (canvas-dict-TL canv)))
(defun canvas-dict-TC.X (canv) (point-x (canvas-dict-TC canv)))
(defun canvas-dict-TC.Y (canv) (point-y (canvas-dict-TC canv)))
(defun canvas-dict-TR.X (canv) (point-x (canvas-dict-TR canv)))
(defun canvas-dict-TR.Y (canv) (point-y (canvas-dict-TR canv)))
(defun canvas-dict-CL.X (canv) (point-x (canvas-dict-CL canv)))
(defun canvas-dict-CL.Y (canv) (point-y (canvas-dict-CL canv)))
(defun canvas-dict-CC.X (canv) (point-x (canvas-dict-CC canv)))
(defun canvas-dict-CC.Y (canv) (point-y (canvas-dict-CC canv)))
(defun canvas-dict-CR.X (canv) (point-x (canvas-dict-CR canv)))
(defun canvas-dict-CR.Y (canv) (point-y (canvas-dict-CR canv)))
(defun canvas-dict-BL.X (canv) (point-x (canvas-dict-BL canv)))
(defun canvas-dict-BL.Y (canv) (point-y (canvas-dict-BL canv)))
(defun canvas-dict-BC.X (canv) (point-x (canvas-dict-BC canv)))
(defun canvas-dict-BC.Y (canv) (point-y (canvas-dict-BC canv)))
(defun canvas-dict-BR.X (canv) (point-x (canvas-dict-BR canv)))
(defun canvas-dict-BR.Y (canv) (point-y (canvas-dict-BR canv)))



;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro with-canvas
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{with-canvas}} (sym-center sym-width sym-height) canvas ${BODY} body
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `sym-center` ---- `canvas` の center 値を参照するための変数名を指定します。
;;* `sym-width` ---- `canvas` の幅を参照するための変数名を指定します。
;;* `sym-height` ---- `canvas` の高さを参照するための変数名を指定します。
;;* `canvas` ---- 対象のキャンバスを指定します。
;;* `body` ---- 実行するコードを指定します。
;;
;;${DESCRIPTION}
;;
;;　`canvas` の中心、幅、および高さを変数で直接参照できるかのようなレキシカル環境を確立し、
;;コード `body` を実行します。コード `body` 内では、 `sym-center sym-width sym-height` 
;;それぞれで指定した名前の変数で値の取得が可能です。
;;
;;${NO_SEE_ALSO}
;;
;;${NOTES}
;;
;;　with-canvas マクロは非推奨となりました。今後は with-current-canvas マクロを使用して
;;ください。
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :with-canvas
 |#
(defmacro with-canvas ((sym-center sym-width sym-height) canvas &rest body)
  (let ((g-canvas (gensym "CANVAS")))
    `(let ((,g-canvas ,canvas))
       (declare (ignorable ,g-canvas))
       (symbol-macrolet ((,sym-center (canvas-dict-center ,g-canvas))
                         (,sym-width  (cadr ,g-canvas))
                         (,sym-height (cddr ,g-canvas)))
         ,@body))))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro with-current-canvas
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{with-current-canvas}} (${REST} vars) ${BODY} body
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `vars` ---- `canvas` における各種の値を参照するための変数を指定します。
;;* `body` ---- 実行するコードを指定します。
;;
;;${DESCRIPTION}
;;
;;　現在のキャンバスの中心、幅、高さなど各種の値を直接参照できるかのようなレキシカル
;;環境を確立し、コード `body` を実行します。使用例は [$@ 章](#サブキャンバス)を参照して
;;ください。
;;
;;${SEE_ALSO}
;;
;;* [](#サブキャンバス)
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :with-current-canvas
 |#
(defmacro with-current-canvas ((&rest vars) &rest body)
  (labels ((fix-let-vars (e)
             (when (symbolp e)
               (setf e (list e e)))
             (let ((method-sym (onlisp/symb
                                (onlisp/mkstr "CANVAS-DICT-" (cadr e)))))
               `(,(car e) (,method-sym canvas)))))
    `(let ,(mapcar #'fix-let-vars vars)
       ,@body)))


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro with-subcanvas
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{with-subcanvas}} (top-left width height ${KEY} debug) ${BODY} body
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `top-left` ---- 作成するサブキャンバスの（現在のキャンバスから見た）左上の座標を指定します。
;;* `width` ---- 作成するサブキャンバスの幅を指定します。
;;* `height` ---- 作成するサブキャンバスの高さ指定します。
;;* `debug` ---- `T` または色名を示すキーワードシンボルを指定するとキャンバスの領域を明示する補助線が描画されます。
;;* `body` ---- 作成したサブキャンバス内部に描画するコードを指定します。
;;
;;${DESCRIPTION}
;;
;;　任意の位置とサイズでサブキャンバスを確立し、その内部に描画を行ないます。既存の図形要素の
;;内部をサブキャンバスとした描画をしたい場合、with-subcanvas-of マクロを使用してください。
;;
;;${SEE_ALSO}
;;
;;* [](#サブキャンバス)
;;* with-subcanvas-of マクロ
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :with-subcanvas
 |#
(defmacro with-subcanvas ((top-left width height &key debug) &rest body)
  (if (not debug)
      `(let ((canvas (make-canvas (point+ (car canvas) ,top-left) ,width ,height)))
         (declare (special canvas))
         ,@body)
      (let ((clr (if (keywordp debug) debug :red)))
        `(let ((canvas (make-canvas (point+ (car canvas) ,top-left) ,width ,height)))
           (declare (special canvas))
           (let ((*dict-mute-history* t))
             (rect (canvas-dict-center canvas)
                   (canvas-width canvas) (canvas-height canvas)
                   :stroke (list :color ,clr :width 1 :dasharray '(1 2)) :fill :none))
           ,@body))))


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro with-subcanvas-of
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{with-subcanvas-of}} (id) ${BODY} body
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `id` ---- 対象となる図形要素の ID をキーワードシンボルで指定します。
;;* `body` ---- 作成したサブキャンバス内部に描画するコードを指定します。
;;
;;${DESCRIPTION}
;;
;;　既存の図形要素の内部をサブキャンバスとして、その内部に描画を行ないます。任意の領域を
;;サブキャンバスとした描画をしたい場合、with-subcanvas マクロを使用してください。
;;
;;${SEE_ALSO}
;;
;;* [](#サブキャンバス)
;;* with-subcanvas マクロ
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :with-subcanvas-of
 |#
(defmacro with-subcanvas-of ((id) &body body)
  (let ((g-obj (gensym "TBL")))
    `(let* ((,g-obj (kaavio::dict-get-entity (kaavio::get-dictionary) ,id))
            (canvas (kaavio:shape-get-subcanvas ,g-obj)))
       (declare (special canvas))
       ,@body)))




