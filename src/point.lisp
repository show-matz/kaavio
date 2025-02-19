#|
#|ASD|#                (:file "point"                     :depends-on ("kaavio"))
#|EXPORT|#                ;point.lisp
 |#

(in-package :kaavio)

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function make-point
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{make-point}} x y ${OPTIONAL} type => result
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `x` ---- x 座標を数値で指定します。
;;* `y` ---- y 座標を数値で指定します。
;;* `type` ---- `:relative` または `:absolute` を指定します。
;;* `result` ---- point 値が返ります。
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
#|EXPORT|#                :make-point
 |#
(defun make-point (x y &optional type)
  (let ((type (or type :relative)))
    (cond
      ((eq type :relative) (cons x (cons y nil)))
      ((eq type :absolute) (cons x (cons y :absolute)))
      (t (error "type must be :relative or :absolute.")))))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function copy-point
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{copy-point}} pt => result
;;
;;${ARGS_AND_VALS}
;;
;;* pt ---- a point
;;* result ---- a point
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
#|EXPORT|#                :copy-point
 |#
(defun copy-point (pt)
  (make-point (car pt) (cadr pt) (cddr pt)))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function point-p
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{point-p}} pt
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
#|EXPORT|#                :point-p
 |#
(defun point-p (pt)
  (and (consp pt)
       (consp   (cdr  pt))
       (numberp (car  pt))
       (numberp (cadr pt))))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function point-absolute-p
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{point-absolute-p}} pt
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
#|EXPORT|#                :point-absolute-p
 |#
(defun point-absolute-p (pt)
  (eq (cddr pt) :absolute))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function point-relative-p
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{point-relative-p}} pt
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
#|EXPORT|#                :point-relative-p
 |#
(defun point-relative-p (pt)
  (null (cddr pt)))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function point-x
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{point-x}} pt
;;* ${{B}{(setf point-x)}} val pt
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
#|EXPORT|#                :point-x
 |#
(defun point-x (pt) (car  pt))
(defun (setf point-x) (val pt) (setf (car  pt) val))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function point-y
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{point-y}} pt
;;* ${{B}{(setf point-y)}} val pt
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
#|EXPORT|#                :point-y
 |#
(defun point-y (pt) (cadr pt))
(defun (setf point-y) (val pt) (setf (cadr pt) val))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function pt+
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{pt+}} pt1 pt2
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
#|EXPORT|#                :pt+
 |#
(defun pt+ (pt1 pt2)
  (make-point (+  (car  pt1) (car  pt2))
              (+  (cadr pt1) (cadr pt2))
              (or (cddr pt1) (cddr pt2))))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function pt-
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{pt-}} pt1 pt2
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
#|EXPORT|#                :pt-
 |#
(defun pt- (pt1 pt2)
  (make-point (-  (car  pt1) (car  pt2))
              (-  (cadr pt1) (cadr pt2))
              (or (cddr pt1) (cddr pt2))))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function pt*
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{pt*}} pt n
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
#|EXPORT|#                :pt*
 |#
(defun pt* (pt n)
  (make-point (*  (car  pt) n)
              (*  (cadr pt) n) (cddr pt)))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function pt/
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{pt/}} pt n
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
#|EXPORT|#                :pt/
 |#
(defun pt/ (pt n)
  (make-point (/  (car  pt) n)
              (/  (cadr pt) n) (cddr pt)))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function point+
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{point+}} pt1 pt2
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
#|EXPORT|#                :point+
 |#
(defun point+ (pt1 pt2) (pt+ pt1 pt2))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function point-
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{point-}} pt1 pt2
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
#|EXPORT|#                :point-
 |#
(defun point- (pt1 pt2) (pt- pt1 pt2))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function point*
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{point*}} pt n
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
#|EXPORT|#                :point*
 |#
(defun point* (pt n)    (pt* pt  n))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function point/
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{point/}} pt n
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
#|EXPORT|#                :point/
 |#
(defun point/ (pt n)    (pt/ pt  n))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function point/x+
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{point/x+}} pt x
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
#|EXPORT|#                :point/x+
 |#
(defun point/x+ (pt x)
  (make-point (+ (car pt) x) (cadr pt) (cddr pt)))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function point/y+
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{point/y+}} pt y
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
#|EXPORT|#                :point/y+
 |#
(defun point/y+ (pt y)
  (make-point (car pt) (+ (cadr pt) y) (cddr pt)))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function point/xy+
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{point/xy+}} pt x y
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
#|EXPORT|#                :point/xy+
 |#
(defun point/xy+ (pt x y)
  (make-point (+ (car  pt) x)
              (+ (cadr pt) y) (cddr pt)))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function x+
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{x+}} pt x => result
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
#|EXPORT|#                :x+
 |#
(defun x+ (pt x)
  (make-point (+ (car pt) x) (cadr pt) (cddr pt)))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function y+
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{y+}} pt y => result
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
#|EXPORT|#                :y+
 |#
(defun y+ (pt y)
  (make-point (car pt) (+ (cadr pt) y) (cddr pt)))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function xy+
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{xy+}} pt x y => result
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
#|EXPORT|#                :xy+
 |#
(defun xy+ (pt x y)
  (make-point (+ (car  pt) x)
              (+ (cadr pt) y) (cddr pt)))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function point-distance
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{point-distance}} pt1 pt2
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
#|EXPORT|#                :point-distance
 |#
(defun point-distance (pt1 pt2)
  (let ((x (- (point-x pt1) (point-x pt2)))
        (y (- (point-y pt1) (point-y pt2))))
    (sqrt (+ (* x x) (* y y)))))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro with-point
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{with-point}} (sym-x sym-y) pt ${BODY} body
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
#|EXPORT|#                :with-point
 |#
(defmacro with-point ((sym-x sym-y) pt &rest body)
  (let ((g-pt (gensym "PT")))
    `(let ((,g-pt ,pt))
       (symbol-macrolet ((,sym-x (car  ,g-pt))
                         (,sym-y (cadr ,g-pt)))
         (declare (ignorable ,sym-x ,sym-y))
         ,@body))))

