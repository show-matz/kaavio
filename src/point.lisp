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
;;* `type` ---- `:relative` または `:absolute` を指定します。省略した場合のデフォルト値は `:relative` です。
;;* `result` ---- 座標値が返ります。
;;
;;${DESCRIPTION}
;;
;;　座標値を生成します。 `type` に `absolute` を指定すると絶対座標に、それ以外の場合は
;;相対座標になります。絶対座標は画像全体をキャンバスとしてその左上を `'(0 0)` とする座標
;;で、相対座標は「現在のキャンバス」をの左上を `'(0 0)` とする座標です。
;;
;;${SEE_ALSO}
;;
;;* [](#座標と位置)
;;* [](#サブキャンバス)
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
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* pt ---- コピー対象の座標値を指定します。
;;* result ---- コピーされた座標値が返ります。
;;
;;${DESCRIPTION}
;;
;;　座標値をコピーします。あまり使いません。
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
;;* ${{B}{point-p}} pt => result
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `pt` ---- 対象オブジェクトを指定します。
;;* `result` ---- `T` または `NIL` が返ります。
;;
;;${DESCRIPTION}
;;
;;　`pt` が座標値かどうかを調べます。
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
;;* ${{B}{point-absolute-p}} pt => result
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `pt` ---- 対象オブジェクトを指定します。
;;* `result` ---- `T` または `NIL` が返ります。
;;
;;${DESCRIPTION}
;;
;;　`pt` が絶対座標を示す座標値かどうかを調べます。
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
  (and (point-p pt)
       (eq (cddr pt) :absolute)))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function point-relative-p
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{point-relative-p}} pt => result
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `pt` ---- 対象オブジェクトを指定します。
;;* `result` ---- `T` または `NIL` が返ります。
;;
;;${DESCRIPTION}
;;
;;　`pt` が相対座標を示す座標値かどうかを調べます。
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
  (and (point-p pt)
       (null (cddr pt))))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function point-x
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{point-x}} pt => result
;;* (setf (${{B}{point-x}} pt) val)
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `pt` ---- 対象の座標値を指定します。
;;* `result` ---- `pt` の x 座標が返ります。
;;* `val` ---- `pt` に設定する x 座標値を指定します。
;;
;;${DESCRIPTION}
;;
;;　座標値の x 軸の値を取得または設定します。
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
;;* (setf ${{B}{point-y}} pt) val)
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `pt` ---- 対象の座標値を指定します。
;;* `result` ---- `pt` の y 座標が返ります。
;;* `val` ---- `pt` に設定する y 座標値を指定します。
;;
;;${DESCRIPTION}
;;
;;　座標値の y 軸の値を取得または設定します。
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
;;* ${{B}{pt+}} pt1 pt2 => result
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `pt1` ---- 対象の座標値を指定します。
;;* `pt2` ---- 対象の座標値を指定します。
;;* `result` ---- 結果の座標値が返ります。
;;
;;${DESCRIPTION}
;;
;;　2 つの座標値を足しあわせます。どちらかまたは両方が絶対座標の場合、結果も絶対座標に
;;なります。function point+ と同じことをします。
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
;;* ${{B}{pt-}} pt1 pt2 => result
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `pt1` ---- 対象の座標値を指定します。
;;* `pt2` ---- 対象の座標値を指定します。
;;* `result` ---- 結果の座標値が返ります。
;;
;;${DESCRIPTION}
;;
;;　`pt1` から `pt2` を引きます。どちらかまたは両方が絶対座標の場合、結果も絶対座標に
;;なります。function point- と同じことをします。
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
;;* ${{B}{pt*}} pt n => result
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `pt` ---- 対象の座標値を指定します。
;;* `n` ---- 対象の座標値を指定します。
;;* `result` ---- 結果の座標値が返ります。
;;
;;${DESCRIPTION}
;;
;;　`pt`を `n` 倍した座標値を返します。function point* と同じことをします。
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
;;* ${{B}{pt/}} pt n => result
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `pt` ---- 対象の座標値を指定します。
;;* `n` ---- 対象の座標値を指定します。
;;* `result` ---- 結果の座標値が返ります。
;;
;;${DESCRIPTION}
;;
;;　`pt`を `1/n` 倍した座標値を返します。function point/ と同じことをします。
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
;;* ${{B}{point+}} pt1 pt2 => result
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `pt1` ---- 対象の座標値を指定します。
;;* `pt2` ---- 対象の座標値を指定します。
;;* `result` ---- 結果の座標値が返ります。
;;
;;${DESCRIPTION}
;;
;;　2 つの座標値を足しあわせます。どちらかまたは両方が絶対座標の場合、結果も絶対座標に
;;なります。function pt+ と同じことをします。
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
;;* ${{B}{point-}} pt1 pt2 => result
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `pt1` ---- 対象の座標値を指定します。
;;* `pt2` ---- 対象の座標値を指定します。
;;* `result` ---- 結果の座標値が返ります。
;;
;;${DESCRIPTION}
;;
;;　`pt1` から `pt2` を引きます。どちらかまたは両方が絶対座標の場合、結果も絶対座標に
;;なります。function pt- と同じことをします。
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
;;* ${{B}{point*}} pt n => result
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `pt` ---- 対象の座標値を指定します。
;;* `n` ---- 対象の座標値を指定します。
;;* `result` ---- 結果の座標値が返ります。
;;
;;${DESCRIPTION}
;;
;;　`pt`を `n` 倍した座標値を返します。function pt* と同じことをします。
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
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `pt` ---- 対象の座標値を指定します。
;;* `n` ---- 対象の座標値を指定します。
;;* `result` ---- 結果の座標値が返ります。
;;
;;${DESCRIPTION}
;;
;;　`pt`を `1/n` 倍した座標値を返します。function pt/ と同じことをします。
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
;;* ${{B}{point/x+}} pt x => result
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `pt` ---- 対象の座標値を指定します。
;;* `x` ---- 加算する x 軸の値を指定します。
;;* `result` ---- 結果の座標値が返ります。
;;
;;${DESCRIPTION}
;;
;;　`pt` に `(x 0)` を足した座標値を返します。function x+ と同じことをします。
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
;;* ${{B}{point/y+}} pt y => result
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `pt` ---- 対象の座標値を指定します。
;;* `y` ---- 加算する y 軸の値を指定します。
;;* `result` ---- 結果の座標値が返ります。
;;
;;${DESCRIPTION}
;;
;;　`pt` に `(0 y)` を足した座標値を返します。function y+ と同じことをします。
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
;;* ${{B}{point/xy+}} pt x y => result
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `pt` ---- 対象の座標値を指定します。
;;* `x` ---- 加算する x 軸の値を指定します。
;;* `y` ---- 加算する y 軸の値を指定します。
;;* `result` ---- 結果の座標値が返ります。
;;
;;${DESCRIPTION}
;;
;;　`pt` に `(x y)` を足した座標値を返します。function xy+ と同じことをします。
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
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `pt` ---- 対象の座標値を指定します。
;;* `x` ---- 加算する x 軸の値を指定します。
;;* `result` ---- 結果の座標値が返ります。
;;
;;${DESCRIPTION}
;;
;;　`pt` に `(x 0)` を足した座標値を返します。function point/x+ と同じことをします。
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
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `pt` ---- 対象の座標値を指定します。
;;* `y` ---- 加算する y 軸の値を指定します。
;;* `result` ---- 結果の座標値が返ります。
;;
;;${DESCRIPTION}
;;
;;　`pt` に `(0 y)` を足した座標値を返します。function point/y+ と同じことをします。
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
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `pt` ---- 対象の座標値を指定します。
;;* `x` ---- 加算する x 軸の値を指定します。
;;* `y` ---- 加算する y 軸の値を指定します。
;;* `result` ---- 結果の座標値が返ります。
;;
;;${DESCRIPTION}
;;
;;　`pt` に `(x y)` を足した座標値を返します。function xy+ と同じことをします。
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
;;* ${{B}{point-distance}} pt1 pt2 => result
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `pt1` ---- 対象の座標値を指定します。
;;* `pt2` ---- 対象の座標値を指定します。
;;* `result` ---- 結果の値が返ります。
;;
;;${DESCRIPTION}
;;
;;　`pt1` と `pt2` の間の距離を計算して返します。この時、これらの座標値が絶対座標であるか
;;相対座標であるかは考慮されないので注意が必要です。
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
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `sym-x` ---- `pt` の x 値を参照するための変数名を指定します。
;;* `sym-y` ---- `pt` の y 値を参照するための変数名を指定します。
;;* `pt` ---- 対象の座標値を指定します。
;;* `body` ---- 実行するコードを指定します。
;;
;;${DESCRIPTION}
;;
;;　`pt` の x および y の値を変数で直接参照できるかのようなレキシカル環境を確立し、
;;コード `body` を実行します。コード `body` 内では、 `sym-x sym-y` それぞれで指定した
;;名前の変数で値の取得と設定が可能です。
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

