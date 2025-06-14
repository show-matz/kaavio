#|
#|ASD|#                (:file "kaavio")
#|EXPORT|#                ;kaavio.lisp
 |#

(provide :kaavio)

(defpackage     :kaavio
  (:use         :common-lisp)
  (:nicknames   :kaavio)
  (:export      ;--------------- BEGIN EXPORT
                ;2d-curve.lisp
                :2d-curve
                ;3d-curve.lisp
                :3d-curve
                ;arc.lisp
                :arc
                ;balloon.lisp
                :*default-balloon-round*
                :*default-balloon-align*
                :*default-balloon-valign*
                :*default-balloon-margin*
                :*default-balloon-font*
                :*default-balloon-fill*
                :*default-balloon-stroke*
                :*default-balloon-filter*
                :*default-balloon-layer*
                :balloon
                :with-balloon-options
                ;binutil.lisp
                ;block-arrow.lisp
                :*default-block-arrow-length*
                :*default-block-arrow-size*
                :*default-block-arrow-margin*
                :*default-block-arrow-stroke*
                :*default-block-arrow-fill*
                :*default-block-arrow-filter*
                :*default-block-arrow-layer*
                :block-arrow
                :block-arrow1
                :block-arrow2
                :with-block-arrow-options
                ;brace.lisp
                :*default-brace-font*
                :*default-brace-stroke*
                :*default-brace-filter*
                :*default-brace-layer*
                :brace
                :with-brace-options
                ;canvas.lisp
                :canvas
                :make-canvas
                :copy-canvas
                :canvas-p
                :canvas-center
                :canvas-topleft
                :canvas-topright
                :canvas-bottomleft
                :canvas-bottomright
                :canvas-left
                :canvas-top
                :canvas-right
                :canvas-bottom
                :canvas-width
                :canvas-height
                :canvas-fix-point
                :with-canvas
                :with-current-canvas
                :with-subcanvas
                :with-subcanvas-of
                ;circle.lisp
                :circle-connect-point
                :circle
                ;clipping.lisp
                :*current-clip-path*
                :clipper
                :with-clipping-use
                :with-clipping-current-canvas
                ;colormap.lisp
                :colormap-fix
                ;connector.lisp
                :resolve-connector-points
                :connector
                :connect
                :with-connector-options
                ;constants.lisp
                :*default-link-target*
                :*default-endmark-1*
                :*default-endmark-2*
                :*default-endmark-type*
                :*default-endmark-size*
                :*default-endmark-fill*
                :*default-label-position*
                :*default-label-font*
                :*default-label-offset*
                :*default-connector-style*
                :*default-connector-spacing*
                :*default-connector-stroke*
                :*default-connector-layer*
                :*default-connector-filter*
                :*default-rectangle-rx*
                :*default-rectangle-ry*
                :*default-text-align*
                :*default-paragraph-align*
                :*default-paragraph-valign*
                :*default-paragraph-font*
                :*default-paragraph-layer*
                :*default-history-count*
                :*default-layer*
                ;create-svg.lisp
                :sandbox-start
                :sandbox-stop
                :create-svg
                :diagram
                :register-entity
                :width
                :height
                :layer
                ;cross.lisp
                :*default-cross-fill*
                :*default-cross-stroke*
                :*default-cross-filter*
                :*default-cross-layer*
                :cross
                :with-cross-options
                ;cube.lisp
                :*default-cube-depth*
                :*default-cube-align*
                :*default-cube-valign*
                :*default-cube-margin*
                :*default-cube-font*
                :*default-cube-fill*
                :*default-cube-fill2*
                :*default-cube-stroke*
                :*default-cube-filter*
                :*default-cube-layer*
                :cube
                :with-cube-options
                ;cylinder.lisp
                :*default-cylinder-depth*
                :*default-cylinder-align*
                :*default-cylinder-valign*
                :*default-cylinder-margin*
                :*default-cylinder-font*
                :*default-cylinder-fill*
                :*default-cylinder-stroke*
                :*default-cylinder-filter*
                :*default-cylinder-layer*
                :cylinder
                :with-cylinder-options
                ;defgradient.lisp
                :gradient-stop
                :gradient-definition
                :linear-gradient-definition
                :radial-gradient-definition
                :defgradient
                ;defgroup.lisp
                :group-definition
                :defs
                :defgroup
                ;definition.lisp
                :definition
                ;defpattern.lisp
                :pattern-definition
                :defpattern
                ;diamond.lisp
                :diamond-connect-point
                :diamond
                ;dictionary.lisp
                :*dict-mute-history*
                :dictionary
                ;document.lisp
                :*default-document-align*
                :*default-document-valign*
                :*default-document-margin*
                :*default-document-font*
                :*default-document-fill*
                :*default-document-stroke*
                :*default-document-filter*
                :*default-document-layer*
                :document
                :with-document-options
                ;ellipse.lisp
                :ellipse-connect-point
                :ellipse
                ;endmark-info.lisp
                :endmark-info
                :make-endmark
                :with-endmark-options
                ;entity.lisp
                :attribute-id
                :attribute-width
                :attribute-height
                :attribute-topleft
                :attribute-top
                :attribute-topright
                :attribute-left
                :attribute-center
                :attribute-right
                :attribute-bottomleft
                :attribute-bottom
                :attribute-bottomright
                :attribute-end1
                :attribute-end2
                :entity
                :write-header
                :draw-entity
                :pre-draw
                :post-draw
                :entity-composition-p
                :check-and-draw-local-entity
                ;explosion.lisp
                :*default-explosion-font*
                :*default-explosion-fill*
                :*default-explosion-stroke*
                :*default-explosion-filter*
                :*default-explosion-layer*
                :explosion1
                :explosion2
                :with-explosion-options
                ;fill-info.lisp
                :*default-fill*
                :*mute-fill*
                :fill-info
                :make-fill
                :make-fill2
                ;filter.lisp
                :*default-filter*
                :filter
                :write-filter
                ;folder.lisp
                :*default-folder-tabwidth*
                :*default-folder-tabheight*
                :*default-folder-align*
                :*default-folder-valign*
                :*default-folder-margin*
                :*default-folder-font*
                :*default-folder-fill*
                :*default-folder-stroke*
                :*default-folder-filter*
                :*default-folder-layer*
                :folder
                :with-folder-options
                ;font-info.lisp
                :*default-font*
                :*mute-font*
                :*default-font-fill*
                :*default-font-stroke*
                :*default-font-filter*
                :font-info
                :make-font
                :make-font2
                :font-calc-textarea
                ;grid.lisp
                :grid
                ;group.lisp
                :group
                :group-get-canvas
                :draw-group
                :draw-canvas-frame
                ;image.lisp
                :image
                ;kaavio.lisp
                :make-id
                :fix-name
                :exception
                :caution
                :throw-exception
                :throw-caution
                :type-assert
                :chk-type
                :format-string
                :check-member
                :check-object
                :check-keywords
                :check-numbers
                :write-when
                :it
                :with-dictionary
                :attr
                :escape-characters
                :to-property-strings
                :check
                :rgb
                :repeat
                :with-options
                ;label-info.lisp
                :label-info
                :draw-label-with-point
                :draw-label
                :make-label
                :with-label-options
                ;layer-manager.lisp
                :layer-manager
                ;line.lisp
                :line-get-center
                :line
                ;link-info.lisp
                :link-info
                :write-link-open
                :write-link-close
                :make-link
                ;mathutil.lisp
                :math/len2
                :math/len4
                :math/sin1
                :math/sin2
                :math/sin3
                :math/sin4
                :math/sin5
                :math/cos1
                :math/cos2
                :math/cos3
                :math/cos4
                :math/cos5
                :math/intersection-point
                ;memo.lisp
                :*default-memo-crease*
                :*default-memo-align*
                :*default-memo-valign*
                :*default-memo-margin*
                :*default-memo-font*
                :*default-memo-fill*
                :*default-memo-fill2*
                :*default-memo-stroke*
                :*default-memo-filter*
                :*default-memo-layer*
                :memo
                :with-memo-options
                ;paragraph.lisp
                :paragraph
                :with-paragraph-options
                ;parallelogram.lisp
                :parallelogram-connect-point
                :parallelogram
                ;path.lisp
                :path
                ;pathutil.lisp
                ;person.lisp
                :*default-person-fill*
                :*default-person-stroke*
                :*default-person-filter*
                :*default-person-layer*
                :person
                :with-person-options
                ;point.lisp
                :make-point
                :copy-point
                :point-p
                :point-absolute-p
                :point-relative-p
                :point-x
                :point-y
                :pt+
                :pt-
                :pt*
                :pt/
                :point+
                :point-
                :point*
                :point/
                :point/x+
                :point/y+
                :point/xy+
                :x+
                :y+
                :xy+
                :point-distance
                :with-point
                ;polygon.lisp
                :polygon
                ;raw-svg.lisp
                :raw-svg
                ;rectangle.lisp
                :rectangle
                :rect
                ;regular-polygon.lisp
                :regular-polygon
                ;shadow-filter.lisp
                :drop-shadow
                :glow-shadow
                ;shape.lisp
                :rectangle-connect-point
                :shape
                :shape-get-subcanvas
                :shape-cc-center
                :shape-connect-point
                ;stencil.lisp
                :*include-paths*
                :*stencil-suffix*
                :reset-stencil-load-cache
                :load-stencil
                ;stroke-info.lisp
                :*default-stroke*
                :*mute-stroke*
                :stroke-info
                :make-stroke
                :make-stroke2
                ;table.lisp
                :*default-table-font*
                :*default-table-stroke*
                :*default-table-fill*
                :*default-table-layer*
                :table
                :with-table-options
                :with-table-cell
                :with-table-range
                ;text-shape.lisp
                :text-shape
                :text-shape-calc-size
                :text-shape-paragraph-area
                ;text.lisp
                :write-text-tag
                :text
                ;textbox.lisp
                :*default-textbox-rx*
                :*default-textbox-ry*
                :*default-textbox-align*
                :*default-textbox-valign*
                :*default-textbox-margin*
                :*default-textbox-font*
                :*default-textbox-fill*
                :*default-textbox-stroke*
                :*default-textbox-filter*
                :*default-textbox-layer*
                :textbox
                :with-textbox-options
                ;theme.lisp
                :register-theme
                :with-theme
                ;use.lisp
                :use
                ;writer.lisp
                :writer-write
                :writer-incr-level
                :writer-decr-level
                :writer-close
                :buffer-writer
                :create-svg-writer
                ;--------------- END EXPORT
))


(defpackage     :kaavio-user
  (:use         :common-lisp
                :kaavio)
  (:nicknames   :kaavio-user))


(in-package :kaavio)


;;------------------------------------------------------------------------------
;;
;; string manipulation utilities
;;
;;------------------------------------------------------------------------------
;;(locally (declare (optimize speed))
;;  (defun left (str len)
;;  (declare (type string str))
;;  (declare (type fixnum len))
;;  (subseq str 0 len)))
;;
;;(locally (declare (optimize speed))
;;  (defun right (str len)
;;  (declare (type string str))
;;  (declare (type fixnum len))
;;  (subseq str (- (length str) len))))

(locally (declare (optimize speed))
  (defun string/mid (str idx &optional (len -1))
    (declare (type string str))
    (declare (type fixnum idx len))
    (if (< len 0)
        (subseq str idx)
        (subseq str idx (+ idx len)))))

(locally (declare (optimize speed))
  (defun string/replace (target str1 str2)
    (declare (type string target str1 str2))
    (let ((len1 (length str1))
          (len2 (length str2))
          (target-len (length target)))
      (declare (type fixnum len1 len2 target-len))
      (labels ((imp (idx acc)
                 (declare (type fixnum idx))
                 (if (< (- target-len idx) len1)
                     (push (subseq target idx) acc)
                     (let ((ret (search str1 target :start2 idx)))
                       (if (null ret)
                           (push (subseq target idx) acc)
                           (progn
                             (when (< idx ret)
                               (push (subseq target idx ret) acc))
                             (push str2 acc)
                             (imp (+ ret len1) acc)))))))
        (if (and (= len1 1) (= len2 1))
            (substitute (aref str2 0) (aref str1 0) target)
            (apply #'concatenate 'string (nreverse (imp 0 nil))))))))

; example : (string/split "abc,def,ghi" #\,) => ("abc" "def" "ghi")
(locally (declare (optimize speed))
  (defun string/split (line separator)
    (declare (type string line))
    (let ((acc nil))
      (do ((idx1 0)
           (idx2 0))
          ((null idx1) (nreverse acc))
        (setf idx2 (position separator line :start idx1))
        (if (null idx2)
            (push (string/mid line idx1) acc)
            (progn
              (push (string/mid line idx1 (- idx2 idx1)) acc)
              (incf idx2)))
        (setf idx1 idx2)))))

;;------------------------------------------------------------------------------
;;
;; utilities from 'on lisp' and their variant.
;;
;;------------------------------------------------------------------------------
(defun onlisp/mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args)
      (princ a s))))

(defun onlisp/symb (&rest args)
  (values (intern (apply #'onlisp/mkstr args) 'kaavio)))

(defun onlisp/keysymb (&rest args)
  (values (intern (apply #'onlisp/mkstr args) :keyword)))

(defun onlisp/flatten (x &optional (acc nil))
  (cond ((null x) acc)
        ((atom x) (cons x acc))
        (t (onlisp/flatten (car x) (onlisp/flatten (cdr x) acc)))))





;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function make-id
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{make-id}} prefix ${REST} args => result
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `prerix` ---- 文字列またはシンボルを指定します。
;;* `args` ---- 任意の数の文字列またはシンボルを指定します。
;;* `result` ---- 結果がキーワードシンボルで返ります。
;;
;;${DESCRIPTION}
;;
;;　`prefix` および追加の `args` を連結した ID を生成し、キーワードシンボルのかたちで
;;返します。
;;
;;${NO_SEE_ALSO}
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :make-id
 |#
(defun make-id (prefix &rest args)
  (let ((name (with-output-to-string (stream)
                (dolist (itm (cons prefix args))
                  (format stream "~A" (if (stringp itm)
                                          (string-upcase itm) itm))))))
    (intern name 'keyword)))

#|
#|EXPORT|#                :fix-name
 |#
(defun fix-name (name &optional no-multiline)
  (setf name (if (symbolp name)
                 (string-downcase (symbol-name name)) name))
  (unless (stringp name)
    (error "fix-name : invalid data type."))
  (if no-multiline
      name
      (string/replace name "~%" "
")))



(defun __write-imp (stream itm)
  (when itm
    (if (keywordp itm)
        (princ (string-downcase (symbol-name itm)) stream)
        (if (and (numberp itm) (not (integerp itm)))
            (format stream "~F" (coerce itm 'single-float))
            (format stream "~A" itm)))))


#|
#|EXPORT|#                :exception
#|EXPORT|#                :caution
#|EXPORT|#                :throw-exception
#|EXPORT|#                :throw-caution
#|EXPORT|#                :type-assert
#|EXPORT|#                :chk-type
 |#
(define-condition exception (cl:error)
  ((msg :initarg  :msg :accessor exception-msg))
  (:report (lambda (condition stream)
             (write-string (exception-msg condition) stream))))

(define-condition caution (cl:warning)
  ((msg :initarg  :msg :accessor caution-msg))
  (:report (lambda (condition stream)
             (write-string (caution-msg condition) stream))))

(defmacro throw-exception (fmt &rest args)
  `(error (make-condition 'exception :msg (format nil ,fmt ,@args))))

(defmacro throw-caution (fmt &rest args)
  `(warn (make-condition 'caution :msg (format nil ,fmt ,@args))))

(defmacro type-assert (symbol type)
  `(check-type ,symbol ,type))

(defmacro chk-type (symbol type &optional (name nil))
  (let ((fmt (format nil "The value of ~A is ~~A, which is not of type ~A." (or name symbol) type)))
    `(unless (typep ,symbol ',type)
       (throw-exception ,fmt ,symbol))))





#|
#|EXPORT|#                :format-string
 |#
(defun format-string (&rest args)
  (with-output-to-string (stream)
    (dolist (itm args)
      (__write-imp stream itm))))

;;#|
;;#|EXPORT|#              :fix-member-when-nil
;; |#
;;(defmacro fix-member-when-nil (member value &optional (base nil) (kwd nil))
;;  (type-assert member symbol)
;;  (if (null base)
;;    `(unless ,member (setf ,member ,value))
;;    (progn
;;      (type-assert base symbol)
;;      (type-assert kwd  symbol)
;;      `(unless ,member
;;         (setf ,member (or (and ,base (class:member ,base ,kwd)) ,value))))))

#|
#|EXPORT|#                :check-member
#|EXPORT|#                :check-object
#|EXPORT|#                :check-keywords
#|EXPORT|#                :check-numbers
 |#
(defmacro check-member (sym &key (nullable nil) (types nil))
  (type-assert sym symbol)
  (if (not nullable)
      `(if (null ,sym)
           (throw-exception ,(format nil "~A is nil." sym))
           (chk-type ,sym ,types))
      `(when ,sym
         (chk-type ,sym ,types))))

(defmacro check-object (sym canvas dict &key (nullable nil) (class nil))
  (type-assert sym symbol)
  (type-assert canvas symbol)
  (type-assert dict   symbol)
  (type-assert class  symbol)
  `(,@(if nullable `(when ,sym) `(progn))
      (unless (typep ,sym ',class)
        (throw-exception ,(format nil "parameter ~A is not ~A object." sym class)))
      (check ,sym ,canvas ,dict)))

(defmacro check-keywords (sym &rest choices)
  (type-assert sym symbol)
  `(unless (or ,@(mapcar (lambda (v)
                           (type-assert v keyword)
                           `(eq ,sym ,v)) choices))
     (throw-exception ,(format nil "~A must be in ~A." sym choices))))

(defmacro check-numbers (sym &rest choices)
  (type-assert sym symbol)
  `(unless (or ,@(mapcar (lambda (v)
                           (type-assert v number)
                           `(= ,sym ,v)) choices))
     (throw-exception ,(format nil "~A must be in ~A." sym choices))))

#|
#|EXPORT|#                :write-when
#|EXPORT|#                :it
 |#
(defmacro write-when (item &rest args)
  `(let ((it ,item))
     (when it
       (format-string ,@args))))


#|
#|EXPORT|#                :with-dictionary
#|EXPORT|#                :attr
 |#
(defmacro with-dictionary (dict &rest body)
  (type-assert dict symbol)
  (labels ((property-ref-symbolp (x)
             (when (and (symbolp x) (not (keywordp x)))
               (let* ((name (symbol-name x))
                      (pos (position #\. name)))
                 (when pos
                   (and (< 0 pos)
                        (< pos (1- (length name))))))))
           (make-symbol-macrolet (sym)
             (let* ((name (symbol-name sym))
                    (pos  (position #\. name))
                    (id   (subseq name 0 pos)))
               (if (string= id "CANVAS")
                   (let ((id-sym (onlisp/symb (subseq name 0 pos)))
                         (method (onlisp/symb "CANVAS-DICT-" (subseq name (1+ pos)))))
                     `(,sym (,method ,id-sym)))
                   (let ((id-kwd (onlisp/keysymb (subseq name 0 pos)))
                         (method (onlisp/symb "ATTRIBUTE-" (subseq name (1+ pos)))))
                     `(,sym (,method (dict-get-entity ,dict ,id-kwd))))))))
    (let ((syms (remove-duplicates
                 (remove-if-not #'property-ref-symbolp
                                (kaavio::onlisp/flatten body)))))
      `(symbol-macrolet ,(mapcar #'make-symbol-macrolet syms)
         (labels ((get-dictionary () ,dict))
           (declare (ignorable #'get-dictionary))
           (macrolet ((attr (id name)
                        (let ((func-sym (kaavio::onlisp/symb "ATTRIBUTE-" name)))
                          (list ,'func-sym (list 'kaavio::dict-get-entity ',dict id)))))
             ,@body))))))

#|
#|EXPORT|#                :escape-characters
 |#
(defun escape-characters (str)
  (setf str (string/replace str "&"  "&amp;"))
  (setf str (string/replace str "<"  "&lt;"))
  (setf str (string/replace str ">"  "&gt;"))
  (setf str (string/replace str "\"" "&quot;"))
  (setf str (string/replace str "'"  "&#x27;"))
  str)


#|
#|EXPORT|#                :to-property-strings
 |#
(defgeneric to-property-strings (info))

#|
(let ((lnk (make-link "http://www.google.co.jp/"))
      (writer (make-instance 'buffer-writer)))
  (write-link-open lnk writer)
  (writer-write writer "test")
  (write-link-close lnk writer)
  (writer-close writer))
|#

#|
#|EXPORT|#                :check
 |#
(defgeneric check (obj canvas dict))


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function rgb
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{rgb}} r g b => result
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `r` ---- 0 ～ 255 の整数値または 0.0 ～ 1.0 の浮動小数点値
;;* `g` ---- 0 ～ 255 の整数値または 0.0 ～ 1.0 の浮動小数点値
;;* `b` ---- 0 ～ 255 の整数値または 0.0 ～ 1.0 の浮動小数点値
;;* `result` ---- 結果が `"#RRGGBB"` 形式の文字列で返ります。
;;
;;${DESCRIPTION}
;;
;;　`r g b` の 3 値から色指定の文字列を生成指定返します。
;;
;;${SEE_ALSO}
;;
;;* [](#色の指定)
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :rgb
 |#
(defun rgb (r g b)
  (labels ((fix (n)
             (let ((n (if (floatp n)
                          (round (* 255 n)) n)))
               (if (not (numberp n))
                   0
                   (if (< 255 n) 255 n)))))
    (format nil "#~2,'0x~2,'0x~2,'0x" (fix r) (fix g) (fix b))))


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function repeat
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{repeat}} value count ${REST} customs => result
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `value` ---- 数値、または一引数関数を指定します。
;;* `count` ---- 数値を指定します。
;;* `customs` ---- `(n v)` 形式のリストを任意数指定します。ここで `n v` はともに整数です。
;;* `result` ---- 結果がリストで返ります。
;;
;;${DESCRIPTION}
;;
;;　`value count customs` に従って数値のリストを生成して返します。table マクロにおける
;;`rows` および `cols` パラメータでの使用を想定しています。
;;
;;　基本的には、この関数は `count` 個の `value` からなるリストを生成します。ただし、
;;`customs` が指定されている場合、生成したリストを `customs` に従って変更します。
;;`customs` は、 `(n v)` 形式のリストの羅列であることが期待され、これによって結果の 
;;`n` 番目の要素が `v` に置き換えられます。
;;
;;　さらに、 `value` は数値ではなく一引数関数にすることもできます。この場合、その関数は
;;結果リストの個々の値を生成するためにインデックスを引数としてコールされます。以下に
;;それぞれの例を示します。
;;
;;```lisp
;;(repeat 50 4)                 ; => '(50 50 50 50)
;;(repeat 40 5 '(0 80) '(3 60)) ; => '(80 40 40 60 40)
;;(repeat #'identity 10)        ; => '(0 1 2 3 4 5 6 7 8 9)
;;(repeat (lambda (i)
;;           (* 10 (1+ i))) 10)  ; => '(10 20 30 40 50 60 70 80 90 100)
;;```
;;
;;${SEE_ALSO}
;;
;;* table マクロ
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :repeat
 |#
(defun repeat (value count &rest customs)
  (let ((gen (if (functionp value)
                 value
                 (constantly value))))
    (labels ((impl (i acc)
               (if (= count i)
                   (nreverse acc)
                   (impl (1+ i) (push (funcall gen i) acc)))))
      (let ((lst (impl 0 nil)))
        (mapcar (lambda (pair)
                  (setf (nth (first pair) lst) (second pair))) customs)
        lst))))


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro with-options
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{with-options}} (${KEY} fill stroke font filter layer) ${BODY} body
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `fill` ---- 内部の塗り潰しを指定します。
;;* `stroke` ---- 外枠を描画するストロークを指定します。
;;* `font` ---- フォントを指定します。
;;* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します。
;;* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します。
;;
;;${DESCRIPTION}
;;
;;　ストローク、塗り潰し、フォントなどのデフォルト設定を変更します。
;;
;;${NO_SEE_ALSO}
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :with-options
 |#
(defmacro with-options ((&key (fill   nil fill-p)
                              (stroke nil stroke-p)
                              (font   nil font-p)
                              (filter nil filter-p)
                              (layer  nil layer-p)) &rest body)
  (let ((bindings nil))
    (labels ((impl (arg-p binding)
               (when arg-p (push binding bindings))))
      (impl fill-p   `(*default-fill*   (make-fill2   *default-fill*   ,fill)))
      (impl stroke-p `(*default-stroke* (make-stroke2 *default-stroke* ,stroke)))
      (impl font-p   `(*default-font*   (make-font2   *default-font*   ,font)))
      (impl filter-p `(*default-filter* ,filter))
      (impl layer-p  `(*default-layer*  ,layer)))
    `(let ,(nreverse bindings)
       ,@body)))


