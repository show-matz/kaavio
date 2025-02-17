#|
#|ASD|#                (:file "table"                     :depends-on ("kaavio"
#|ASD|#                                                                "constants"
#|ASD|#                                                                "group"
#|ASD|#                                                                "font-info"
#|ASD|#                                                                "fill-info"
#|ASD|#                                                                "stroke-info"
#|ASD|#                                                                "writer"))
#|EXPORT|#                ;table.lisp
 |#


(in-package :kaavio)

;; :rN => (values N N nil nil)
;; :cN => (values nil nil N N)
;; :rNcM => (values N N M M)
;; :rN-M => (values N M nil nil)
;; :cN-M => (values nil nil N M)
;; :rJ-KcN-M => (values J K N M)
;; ToDo : :r0c1r2 みたいのがエラーにならない
(defun table-destructure-rc-keyword (kwd)
  (labels ((recur (mode lst acc)
             (if (null lst)
                 (nreverse acc)
                 (let ((item (car lst)))
                   (cond
                     ((char= #\R item) (recur :R (cdr lst) (push :R1 acc)))
                     ((char= #\C item) (recur :C (cdr lst) (push :C1 acc)))
                     ((char= #\- item) (cond
                                         ((eq mode :R) (recur :R  (cdr lst) (push :R2 acc)))
                                         ((eq mode :C) (recur :C  (cdr lst) (push :C2 acc)))
                                         (t            (recur nil (cdr lst) acc))))
                     (t (let ((code (char-code item)))
                          (if (not (<= (char-code #\0) code (char-code #\9)))
                              (recur mode (cdr lst) (push item acc))
                              (progn
                                (unless (integerp (car acc))
                                  (push 0 acc))
                                (setf (car acc) (+ (* (car acc) 10) (- code (char-code #\0))))
                                (recur mode (cdr lst) acc))))))))))
    (handler-case
        (destructuring-bind (&key r1 r2 c1 c2)
            (recur nil (coerce (symbol-name kwd) 'list) nil)
          (values r1 (or r2 r1)
                  c1 (or c2 c1)))
      (t () (values nil nil nil nil)))))
                           

(labels ((recur (idx lst acc)
           (if (null lst)
               (values nil nil)
               (let ((n (car lst)))
                 (if (zerop idx)
                     (values n (+ acc (/ n 2)))
                     (recur (1- idx) (cdr lst) (+ acc n))))))
         (recur-range (idx1 idx2 lst pre acc)
           (if (zerop idx2)
               (values acc (+ pre (/ acc 2)))
               (if (null lst)
                   (values nil nil)
                   (let ((n (car lst)))
                     (recur-range (1- idx1) (1- idx2) (cdr lst)
                                  (+ pre (if (< 0 idx1) n 0))
                                  (+ acc (if (and (<= idx1 0) (< 0 idx2)) n 0))))))))

  (defun table-get-cell-area (r c rows cols)
    (multiple-value-bind (w x) (recur c cols 0)
      (multiple-value-bind (h y) (recur r rows 0)
        (values (list x y) w h))))

  ;; returns (values center-pt width height)
  (defun table-get-sub-area (kwd center rows cols topleft)
    (if (eq :rc kwd)
        (values center (apply #'+ cols) (apply #'+ rows))
        (multiple-value-bind (r1 r2 c1 c2) (table-destructure-rc-keyword kwd)
          (if (not (or r1 c1))
              (values nil nil nil)
              (labels ((impl (idx1 idx2 lst)
                         (if (not (and idx1 idx2))
                             (let ((total (apply #'+ lst)))
                               (values total (/ total 2)))
                             (cond
                               ((= idx1 idx2) (recur idx1 lst 0))
                               ((< idx1 idx2) (recur-range idx1 (1+ idx2) lst 0 0))
                               ((< idx2 idx1) (recur-range idx2 (1+ idx1) lst 0 0))))))
                (multiple-value-bind (w x) (impl c1 c2 cols)
                  (multiple-value-bind (h y) (impl r1 r2 rows)
                    (if (not (and x y w h))
                        (values nil nil nil)
                        (values (xy+ topleft x y) w h))))))))))

(defun table-normalize-texts (texts)
  (labels ((fix-data (data)
             (if (null data)
                 nil   ;; nil means 'no text cell'
                 (destructuring-bind (d &key font align valign)
                                     (if (listp data) data (cons data nil))
                   ;; auto aligning when no align specified.
                   (unless align
                     (setf align (typecase d
                                   (number  :right)
                                   (symbol  :center)
                                   (keyword :center)
                                   (t       :left))))
                   `(,(if (stringp d)
                          d
                          (format nil "~A" d))
                      ,align ,(or valign :center) ,font))))
           (row-impl (lst acc)
             (if (null lst)
                 (nreverse acc)
                 (row-impl (cdr lst)
                           (push (fix-data (car lst)) acc))))
           (tbl-impl (lst acc)
             (if (null lst)
                 (nreverse acc)
                 (tbl-impl (cdr lst)
                           (push (row-impl (car lst) nil) acc)))))
    (tbl-impl texts nil)))


(defun table-fix-text (pt w h lst default-font)
  (destructuring-bind (txt align valign font) lst
    (let* ((fnt  (or font default-font))
           (fs/2 (/ (slot-value fnt 'size) 2))
           (pivot (case align
                    ((:center) (y+  pt fs/2))
                    ((:right)  (xy+ pt (- (/ w 2) fs/2) fs/2))
                    ((:left)   (xy+ pt (- fs/2 (/ w 2)) fs/2)))))
      (case valign
        ((:bottom) (setf (point-y pivot) (+ (point-y pt)    (/ h 2)  (- fs/2))))
        ((:top)    (setf (point-y pivot) (+ (point-y pt) (- (/ h 2)) (*  3 fs/2)))))
      (values pivot txt align font))))


#|
#|EXPORT|#                :*default-table-font*
#|EXPORT|#                :*default-table-stroke*
#|EXPORT|#                :*default-table-fill*
#|EXPORT|#                :*default-table-layer*
 |#
(defparameter *default-table-font*   nil)
(defparameter *default-table-stroke* nil)
(defparameter *default-table-fill*   nil)
(defparameter *default-table-layer*  nil)

;;-------------------------------------------------------------------------------
;;
;; class table
;;
;;-------------------------------------------------------------------------------
(defclass table (group)
  ((rows    :initform nil :initarg :rows)       ; list of integers
   (cols    :initform nil :initarg :cols)       ; list of integers
   (font    :initform nil :initarg :font)       ; (or nil font-info)
   (stroke  :initform nil :initarg :stroke)     ; (or nil stroke-info)
   (fills   :initform nil :initarg :fills)      ; list
   (texts   :initform nil :initarg :texts)))    ; list


(defmethod initialize-instance :after ((tbl table) &rest initargs)
  (declare (ignore initargs))
  (labels ((fix-fills (lst acc)
             (if (null lst)
                 (nreverse acc)
                 (progn
                   (push (car lst) acc)
                   (push (make-fill (cadr lst)) acc)
                   (fix-fills (cddr lst) acc)))))
    (with-slots (font stroke fills layer) tbl
      (setf font   (make-font   (or font   *default-table-font*   *default-font*)))
      (setf stroke (make-stroke (or stroke *default-table-stroke* *default-stroke*)))
      (when (and (null fills) *default-table-fill*)
        (setf fills (list :rc *default-table-fill*)))
      (when (and (null fills) *default-fill*)
        (setf fills (list :rc *default-fill*)))
      (setf fills  (fix-fills fills nil))
      (setf layer  (if (eq layer :none)
                       nil
                       (or layer *default-table-layer* *default-layer*)))))
  tbl)

(defmethod check ((tbl table) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (font fills stroke texts) tbl
    (labels ((chk-fills (lst)
               (when lst
                 (let ((fill (cadr fills)))
                   (check-object fill canvas dict :nullable nil :class fill-info))
                 (chk-fills (cdr lst)))))
      (chk-fills fills))
    (check-object font      canvas dict :nullable t :class font-info)
    (check-object stroke    canvas dict :nullable t :class stroke-info)
    (setf texts (table-normalize-texts texts)))
  nil)

;; override of group::draw-group
(defmethod draw-group ((tbl table) writer)
  (let ((canvas (group-get-canvas tbl)))
    (let ((width  (canvas-width  canvas))
          (height (canvas-height canvas)))
      (macrolet ((register-entity (entity)
                   `(check-and-draw-local-entity ,entity canvas writer)))
        (with-slots (rows cols stroke fills font texts) tbl
          (let ((center (attribute-center tbl)))
            ;; filling ----------------------------------------------
            ;; 「同じ色指定が連続するなら g tag でまとめたい」が、keyword とは限らないので断念
            (unless (and (= 2 (length fills))
                         (eq :none (slot-value (cadr fills) 'kaavio::color)))
              (writer-write writer "<g stroke='none'>")
              (writer-incr-level writer)
              (let ((*mute-stroke* t))
                (labels ((fill-impl (lst)
                           (when lst
                             (let ((kwd  (car  lst))
                                   (info (cadr lst)))
                               (multiple-value-bind (c w h)
                                   (table-get-sub-area kwd center
                                                       rows cols (canvas-topleft canvas))
                                 (when (and c w h)
                                   (rect c w h :fill info :stroke :none)))
                               (fill-impl (cddr lst))))))
                  (fill-impl fills)))
              (writer-decr-level writer)
              (writer-write writer "</g>"))
            ;; draw lines -------------------------------------------
            ;; ToDo : lines の custom drawing をサポートしたい（これは壮大）
            (unless (eq :none (slot-value stroke 'color))
              (writer-write writer "<g " (to-property-strings stroke) " fill='none'>")
              (writer-incr-level writer)
              (let ((*mute-fill*   t)
                    (*mute-stroke* t))
                (rect center (reduce #'+ cols)  (reduce #'+ rows) :fill :none :stroke stroke)
                (let ((y 0))
                  (dolist (r (butlast rows))
                    (incf y r)
                    (line `((0 ,y) (,width ,y)) :stroke stroke)))
                (let ((x 0))
                  (dolist (c (butlast cols))
                    (incf x c)
                    (line `((,x ,0) (,x ,height)) :stroke stroke))))
              (writer-decr-level writer)
              (writer-write writer "</g>"))
            ;; draw texts -------------------------------------------
            (when texts
              (writer-write writer "<g " (to-property-strings font) " >")
              (writer-incr-level writer)
              (let ((r 0))
                (dolist (row texts)
                  (let ((c 0))
                    (dolist (data row)
                      (when data
                        (multiple-value-bind (pt w h) (table-get-cell-area r c rows cols)
                          (multiple-value-bind (pt txt align fnt) (table-fix-text pt w h data font)
                            (let ((*mute-font* (null fnt)))
                              (text pt txt :align align :font (or fnt font))))))
                      (incf c)))
                  (incf r)))
              (writer-decr-level writer)
              (writer-write writer "</g>")))))))
  nil)
  

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro table
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{table}} position rows cols ${KEY} pivot font fills stroke texts layer id
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `rows` ---- 行数と各行の高さを数値のリストで指定します。リストの長さが行数、リスト要素の数値が行の高さです。[repeat 関数](#function repeat)が役に立つかもしれません。
;;* `cols` ---- 列数と各列の幅を数値のリストで指定します。リストの長さが列数、リスト要素の数値が列の幅です。[repeat 関数](#function repeat)が役に立つかもしれません。
;;* `pivot` ---- 基準点がテーブルのどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
;;* `stroke` ---- 罫線を描画する線を指定します。
;;* `fills` ---- 表、行、列、またはセル個別の塗り潰しを指定します。詳細は後述します。
;;* `font` ---- 表内でテキストを描画する際に使用するフォントを指定します。省略した場合、 `*default-table-font*, *default-font*` の順でデフォルトフォントが使用されます。また、 `texts` パラメータ指定の中でセル毎に個別にフォントを指定することもできます。
;;* `texts` ---- 表内の各セルに設定するテキストをリストで指定します。正確には、行のリストを連ねたリストで指定します。テキスト情報はキーワードなどのシンボル、数値、文字列を指定できますが、アライメントやフォント情報を指定する場合は テキスト情報自体をリストにする必要があります。
;;* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
;;* `id` ---- ID を付与したい場合、その名前をキーワードで指定します。
;;
;;${DESCRIPTION}
;;
;;　テーブルを描画します。複数のテーブルでスタイルを統一したい場合、macro with-table-options を
;;使うことができます。
;;
;;${SEE_ALSO}
;;
;;* テーブル
;;* macro with-table-options
;;* macro with-table-cell
;;* macro with-table-range
;;
;;${NOTES}
;;
;;　`fills` パラメータは、位置を示すキーワードとフィル情報の２つの値を繰り返すリストで指定して
;;ください。位置は、表全体であれば `:rc` 、列や行全体を指定する場合は `:rN` や `:cM` を指定
;;します。ここで、 `N,M` は行や列の番号を示す整数です（上または左から０で始まります）。 
;;`:rN-M` といった範囲指定も可能です。単独のセルを指定する場合、同じ要領で `:rNcM` と指定して
;;ください。 `:rK-LcN-M` 形式の範囲指定も可能です。 `fills` パラメータ全体が省略された場合、
;;表の背景は塗り潰されません。
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :table
 |#
(defmacro table (position rows cols &key pivot font fills stroke texts layer id)
  `(register-entity (make-instance 'kaavio:table
                                   :position ,position :pivot ,pivot
                                   :width  (apply #'+ ,cols)
                                   :height (apply #'+ ,rows)
                                   :rows ,rows :cols ,cols
                                   :stroke ,stroke :fills ,fills
                                   :font ,font :texts ,texts :layer ,layer :id ,id)))



;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro with-table-options
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{with-table-options}} (${KEY} font fill stroke layer) ${BODY} body
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
#|EXPORT|#                :with-table-options
 |#
(defmacro with-table-options ((&key font fill stroke layer) &rest body)
  (labels ((impl (params acc)
             (if (null params)
                 acc
                 (let ((value  (car  params))
                       (symbol (cadr params)))
                   (impl (cddr params)
                         (if (null value)
                             acc
                             (push (list symbol value) acc)))))))
    (let ((lst (impl (list font   '*default-table-font*
                           fill   '*default-table-fill*
                           stroke '*default-table-stroke*
                           layer  '*default-table-layer*) nil)))
      `(let ,lst
         ,@body))))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro with-table-cell
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{with-table-cell}} (id r c) ${BODY} body
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
#|EXPORT|#                :with-table-cell
 |#
(defmacro with-table-cell ((id r c) &body body)
  (let ((g-tbl     (gensym "TBL"))
        (g-center  (gensym "CENTER"))
        (g-width   (gensym "WIDTH"))
        (g-height  (gensym "HEIGHT"))
        (g-topleft (gensym "TOPLEFT")))
    `(let ((,g-tbl (kaavio::dict-get-entity (kaavio::get-dictionary) ,id)))
       (multiple-value-bind (,g-center ,g-width ,g-height)
                (kaavio::table-get-cell-area ,r ,c (slot-value ,g-tbl 'kaavio::rows)
                                                    (slot-value ,g-tbl 'kaavio::cols))
         (let* ((,g-topleft (point+ (attribute-topleft ,g-tbl)
                                   (xy+ ,g-center (- (/ ,g-width 2)) (- (/ ,g-height 2)))))
                (canvas (make-canvas ,g-topleft ,g-width ,g-height)))
           (declare (special canvas))
           ,@body)))))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro with-table-range
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{with-table-range}} (id kwd) ${BODY} body
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
#|EXPORT|#                :with-table-range
 |#
(defmacro with-table-range ((id kwd) &body body)
  (let ((g-tbl     (gensym "TBL"))
        (g-center  (gensym "CENTER"))
        (g-width   (gensym "WIDTH"))
        (g-height  (gensym "HEIGHT"))
        (g-topleft (gensym "TOPLEFT")))
    `(let ((,g-tbl (kaavio::dict-get-entity (kaavio::get-dictionary) ,id)))
       (multiple-value-bind (,g-center ,g-width ,g-height)
                (kaavio::table-get-sub-area ,kwd (kaavio:attribute-center ,g-tbl)
                                                 (slot-value ,g-tbl 'kaavio::rows)
                                                 (slot-value ,g-tbl 'kaavio::cols)
                                                 (attribute-topleft ,g-tbl))
         (let* ((,g-topleft (xy+ ,g-center (- (/ ,g-width 2)) (- (/ ,g-height 2))))
                (canvas (make-canvas ,g-topleft ,g-width ,g-height)))
           (declare (special canvas))
           ,@body)))))

