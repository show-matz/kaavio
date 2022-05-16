<!-- define: APPNAME = diagram -->
<!-- define: BLANK_PARAGRAPH = '　　' -->
<!-- define: TODO = '@((background:red;color:white;)(ToDo : %1))' -->

<!-- title:${APPNAME} internal -->    
<!-- style:./default.css -->			
<!-- <!-- config:term-link-in-header -->			

<!-- filter:diagram  = bash ./diagram.sh  %in %out -->
<!-- filter:plantuml = bash ./plantuml.sh %in %out -->

<!-- config:write-comment -->			
<!-- config:header-numbering 2 5 -->

# ${APPNAME} internal documents

　この文書は、 **${APPNAME}** の内部設計文書です。

## Table of contents

<!-- embed:toc-x 2 5 -->
<!-- toc-link: top 'Table of contents' -->

${BLANK_PARAGRAPH}

## 概念
### 座標系

　${APPNAME} の座標系は、左上を原点としています。そこから水平右方向に x 軸、垂直
下方向に y 軸です。角度は時計回りになります。

```diagram
(diagram (:w 300 :h 100)
  (grid)
  (circle '(  0  0) 3 :fill :red :stroke :none)
  (text   '(  5 15) "(0 0)")
  (text   '(260 15) "→ x")
  (text   '(  5 90) "↓ y")
  (with-stroke (:color :gray :width 1)
    (line  '((100 30) (150 30)))
    (line  '((100 30) (120 80))))
  (with-stroke (:color :black :width 1)
    (arc    '(100 30) 40 0 68)
    (line  '((115 67) (118 61)))
    (line  '((115 67) (122 67))))
  (text   '(140 60) "θ"))
```

### キャンバス
<!-- autolink: [$$](#キャンバス) -->

　${APPNAME} におけるキャンバスとは、点 `top-left` と幅、高さを持つ矩形です。

　${{TODO}{サブキャンバスなどについても説明しないと}}

### エンティティ
<!-- autolink: [$$](#エンティティ) -->

　${{TODO}{まだ記述されていません}}

### ディクショナリ
<!-- autolink: [$$](#ディクショナリ) -->

　ディクショナリは、エンティティを ID で管理するためのクラスです。また、$1 などの履歴でアクセスする
ための履歴機能も提供します。

　${{TODO}{まだ記述されていません}}

## src 配下のコンポーネント
### arc.lisp
<!-- autolink: [$$](#arc.lisp) -->

　arc.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp
* point.lisp
* canvas.lisp
* mathutil.lisp
* path.lisp

### binutil.lisp
<!-- autolink: [$$](#binutil.lisp) -->

　binutil.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp

### canvas.lisp
<!-- autolink: [$$](#canvas.lisp) -->

　canvas.lisp はキャンバスを実装します。以下のコンポーネントに依存しています。

* cl-diagram.lisp
* point.lisp

#### make-canvas 関数
<!-- autolink: [make-canvas](#make-canvas 関数) -->

```lisp
(defun make-canvas (top-left width height) ... )
```

　${{TODO}{まだ記述されていません。}}

#### copy-canvas 関数
<!-- autolink: [copy-canvas](#copy-canvas 関数) -->

```lisp
(defun copy-canvas (canvas) ... )
```

　${{TODO}{まだ記述されていません。}}

#### canvas-p 関数
<!-- autolink: [canvas-p](#canvas-p 関数) -->

```lisp
(defun canvas-p (canvas) ... )
```

　${{TODO}{まだ記述されていません。}}

#### canvas-topleft 関数
<!-- autolink: [canvas-topleft](#canvas-topleft 関数) -->

```lisp
(defun canvas-topleft (canvas) ... )
```

　${{TODO}{まだ記述されていません。}}

#### canvas-left 関数
<!-- autolink: [canvas-left](#canvas-left 関数) -->

```lisp
(defun canvas-left    (canvas) ... )
```

　${{TODO}{まだ記述されていません。}}

<!-- (defun (setf canvas-left) (val canvas) -->

#### canvas-top 関数
<!-- autolink: [canvas-top](#canvas-top 関数) -->

```lisp
(defun canvas-top     (canvas) ... )
```

　${{TODO}{まだ記述されていません。}}

<!-- (defun (setf canvas-top)  (val canvas) -->

#### canvas-right 関数
<!-- autolink: [canvas-right](#canvas-right 関数) -->

```lisp
(defun canvas-right   (canvas) ... )
```

　${{TODO}{まだ記述されていません。}}

#### canvas-bottom 関数
<!-- autolink: [canvas-bottom](#canvas-bottom 関数) -->

```lisp
(defun canvas-bottom  (canvas) ... )
```

　${{TODO}{まだ記述されていません。}}

#### canvas-width 関数
<!-- autolink: [canvas-width](#canvas-width 関数) -->

```lisp
(defun canvas-width  (canvas) ... )
```

　${{TODO}{まだ記述されていません。}}

<!-- (defun (setf canvas-width)  (val canvas) -->

#### canvas-height 関数
<!-- autolink: [canvas-height](#canvas-height 関数) -->

```lisp
(defun canvas-height (canvas) ... )
```

　${{TODO}{まだ記述されていません。}}

<!-- (defun (setf canvas-height) (val canvas) -->

#### canvas-fix-point 関数
<!-- autolink: [canvas-fix-point](#canvas-fix-point 関数) -->

```lisp
(defun canvas-fix-point (canvas pt) ... )
```

　${{TODO}{まだ記述されていません。}}

#### with-canvas マクロ
<!-- autolink: [with-canvas](#with-canvas マクロ) -->

```lisp
(defmacro with-canvas ((sym-topleft sym-width sym-height) canvas &rest body) ... )
```

　${{TODO}{まだ記述されていません。}}

#### with-subcanvas マクロ
<!-- autolink: [with-subcanvas](#with-subcanvas マクロ) -->

```lisp
(defmacro with-subcanvas ((top-left width height) &rest body) ... )
```

　${{TODO}{まだ記述されていません。}}

#### canvas-dict-width 関数
<!-- autolink: [canvas-dict-width](#canvas-dict-width 関数) -->

```lisp
(defun canvas-dict-width (canvas) ... )
```

　${{TODO}{まだ記述されていません。}}

#### canvas-dict-height 関数
<!-- autolink: [canvas-dict-height](#canvas-dict-height 関数) -->

```lisp
(defun canvas-dict-height (canvas) ... )
```

　${{TODO}{まだ記述されていません。}}

#### canvas-dict-topleft 関数
<!-- autolink: [canvas-dict-topleft](#canvas-dict-topleft 関数) -->

```lisp
(defun canvas-dict-topleft (canvas) ... )
```

　${{TODO}{まだ記述されていません。}}

#### canvas-dict-top 関数
<!-- autolink: [canvas-dict-top](#canvas-dict-top 関数) -->

```lisp
(defun canvas-dict-top (canvas) ... )
```

　${{TODO}{まだ記述されていません。}}

#### canvas-dict-topright 関数
<!-- autolink: [canvas-dict-topright](#canvas-dict-topright 関数) -->

```lisp
(defun canvas-dict-topright (canvas) ... )
```

　${{TODO}{まだ記述されていません。}}

#### canvas-dict-left 関数
<!-- autolink: [canvas-dict-left](#canvas-dict-left 関数) -->

```lisp
(defun canvas-dict-left (canvas) ... )
```

　${{TODO}{まだ記述されていません。}}

#### canvas-dict-center 関数
<!-- autolink: [canvas-dict-center](#canvas-dict-center 関数) -->

```lisp
(defun canvas-dict-center (canvas) ... )
```

　${{TODO}{まだ記述されていません。}}

#### canvas-dict-right 関数
<!-- autolink: [canvas-dict-right](#canvas-dict-right 関数) -->

```lisp
(defun canvas-dict-right (canvas) ... )
```

　${{TODO}{まだ記述されていません。}}

#### canvas-dict-bottomleft 関数
<!-- autolink: [canvas-dict-bottomleft](#canvas-dict-bottomleft 関数) -->

```lisp
(defun canvas-dict-bottomleft (canvas) ... )
```

　${{TODO}{まだ記述されていません。}}

#### canvas-dict-bottom 関数
<!-- autolink: [canvas-dict-bottom](#canvas-dict-bottom 関数) -->

```lisp
(defun canvas-dict-bottom (canvas) ... )
```

　${{TODO}{まだ記述されていません。}}

#### canvas-dict-bottomright 関数
<!-- autolink: [canvas-dict-bottomright](#canvas-dict-bottomright 関数) -->

```lisp
(defun canvas-dict-bottomright (canvas) ... )
```

　${{TODO}{まだ記述されていません。}}

### circle.lisp
<!-- autolink: [$$](#circle.lisp) -->

　circle.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp
* constants.lisp
* mathutil.lisp
* canvas.lisp
* point.lisp
* shape.lisp
* stroke-info.lisp
* link-info.lisp
* writer.lisp

### cl-diagram.lisp
<!-- autolink: [$$](#cl-diagram.lisp) -->

　cl-diagram.lisp は他のコンポーネントには依存していません。

#### string/mid 関数
<!-- autolink: [string/mid](#string/mid 関数) -->

```lisp
(defun string/mid (str idx &optional (len -1)) ... )
```

　${{TODO}{まだ記述されていません。}}

#### string/replace 関数
<!-- autolink: [string/replace](#string/replace 関数) -->

```lisp
(defun string/replace (target str1 str2) ... )
```

　${{TODO}{まだ記述されていません。}}

#### string/split 関数
<!-- autolink: [string/split](#string/split 関数) -->

```lisp
(defun string/split (line separator) ... )
```

　${{TODO}{まだ記述されていません。}}

#### onlisp/mkstr 関数
<!-- autolink: [onlisp/mkstr](#onlisp/mkstr 関数) -->

```lisp
(defun onlisp/mkstr (&rest args) ... )
```

　${{TODO}{まだ記述されていません。}}

#### onlisp/symb 関数
<!-- autolink: [onlisp/symb](#onlisp/symb 関数) -->

```lisp
(defun onlisp/symb (&rest args) ... )
```

　${{TODO}{まだ記述されていません。}}

#### onlisp/keysymb 関数
<!-- autolink: [onlisp/keysymb](#onlisp/keysymb 関数) -->

```lisp
(defun onlisp/keysymb (&rest args) ... )
```

　${{TODO}{まだ記述されていません。}}

#### onlisp/flatten 関数
<!-- autolink: [onlisp/flatten](#onlisp/flatten 関数) -->

```lisp
(defun onlisp/flatten (x &optional (acc nil)) ... )
```

　${{TODO}{まだ記述されていません。}}

#### fix-name 関数
<!-- autolink: [fix-name](#fix-name 関数) -->

```lisp
(defun fix-name (name &optional no-multiline) ... )
```

　${{TODO}{まだ記述されていません。}}

#### __write-imp 関数
<!-- autolink: [__write-imp](#__write-imp 関数) -->

```lisp
(defun __write-imp (stream itm) ... )
```

　${{TODO}{まだ記述されていません。}}

#### exception コンディション
<!-- autolink: [exception](#exception コンディション) -->

```lisp
(define-condition exception (cl:error) ... )
```

　${{TODO}{まだ記述されていません。}}

#### caution コンディション
<!-- autolink: [caution](#caution コンディション) -->

```lisp
(define-condition caution (cl:warning) ... )
```

　${{TODO}{まだ記述されていません。}}

#### throw-exception マクロ
<!-- autolink: [throw-exception](#throw-exception マクロ) -->

```lisp
(defmacro throw-exception (fmt &rest args) ... )
```

　${{TODO}{まだ記述されていません。}}

#### throw-caution マクロ
<!-- autolink: [throw-caution](#throw-caution マクロ) -->

```lisp
(defmacro throw-caution (fmt &rest args) ... )
```

　${{TODO}{まだ記述されていません。}}

#### type-assert マクロ
<!-- autolink: [type-assert](#type-assert マクロ) -->

```lisp
(defmacro type-assert (symbol type) ... )
```

　${{TODO}{まだ記述されていません。}}

#### chk-type マクロ
<!-- autolink: [chk-type](#chk-type マクロ) -->

```lisp
(defmacro chk-type (symbol type &optional (name nil)) ... )
```

　${{TODO}{まだ記述されていません。}}

#### format-string 関数
<!-- autolink: [format-string](#format-string 関数) -->

```lisp
(defun format-string (&rest args) ... )
```

　${{TODO}{まだ記述されていません。}}

#### check-member マクロ
<!-- autolink: [check-member](#check-member マクロ) -->

```lisp
(defmacro check-member (sym &key (nullable nil) (types nil)) ... )
```

　${{TODO}{まだ記述されていません。}}

#### check-object マクロ
<!-- autolink: [check-object](#check-object マクロ) -->

```lisp
(defmacro check-object (sym canvas dict &key (nullable nil) (class nil)) ... )
```

　${{TODO}{まだ記述されていません。}}

#### check-keywords マクロ
<!-- autolink: [check-keywords](#check-keywords マクロ) -->

```lisp
(defmacro check-keywords (sym &rest choices) ... )
```

　${{TODO}{まだ記述されていません。}}

#### check-numbers マクロ
<!-- autolink: [check-numbers](#check-numbers マクロ) -->

```lisp
(defmacro check-numbers (sym &rest choices) ... )
```

　${{TODO}{まだ記述されていません。}}

#### write-when マクロ
<!-- autolink: [write-when](#write-when マクロ) -->

```lisp
(defmacro write-when (item &rest args) ... )
```

　${{TODO}{まだ記述されていません。}}

#### with-dictionary マクロ
<!-- autolink: [with-dictionary](#with-dictionary マクロ) -->

```lisp
(defmacro with-dictionary (dict &rest body) ... )
```

　${{TODO}{まだ記述されていません。}}

#### escape-characters 関数
<!-- autolink: [escape-characters](#escape-characters 関数) -->

```lisp
(defun escape-characters (str) ... )
```

　${{TODO}{まだ記述されていません。}}

#### to-property-strings 総称関数
<!-- autolink: [to-property-strings](#to-property-strings 総称関数) -->

```lisp
(defgeneric to-property-strings (info))
```

　${{TODO}{まだ記述されていません。}}

#### to-style-strings 総称関数
<!-- autolink: [to-style-strings](#to-style-strings 総称関数) -->

```lisp
(defgeneric to-style-strings (info))
```

　${{TODO}{まだ記述されていません。}}

#### check 総称関数
<!-- autolink: [check](#check 総称関数) -->

```lisp
(defgeneric check (obj canvas dict))
```

　${{TODO}{まだ記述されていません。}}

#### rgb 関数
<!-- autolink: [rgb](#rgb 関数) -->

```lisp
(defun rgb (r g b) ... )
```

　R, G, B の各色要素の値を 0 - 255 で与えることにより、`#RRGGBB` 形式の文字列を生成します。
以下に例を示します。

```lisp
(diagram:rgb 10 20 30)
; => "#0A141E"
```

### connector.lisp
<!-- autolink: [$$](#connector.lisp) -->

　connector.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp
* constants.lisp
* dictionary.lisp
* line.lisp
* rectangle.lisp
* shape.lisp
* writer.lisp

### constants.lisp
<!-- autolink: [$$](#constants.lisp) -->

　constants.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp

### create-svg.lisp
<!-- autolink: [$$](#create-svg.lisp) -->

　create-svg.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp
* constants.lisp
* entity.lisp
* layer-manager.lisp
* dictionary.lisp
* point.lisp
* canvas.lisp
* font-info.lisp
* stroke-info.lisp
* writer.lisp

#### create-svg マクロ
<!-- autolink: [create-svg](#create-svg マクロ) -->

```lisp
(defmacro create-svg ((&key width height desc) &rest body) ... )
```

　${{TODO}{まだ記述されていません。}}

#### diagram マクロ
<!-- autolink: [diagram](#diagram マクロ) -->

```lisp
(defmacro diagram ((&key w h) &rest body) ... )
```

　${{TODO}{まだ記述されていません。}}

### dictionary.lisp
<!-- autolink: [$$](#dictionary.lisp) -->

　dictionary.lisp は、ディクショナリ機能を実装します。以下のコンポーネントに依存しています。

* cl-diagram.lisp

#### dictionary クラス
<!-- autolink: [dictionary](#dictionary クラス) -->

```lisp
(defclass dictionary ()
  ((history-max  :initform   0 :initarg :hist-max)   ; integer
   (history-top  :initform   0 :initarg :hist-top)   ; list
   (history-size :initform   0 :initarg :hist-size)  ; integer
   (map          :initform nil :initarg :map)))      ; hashtable
```

　基本的には、キーワードシンボルをキーとする hashtable で管理されます。 `history-*` メンバは履歴
機能のためのもので、 `history-top` が `entity` のリストを保有します。 `history-size` がリストの
現在のサイズを保持しており、これが `history-max` を超えないように制御されます。

#### dict-create 関数
<!-- autolink: [dict-create](#dict-create 関数) -->

```lisp
(defun dict-create (history-max-count) ... )
```

　`dictionary` のインスタンスを新規に作成して返します。履歴機能で保有する最大数をパラメータとして
受け取ります。これは後から変更することはできません。

#### dict-register 関数
<!-- autolink: [dict-register](#dict-register 関数) -->

```lisp
(defun dict-register (dict entity) ... )
```

　`dictionary` のインスタンス `dict` に `entity` を登録します。 `entity` に `id` が指定されて
いない場合、履歴には追加されますがディクショナリ登録はされません。

#### dict-get-entity 関数
<!-- autolink: [dict-get-entity](#dict-get-entity 関数) -->

```lisp
(defun dict-get-entity (dict id) ... )
```

　指定した `id` の `entity` をディクショナリ、または履歴から探して返します。

　`id` が $1 などの履歴を示すシンボルの場合、履歴からエンティティを探します。そうでない場合、
ID でハッシュテーブルを検索し、みつけたエンティティを返します。

### ellipse.lisp
<!-- autolink: [$$](#ellipse.lisp) -->

　ellipse.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp
* constants.lisp
* canvas.lisp
* point.lisp
* shape.lisp
* stroke-info.lisp
* link-info.lisp
* writer.lisp

### endmark-info.lisp
<!-- autolink: [$$](#endmark-info.lisp) -->

　endmark-info.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp
* constants.lisp
* mathutil.lisp
* point.lisp
* canvas.lisp
* dictionary.lisp
* fill-info.lisp
* stroke-info.lisp
* writer.lisp

### entity.lisp
<!-- autolink: [$$](#entity.lisp) -->

　entity.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp
* canvas.lisp
* writer.lisp

### fill-info.lisp
<!-- autolink: [$$](#fill-info.lisp) -->

　fill-info.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp

### font-info.lisp
<!-- autolink: [$$](#font-info.lisp) -->

　font-info.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp
* fill-info.lisp
* stroke-info.lisp

### group.lisp
<!-- autolink: [$$](#group.lisp) -->

　group.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp
* canvas.lisp
* shape.lisp
* writer.lisp

### image.lisp
<!-- autolink: [$$](#image.lisp) -->

　image.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp
* binutil.lisp
* shape.lisp
* label-info.lisp
* link-info.lisp
* point.lisp
* writer.lisp

### label-info.lisp
<!-- autolink: [$$](#label-info.lisp) -->

　label-info.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp
* constants.lisp
* point.lisp
* canvas.lisp
* font-info.lisp
* shape.lisp
* writer.lisp

### layer-manager.lisp
<!-- autolink: [$$](#layer-manager.lisp) -->

　layer-manager.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp
* writer.lisp

### line.lisp
<!-- autolink: [$$](#line.lisp) -->

　line.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp
* constants.lisp
* point.lisp
* mathutil.lisp
* label-info.lisp
* stroke-info.lisp
* endmark-info.lisp
* entity.lisp
* writer.lisp

### link-info.lisp
<!-- autolink: [$$](#link-info.lisp) -->

　link-info.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp
* constants.lisp
* writer.lisp

### mathutil.lisp
<!-- autolink: [$$](#mathutil.lisp) -->

　mathutil.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp
* point.lisp

#### math/len2 関数
<!-- autolink: [math/len2](#math/len2 関数) -->
```lisp
(defun math/len2 (pt1 pt2) ... )
```

　点 `pt1` から点 `pt2` までの距離を計算して返します。 `pt1` および `pt2` は point です。
point が手元にない場合は math/len4 関数を使用してください。

　以下の例では、 `(30 30)` から `(90 70)` までの距離を計算しています。

```lisp
(let ((pt1 (make-point 30 30))
      (pt2 (make-point 90 70)))
  (math/len2 pt1 pt2))
;=> 72.11102
```

　これは、以下の青い線の部分の長さを測っていることになります。

<!-- snippet: SAMPLE_OF_LEN2_LEN4 -->
(diagram (:w 150 :h 100)
  (grid)
  (circle '(30 30) 3 :stroke :none :fill :red :id :pt1)
  (text '(35 25) "(30 30)" :align :left)
  (circle '(90 70) 3 :stroke :none :fill :red :id :pt2)
  (text '(95 75) "(90 70)" :align :left)
  (connector :pt1 :pt2 :stroke :blue))
<!-- end snippet -->

```diagram
<!-- expand: SAMPLE_OF_LEN2_LEN4 -->
```

#### math/len4 関数
<!-- autolink: [math/len4](#math/len4 関数) -->
```lisp
(defun math/len4 (x1 y1 x2 y2) ... )
```

　点 `(x1 y1)` から点 `(x2 y2)` までの距離を計算して返します。point が手元にある場合は math/len2 
関数を使用してください。

　以下の例では、 `(30 30)` から `(90 70)` までの距離を計算しています。

```lisp
(math/len4 30 30 90 70)
;=> 72.11102
```

　これは、以下の青い線の部分の長さを測っていることになります。

```diagram
<!-- expand: SAMPLE_OF_LEN2_LEN4 -->
```

#### math/sin1 関数
<!-- autolink: [math/sin1](#math/sin1 関数) -->
```lisp
(defun math/sin1 (degree) ... )
```

　角度 `degree` の sin を計算して返します。 `degree` は [0, 360) である必要があります。

#### math/cos1 関数
<!-- autolink: [math/cos1](#math/cos1 関数) -->

```lisp
(defun math/cos1 (degree) ... )
```

　角度 `degree` の cos を計算して返します。 `degree` は [0, 360) である必要があります。

#### math/sin2 関数
<!-- autolink: [math/sin2](#math/sin2 関数) -->

```lisp
(defun math/sin2 (pt1 pt2) ... )
```

　2 点 `pt1, pt2` を通る直線が x 軸に対してなす角度の sin を計算して返します。 `pt1` 
および `pt2` は point です。すなわち、以下における `b / len` を計算します。point が
手元にない場合は math/sin4 関数を使用してください。

<!-- snippet: SAMPLE_OF_SIN2_COS2 -->
(diagram (:w 200 :h 100)
  (grid)
  (let ((pt1 (make-point  50 30))
        (pt2 (make-point 150 70))
        (pt3 (make-point 150 30)))
    (with-stroke (:color :gray :dasharray '(2 2))
      (line (list pt1 pt3))
      (line (list pt2 pt3)))
    (line (list pt1 pt2) :stroke :blue)
    (circle pt1 3 :fill :red :stroke :none)
    (text (y+ pt1 -10) "pt1" :align :right)
    (circle pt2 3 :fill :red :stroke :none)
    (text (y+ pt2  15) "pt2")
    (text '(100 27) "a" :align :left)
    (text '(155 55) "b" :align :left)
    (text '( 90 70) "len" :align :left)))
<!-- end snippet -->

```diagram
<!-- expand: SAMPLE_OF_SIN2_COS2 -->
```

#### math/cos2 関数
<!-- autolink: [math/cos2](#math/cos2 関数) -->

```lisp
(defun math/cos2 (pt1 pt2) ... )
```

　2 点 `pt1, pt2` を通る直線が x 軸に対してなす角度の cos を計算して返します。 `pt1` 
および `pt2` は point です。すなわち、以下における `a / len` を計算します。point が
手元にない場合は math/cos4 関数を使用してください。

```diagram
<!-- expand: SAMPLE_OF_SIN2_COS2 -->
```

#### math/sin3 関数
<!-- autolink: [math/sin3](#math/sin3 関数) -->

```lisp
(defun math/sin3 (pt1 pt2 degree) ... )
```

　2 点 `pt1, pt2` を通る直線に対して角度 `degree` をなす直線が x 軸に対してなす角度の sin を
計算して返します。 `pt1` および `pt2` は point です。すなわち、以下における `sin θ` を返し
ます。 `degree` は [0, 360) である必要があります。point が手元にない場合は math/sin5 関数を
使用してください。

<!-- snippet: SAMPLE_OF_SIN3_COS3 -->
(diagram (:w 200 :h 100)
  (grid)
  (let ((pt1 (make-point  50 30))
        (pt2 (make-point 150 70)))
    (line (list '(40 0) '(80 120)) :stroke (make-stroke :color :gray :dasharray '(2 2)))
    (line (list pt1 pt2) :stroke :blue)
    (circle pt1 3 :fill :red :stroke :none)
    (circle pt2 3 :fill :red :stroke :none)
    (arc  pt1     20 20 72)
    (arc  '(40 0) 20  0 72)
    (text (xy+ pt1 -5 10) "pt1" :align :right)
    (text (y+  pt2    15) "pt2")
    (text '( 70 70) "degree" :align :left)
    (text '( 60 20) "θ" :align :left)))
<!-- end snippet -->

```diagram
<!-- expand: SAMPLE_OF_SIN3_COS3 -->
```

#### math/cos3 関数
<!-- autolink: [math/cos3](#math/cos3 関数) -->

```lisp
(defun math/cos3 (pt1 pt2 degree) ... )
```

　2 点 `pt1, pt2` を通る直線に対して角度 `degree` をなす直線が x 軸に対してなす角度の cos を
計算して返します。 `pt1` および `pt2` は point です。すなわち、以下における `cos θ` を返し
ます。 `degree` は [0, 360) である必要があります。point が手元にない場合は math/cos5 関数を
使用してください。

```diagram
<!-- expand: SAMPLE_OF_SIN3_COS3 -->
```

#### math/sin4 関数
<!-- autolink: [math/sin4](#math/sin4 関数) -->

```lisp
(defun math/sin4 (x1 y1 x2 y2) ... )
```

　2 点 `(x1 y1), (x2 y2)` を通る直線が x 軸に対してなす角度の sin を計算して返します。すなわち、
以下における `b / len` を計算します。point が手元にある場合は math/sin2 関数を使用してください。

<!-- snippet: SAMPLE_OF_SIN4_COS4 -->
(diagram (:w 200 :h 100)
  (grid)
  (let ((pt1 (make-point  50 30))
        (pt2 (make-point 150 70))
        (pt3 (make-point 150 30)))
    (with-stroke (:color :gray :dasharray '(2 2))
      (line (list pt1 pt3))
      (line (list pt2 pt3)))
    (line (list pt1 pt2) :stroke :blue)
    (circle pt1 3 :fill :red :stroke :none)
    (text (y+ pt1 -10) "(x1 y1)" :align :right)
    (circle pt2 3 :fill :red :stroke :none)
    (text (y+ pt2  15) "(x2 y2)")
    (text '(100 27) "a" :align :left)
    (text '(155 55) "b" :align :left)
    (text '( 90 70) "len" :align :left)))
<!-- end snippet -->

```diagram
<!-- expand: SAMPLE_OF_SIN4_COS4 -->
```

#### math/cos4 関数
<!-- autolink: [math/cos4](#math/cos4 関数) -->

```lisp
(defun math/cos4 (x1 y1 x2 y2) ... )
```

　2 点 `(x1 y1), (x2 y2)` を通る直線が x 軸に対してなす角度の cos を計算して返します。すなわち、
以下における `a / len` を計算します。point が手元にある場合は math/cos2 関数を使用してください。

```diagram
<!-- expand: SAMPLE_OF_SIN4_COS4 -->
```

#### math/sin5 関数
<!-- autolink: [math/sin5](#math/sin5 関数) -->

```lisp
(defun math/sin5 (x1 y1 x2 y2 degree) ... )
```

　2 点 `(x1 y1), (x2 y2)` を通る直線に対して角度 `degree` をなす直線が x 軸に対してなす角度の sin を
計算して返します。すなわち、以下における `sin θ` を返します。 `degree` は [0, 360) である必要が
あります。point が手元にある場合は math/sin3 関数を使用してください。

<!-- snippet: SAMPLE_OF_SIN5_COS5 -->
(diagram (:w 200 :h 100)
  (grid)
  (let ((pt1 (make-point  50 30))
        (pt2 (make-point 150 70)))
    (line (list '(40 0) '(80 120)) :stroke (make-stroke :color :gray :dasharray '(2 2)))
    (line (list pt1 pt2) :stroke :blue)
    (circle pt1 3 :fill :red :stroke :none)
    (circle pt2 3 :fill :red :stroke :none)
    (arc  pt1     20 20 72)
    (arc  '(40 0) 20  0 72)
    (text (xy+ pt1 -2 10) "(x1 y1)" :align :right)
    (text (y+  pt2    15) "(x2 y2)")
    (text '( 70 70) "degree" :align :left)
    (text '( 60 20) "θ" :align :left)))
<!-- end snippet -->

```diagram
<!-- expand: SAMPLE_OF_SIN5_COS5 -->
```

#### math/cos5 関数
<!-- autolink: [math/cos5](#math/cos5 関数) -->

```lisp
(defun math/cos5 (x1 y1 x2 y2 degree) ... )
```

　2 点 `(x1 y1), (x2 y2)` を通る直線に対して角度 `degree` をなす直線が x 軸に対してなす角度の cos を
計算して返します。すなわち、以下における `cos θ` を返します。 `degree` は [0, 360) である必要が
あります。point が手元にある場合は math/cos3 関数を使用してください。

```diagram
<!-- expand: SAMPLE_OF_SIN5_COS5 -->
```

### paragraph.lisp
<!-- autolink: [$$](#paragraph.lisp) -->

　paragraph.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp
* constants.lisp
* shape.lisp
* font-info.lisp
* link-info.lisp
* point.lisp
* writer.lisp

### path.lisp
<!-- autolink: [$$](#path.lisp) -->

　path.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp
* constants.lisp
* fill-info.lisp
* stroke-info.lisp
* entity.lisp
* writer.lisp

### pathutil.lisp
<!-- autolink: [$$](#pathutil.lisp) -->

　pathutil.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp

　pathutil はファイル名やパス名を扱うための関数群を提供します。「Practical 
Common LISP」で紹介されている pathname utility から必要なものだけを抽出した
ものです。

#### path/is-directory-name 関数
<!-- autolink: [path/is-directory-name](#path/is-directory-name 関数) -->

```lisp
(defun path/is-directory-name (p) ... )
```

　`p` がディレクトリ名か否かを返します。

#### path/is-file-name 関数
<!-- autolink: [path/is-file-name](#path/is-file-name 関数) -->

```lisp
(defun path/is-file-name (p) ... )
```

　`p` がファイル名か否かを返します。

#### path/is-exists 関数
<!-- autolink: [path/is-exists](#path/is-exists 関数) -->

```lisp
(defun path/is-exists (pathname) ... )
```

　`name` が実際に存在するか否かを返します。ファイルかディレクトリかを問いません。

#### path/is-existing-file 関数
<!-- autolink: [path/is-existing-file](#path/is-existing-file 関数) -->

```lisp
(defun path/is-existing-file (name) ... )
```

　`name` が実際に存在するファイルであるか否かを返します。

#### path/get-time 関数
<!-- autolink: [path/get-time](#path/get-time 関数) -->

```lisp
(defun path/get-time (pathname &optional (type :string)) ... )
```

　`pathname` で指定した名前のファイルについて、その更新日付を返します。オプションの `type` 引数の
値によって、返される値の形式が変わります。

| type                     | 返される値の形式                     |
|:------------------------:|:------------------------------------|
| :string                  | "yyyy/mm/dd HH:MM:SS" 形式の文字列   |
| :value                   | 日付を表す数値                       |
| :values                  | 多値                                |
| キーワードシンボルのリスト | キーワードに対応する値をまとめたリスト |


　最後のリストで要素に使用できるキーワードシンボルは以下の通りです。

* `:second`
* `:minute`
* `:hour`
* `:date`
* `:month`
* `:year`
* `:day`
* `:daylight-p`
* `:zone`


　以下に例を示します。

```lisp
(diagram::path/get-time file-name)
; => "2021/07/19 22:07:54"
(diagram::path/get-time file-name :string)
; => "2021/07/19 22:07:54"
(diagram::path/get-time file-name :value)
; => 3835688874
(diagram::path/get-time file-name :values)
; => 54
;    7
;    22
;    19
;    7
;    2021
;    0
;    NIL
;    -9
(diagram::path/get-time file-name '(:year :month :date))
; => (2021 7 19)
```

#### path/get-current-directory 関数
<!-- autolink: [path/get-current-directory](#path/get-current-directory 関数) -->

```lisp
(defun path/get-current-directory () ... )
```

　カレントディレクトリを pathname オブジェクトで返します。

#### path/set-current-directory 関数
<!-- autolink: [path/set-current-directory](#path/set-current-directory 関数) -->

```lisp
(defun path/set-current-directory (new-pathname) ... )
```

　カレントディレクトリを変更します。

### point.lisp
<!-- autolink: [$$](#point.lisp) -->

　point.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp

　${APPNAME} の座標は point を使って表現します。これは基本的に 2 要素の数値のリストで、
通常は make-point 関数で作成します。

```lisp
 (make-point 1 2) ; => (1 2)
```

　実際には、座標には __絶対座標__ と __相対座標__ があり、それを指定するための `&optional` 
パラメータが存在します。

```lisp
 (make-point 1 2 :absolute) ; => (1 2 . :absolute)
```

#### make-point 関数
<!-- autolink: [make-point](#make-point 関数) -->

```lisp
(defun make-point (x y &optional type) ... )
```

　point を作成するには、x, y を指定して make-point 関数をコールします。
オプションで `:abosolute` か `:relative` を指定できます。省略した場合は 
`:relative` になります。これらは絶対座標か相対座標の区別です。

#### copy-point 関数
<!-- autolink: [copy-point](#copy-point 関数) -->

```lisp
(defun copy-point (pt) ... )
```

　point を複製するには copy-point 関数を使ってください。

#### point-p 関数
<!-- autolink: [point-p](#point-p 関数) -->

```lisp
(defun point-p (pt) ... )
```

#### point-absolute-p 関数
<!-- autolink: [point-absolute-p](#point-absolute-p 関数) -->

```lisp
(defun point-absolute-p (pt) ... )
```

　point が絶対座標か否かは point-absolute-p 関数を使って判定します。

#### point-relative-p 関数
<!-- autolink: [point-relative-p](#point-relative-p 関数) -->

```lisp
(defun point-relative-p (pt) ... )
```

　point が相対座標か否かは point-relative-p 関数を使って判定します。

#### point-x 関数
<!-- autolink: [point-x](#point-x 関数) -->

```lisp
(defun point-x (pt) ... )
(defun (setf point-x) (val pt) ... )
```

　point が保持する x 座標の値を取得／設定するには point-x 関数を使用します。

#### point-y 関数
<!-- autolink: [point-y](#point-y 関数) -->

```lisp
(defun point-y (pt) ... )
(defun (setf point-y) (val pt) ... )
```

　point が保持する y 座標の値を取得／設定するには point-y 関数を使用します。

#### point+ 関数
<!-- autolink: [point+](#point+ 関数) -->

```lisp
(defun point+ (pt1 pt2) ... )
```

#### point- 関数
<!-- autolink: [point-](#point- 関数) -->

```lisp
(defun point- (pt1 pt2) ... )
```

#### point/x+ 関数
<!-- autolink: [point/x+](#point/x+ 関数) -->
<!-- autolink: [x+](#point/x+ 関数) -->

```lisp
(defun point/x+ (pt x) ... )
```

　point の x に加減算をした新しい point を作成して返します。頻繁に使用するため、
同じことをする x+ という関数も用意しています。

#### point/y+ 関数
<!-- autolink: [point/y+](#point/y+ 関数) -->
<!-- autolink: [y+](#point/y+ 関数) -->

```lisp
(defun point/y+ (pt y) ... )
```

　point の y に加減算をした新しい point を作成して返します。頻繁に使用するため、
同じことをする y+ という関数も用意しています。

#### point/xy+ 関数
<!-- autolink: [point/xy+](#point/xy+ 関数) -->
<!-- autolink: [xy+](#point/xy+ 関数) -->

```lisp
(defun point/xy+ (pt x y) ... )
```

　point の x, y に加減算をした新しい point を作成して返します。頻繁に使用するため、
同じことをする xy+ という関数も用意しています。

#### point-distance 関数
<!-- autolink: [point-distance](#point-distance 関数) -->

```lisp
(defun point-distance (pt1 pt2) ... )
```
　2 つの point 間の距離は point-distance 関数で測ることができます。

#### with-point マクロ
<!-- autolink: [with-point](#with-point マクロ) -->

```lisp
(defmacro with-point ((sym-x sym-y) pt &rest body) ... )
```

　point の x, y に symbol-macrolet でアクセスしたければ with-point マクロが
使えます。

${BLANK_PARAGRAPH}

### polygon.lisp
<!-- autolink: [$$](#polygon.lisp) -->

　polygon.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp
* constants.lisp
* fill-info.lisp
* stroke-info.lisp
* link-info.lisp
* entity.lisp
* writer.lisp

### raw-svg.lisp
<!-- autolink: [$$](#raw-svg.lisp) -->

　raw-svg.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp
* entity.lisp
* writer.lisp

### rectangle.lisp
<!-- autolink: [$$](#rectangle.lisp) -->

　rectangle.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp
* constants.lisp
* canvas.lisp
* point.lisp
* shape.lisp
* stroke-info.lisp
* link-info.lisp
* writer.lisp

### shape.lisp
<!-- autolink: [$$](#shape.lisp) -->

　shape.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp
* point.lisp
* canvas.lisp
* mathutil.lisp
* entity.lisp
* link-info.lisp

### stencil.lisp
<!-- autolink: [$$](#stencil.lisp) -->

　stencil.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp
* pathutil.lisp

### stroke-info.lisp
<!-- autolink: [$$](#stroke-info.lisp) -->

　stroke-info.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp

### stylesheet.lisp
<!-- autolink: [$$](#stylesheet.lisp) -->

　stylesheet.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp
* entity.lisp
* stroke-info.lisp
* fill-info.lisp
* font-info.lisp
* writer.lisp

### text-shape.lisp
<!-- autolink: [$$](#text-shape.lisp) -->

　text-shape.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp
* canvas.lisp
* group.lisp
* paragraph.lisp
* font-info.lisp
* fill-info.lisp
* stroke-info.lisp
* writer.lisp

### text.lisp
<!-- autolink: [$$](#text.lisp) -->

　text.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp
* constants.lisp
* entity.lisp
* font-info.lisp
* link-info.lisp
* writer.lisp

### writer.lisp
<!-- autolink: [$$](#writer.lisp) -->

　writer.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp

　writer.lisp では、buffer-writer クラスを定義しています。

#### buffer-writer クラス
<!-- autolink: [buffer-writer](#buffer-writer クラス) -->

　${APPNAME} において SVG データを出力するために使用するクラスです。基本的には、
以下のように使用します。

* create-svg-writer 関数でインスタンスを作成し、
* writer-incr-level / writer-decr-level メソッドでインデントを調整
* writer-write メソッドで行を出力
* writer-close メソッドで出力全体の文字列を取得

##### writer-write メソッド
<!-- autolink: [writer-write](#writer-write メソッド) -->
```lisp
(defmethod writer-write ((writer buffer-writer) &rest params) ... )
```

　一回の呼び出しで 1 行を出力します。このとき、

* writer-incr-level / writer-decr-level メソッドで指定されたレベル数分だけタブを出力します
* `params` で与えられた複数のパラメータを順番に出力します
	* `nil` は出力しません
	* キーワードシンボルはその文字列表現を小文字にしたものを出力します
	* 整数でない数値は、 `single-float` に `coerce` したものを `format "~F"` で出力します
	* 上記のいずれにも該当しない場合、 `format "~A"` で出力します
* 最後に改行コードを出力します

##### writer-incr-level メソッド
<!-- autolink: [writer-incr-level](#writer-incr-level メソッド) -->
```lisp
(defmethod writer-incr-level ((writer buffer-writer)) ... )
```

　buffer-writer インスタンスのインデントレベルをひとつ増加させます。これは writer-write 
メソッドでの出力に影響します。

##### writer-decr-level メソッド
<!-- autolink: [writer-decr-level](#writer-decr-level メソッド) -->
```lisp
(defmethod writer-decr-level ((writer buffer-writer)) ... )
```

　buffer-writer インスタンスのインデントレベルをひとつ減少させます。これは writer-write 
メソッドでの出力に影響します。

##### writer-close メソッド
<!-- autolink: [writer-close](#writer-close メソッド) -->
```lisp
(defmethod writer-close ((writer buffer-writer)) ... )
```

　buffer-writer のストリームを閉じ、そのストリームにバッファリングされていた文字列を返します。

#### create-svg-writer 関数
<!-- autolink: [create-svg-writer](#create-svg-writer 関数) -->

```lisp
(defun create-svg-writer () ... )
```

　buffer-writer クラスを作成して返します。パラメータは取りません。


${BLANK_PARAGRAPH}

## 索引

<!-- embed:index-x -->


${BLANK_PARAGRAPH}

--------------------------------------------------------------------------------

<!-- embed:footnotes -->

