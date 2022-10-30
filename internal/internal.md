<!-- define: APPNAME = kaavio -->
<!-- define: BLANK_PARAGRAPH = '　　' -->
<!-- define: TODO = '@((background:red;color:white;)(ToDo : %1))' -->

<!-- title:${APPNAME} internal -->
<!-- style:./default.css -->			

<!-- config:write-comment -->
<!-- config:header-numbering 2 4 -->
<!-- config:entity-numbering-depth 1 -->
<!-- <!-- config:term-link-in-header -->

<!-- filter:kaavio   = bash ./kaavio.sh  %in %out -->
<!-- filter:plantuml = bash ./plantuml.sh %in %out -->

# ${APPNAME} internal documents

　この文書は、 **${APPNAME}** の内部設計文書です。

<!-- anchor: toc-link-target -->
```raw
<h2>Table of contents</h2>
```
<!-- embed:toc-x 2 3 -->
<!-- toc-link: top 'A#toc-link-target' -->

--------------------------------------------------------------------------------

${BLANK_PARAGRAPH}

## 概念
### 座標系

　${APPNAME} の座標系は、左上を原点としています。そこから水平右方向に x 軸、垂直
下方向に y 軸です。角度は時計回りになります。

```kaavio
(diagram (300 150)
  (grid)
  (circle canvas.topleft 4 :fill :black)
  (let ((em (make-endmark :type :triangle :fill :black :size :small)))
    (text '(5 15) "(0, 0)")
    (line '((250  10) (275  10)) :end2 em) (text '(280  15) "x" :align :left)
    (line '(( 10 100) ( 10 130)) :end2 em) (text '( 10 145) "y" :align :center)
    (line '((100  60) (180  60)))          (text '(185  65) "0°" :align :left)
    (line '((100  60) (140 130)))
    (arc   '(100  60) 30 30 0 0 50)
    (line '((123  80) (115  85)) :end2 em) (text '(130  85) "θ" :align :left)))
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

### 色の指定
<!-- autolink: [$$](#色の指定) -->

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

　entity.lisp は、エンティティを実装します。以下のコンポーネントに依存しています。

* cl-diagram.lisp
* canvas.lisp
* writer.lisp

#### entity クラス
<!-- autolink: [entity](#entity クラス) -->

　entity は id と layer を保持します。どちらもキーワードシンボルとしての保有で、
nil が許可されます。

```lisp
(defclass entity ()
  ((id    :initform nil :initarg :id)       ; keyword
   (layer :initform nil :initarg :layer)))  ; keyword
```

<!-- snippet: CLASS_DEF_ENTITY
class entity {
  keyword id
  keyword layer
}
-->

```plantuml
@startuml
<!-- expand: CLASS_DEF_ENTITY -->
@enduml
```

#### begin-id-group 関数
<!-- autolink: [begin-id-group](#begin-id-group 関数) -->

```lisp
(defun begin-id-group (ent writer) ... )
```

　id を持つエンティティの場合、svg 出力において id を指定した g タグで全体を括るための
関数です。デフォルト実装では pre-draw 総称関数の実装からコールされます。

#### end-id-group 関数
<!-- autolink: [end-id-group](#end-id-group 関数) -->

```lisp
(defun end-id-group (ent writer) ... )
```

　id を持つエンティティの場合、svg 出力において id を指定した g タグを閉じるための関数
です。デフォルト実装では post-draw 総称関数の実装からコールされます。

#### entity-composition-p 総称関数
<!-- autolink: [entity-composition-p](#entity-composition-p 総称関数) -->

```lisp
(defgeneric entity-composition-p (ent)) ... )
```

　エンティティが複合オブジェクトであるか否かを調べるための総称関数です。entity に対する
実装では nil を返します。

#### write-header 総称関数
<!-- autolink: [write-header](#write-header 総称関数) -->

```lisp
(defgeneric write-header (entity writer)) ... )
```

　エンティティの svg コードを出力前にヘッダコメントを出力するための総称関数です。

#### pre-draw 総称関数
<!-- autolink: [pre-draw](#pre-draw 総称関数) -->

```lisp
(defgeneric pre-draw (entity writer)) ... )
```

　エンティティの svg コードを出力する draw-entity 総称関数の実装の冒頭でコールされることを
想定した総称関数です。entity に対するデフォルト実装では、複合エンティティであった場合に
begin-id-group 関数をコールします。

#### post-draw 総称関数
<!-- autolink: [post-draw](#post-draw 総称関数) -->

```lisp
(defgeneric post-draw (entity writer)) ... )
```

　エンティティの svg コードを出力する draw-entity 総称関数の実装の最後にコールされることを
想定した総称関数です。entity に対するデフォルト実装では、複合エンティティであった場合に
end-id-group 関数をコールします。

#### draw-entity 総称関数
<!-- autolink: [draw-entity](#draw-entity 総称関数) -->

```lisp
(defgeneric draw-entity (entity writer)) ... )
```

　エンティティの svg コードを出力するための総称関数です。entity に対する実装は存在しません。
派生クラスで必ず実装してください。

#### check-and-draw-local-entity 関数
<!-- autolink: [check-and-draw-local-entity](#check-and-draw-local-entity 関数) -->

```lisp
(defun check-and-draw-local-entity (entity canvas writer) ... )
```

　dictionary に登録されないローカルな entity を描画するための関数です。確認されている
利用個所としては以下があります。

* group.lisp - draw-canvas-frame 関数
* text-shape.lisp - draw-group メソッド

### fill-info.lisp
<!-- autolink: [$$](#fill-info.lisp) -->

　fill-info.lisp は塗り潰しの指定を表現するクラスです。以下のコンポーネントに依存しています。

* cl-diagram.lisp

#### *default-fill*変数
<!-- autolink: [*default-fill*](#*default-fill*変数) -->

```lisp
(defparameter *default-fill* nil)
```

　デフォルトの塗り潰しを表現するダイナミック変数です。初期状態では
`(make-fill :color :none :opacity nil :rule nil)` の結果が設定されます。

#### fill-info クラス
<!-- autolink: [fill-info](#fill-info クラス) -->

　fill-info は color、opacity、rule を保持します。いずれも nil が許可されます。

```lisp
(defclass fill-info ()
  ((color   :initform nil :initarg :color)    ; (or keyword string)
   (opacity :initform nil :initarg :opacity)  ; number
   (rule    :initform nil :initarg :rule)))   ; (or nil keyword)
```

　fill-info クラス向けに、以下の総称関数のメソッドが実装されています。

* initialize-instance 
* check 
* to-property-strings
* to-style-strings

#### make-fill 関数
<!-- autolink: [make-fill](#make-fill 関数) -->

```lisp
(defun make-fill (&rest params) ... )
```

　fill-info インスタンスを作成します。パラメータの数や種類に応じて作り方が異なります。

* パラメータ無しの場合、 `*default-fill*` を返します
* パラメータ数が 1 の場合、そのパラメータを param として
	* param が fill-info インスタンスの場合、渡されたものをそのまま返します
	* param がリストの場合、 `(apply #'make-fill param)` の結果を返します
	* 上記のいずれでもない場合、 `(make-fill :color param)` の結果を返します
* パラメータ数が 2 以上の場合、以下の関数のように振舞います。

	```lisp
	(defun make-fill (&key color opacity rule base) ... )
	```

	* `base` はその他のパラメータが省略された場合にデフォルト値を取得するための `fill-info` インスタンスで、これが省略された場合は `*default-fill*` が使用されます。
	* `color` は色の指定です。指定方法の詳細は「色の指定」を参照してください。
	* `opacity` で透明度を指定します。0 から 1 までの浮動小数点数で指定します。
	* `rule` は塗りつぶしに関するルールです。 `:nonzero` または `:evenodd` で指定します。

#### with-fill マクロ
<!-- autolink: [with-fill](#with-fill マクロ) -->

```lisp
(defmacro with-fill ((&rest param) &rest body) ... )
```

　`*default-fill*` を一時的に変更します。 `params` には make-fill に与えるパラメータと
同じものを指定します。以下のコードは、

```lisp
(with-fill (:color :red :opacity 0.2)
   ... )
```

　以下と同等です。

```lisp
(let ((*default-fill* (make-fill :color :red :opacity 0.2)))
   ... )
```

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
(diagram (150 100)
  (grid)
  (circle '(30 30) 3 :stroke :none :fill :red :id :pt1)
  (text '(35 25) "(30 30)" :align :left)
  (circle '(90 70) 3 :stroke :none :fill :red :id :pt2)
  (text '(95 75) "(90 70)" :align :left)
  (connector :pt1 :pt2 :stroke :blue))
<!-- end snippet -->

```kaavio
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

```kaavio
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
(diagram (200 100)
  (grid)
  (let ((pt1 (make-point  50 30))
        (pt2 (make-point 150 70))
        (pt3 (make-point 150 30)))
    (with-options (:stroke '(:color :gray :dasharray (2 2)))
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

```kaavio
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

```kaavio
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
(diagram (200 100)
  (grid)
  (let ((pt1 (make-point  50 30))
        (pt2 (make-point 150 70)))
    (line (list '(40 0) '(80 120)) :stroke (make-stroke :color :gray :dasharray '(2 2)))
    (line (list pt1 pt2) :stroke :blue)
    (circle pt1 3 :fill :red :stroke :none)
    (circle pt2 3 :fill :red :stroke :none)
    (arc  '(40 0) 20 20 0  0 72)
    (arc  pt1     20 20 0 25 72)
    (text (xy+ pt1 -5 10) "pt1" :align :right)
    (text (y+  pt2    15) "pt2")
    (text '( 70 70) "degree" :align :left)
    (text '( 60 20) "θ" :align :left)))
<!-- end snippet -->

```kaavio
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

```kaavio
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
(diagram (200 100)
  (grid)
  (let ((pt1 (make-point  50 30))
        (pt2 (make-point 150 70))
        (pt3 (make-point 150 30)))
    (with-options (:stroke '(:color :gray :dasharray (2 2)))
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

```kaavio
<!-- expand: SAMPLE_OF_SIN4_COS4 -->
```

#### math/cos4 関数
<!-- autolink: [math/cos4](#math/cos4 関数) -->

```lisp
(defun math/cos4 (x1 y1 x2 y2) ... )
```

　2 点 `(x1 y1), (x2 y2)` を通る直線が x 軸に対してなす角度の cos を計算して返します。すなわち、
以下における `a / len` を計算します。point が手元にある場合は math/cos2 関数を使用してください。

```kaavio
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
(diagram (200 100)
  (grid)
  (let ((pt1 (make-point  50 30))
        (pt2 (make-point 150 70)))
    (line (list '(40 0) '(80 120)) :stroke (make-stroke :color :gray :dasharray '(2 2)))
    (line (list pt1 pt2) :stroke :blue)
    (circle pt1 3 :fill :red :stroke :none)
    (circle pt2 3 :fill :red :stroke :none)
    (arc  '(40 0) 20 20 0  0 72)
    (arc  pt1     20 20 0 25 72)
    (text (xy+ pt1 -2 10) "(x1 y1)" :align :right)
    (text (y+  pt2    15) "(x2 y2)")
    (text '( 70 70) "degree" :align :left)
    (text '( 60 20) "θ" :align :left)))
<!-- end snippet -->

```kaavio
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

```kaavio
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

#### rectangle クラス
<!-- autolink: [rectangle クラス](#rectangle クラス) -->

　rectangle は shape クラスから派生し、以下のメンバを保持します。center, width、height 以外は nil が
許可されます。

```lisp
(defclass rectangle (shape)
  ((center  :initform nil :initarg :center)      ; point
   (width   :initform   0 :initarg :width)       ; number
   (height  :initform   0 :initarg :height)      ; number
   (rx      :initform nil :initarg :rx)          ; number
   (ry      :initform nil :initarg :ry)          ; number
   (fill    :initform nil :initarg :fill)        ; (or nil fill-info)
   (stroke  :initform nil :initarg :stroke)))    ; (or nil link-info)
```

　rectangle は四角形を描画します。

<!-- snippet: CLASS_DEF_RECTANGLE
class rectangle {
  point center
  number width
  number height
  number rx
  number ry
  fill-info fill
  stroke-info stroke
}
-->

```plantuml
@startuml
left to right direction
<!-- expand: CLASS_DEF_ENTITY -->
<!-- expand: CLASS_DEF_SHAPE -->
<!-- expand: CLASS_DEF_RECTANGLE -->
entity <|-- shape
shape  <|-- rectangle
@enduml
```

　rectangle クラス向けに、以下の総称関数のメソッドが実装されています。

* initialize-instance 
* check 
* shape-width
* shape-height
* shape-center
* draw-entity

#### rectangle マクロ
<!-- autolink: [rectangle マクロ](#rectangle マクロ) -->

```lisp
(defmacro rectangle (center width height
                     &key rx ry class fill stroke link layer id contents) ... )
```

　${{TODO}{まだ記述されていません。}}

<!--
(defmacro rectangle (center width height
					 &key rx ry class fill stroke link layer id contents)
  (let ((code `(register-entity (make-instance 'diagram:rectangle
											   :center ,center
											   :width ,width :height ,height
											   :rx ,rx :ry ,ry :class ,class
											   :fill ,fill :stroke ,stroke
											   :link ,link :layer ,layer :id ,id))))
	(if (null contents)
		code
		(let ((g-obj (gensym "OBJ")))
		  `(let* ((,g-obj ,code)
				  (canvas (diagram:shape-get-subcanvas ,g-obj)))
			 (declare (special canvas))
			 ,@contents)))))
-->

### shape.lisp
<!-- autolink: [$$](#shape.lisp) -->

　shape.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp
* point.lisp
* canvas.lisp
* mathutil.lisp
* entity.lisp
* link-info.lisp

#### shape クラス
<!-- autolink: [shape](#shape クラス) -->

　shape は class と link を保持します。どちらも nil が許可されます。

```lisp
(defclass shape (entity)
  ((class :initform nil :initarg :class)  ; keyword
   (link  :initform nil :initarg :link))) ; (or nil link-info)
```

　shape はおおまかに言って、entity のうち「矩形領域を持つもの」です。つまりコネクタなどの接続
線のようなものは含みません。

<!-- snippet: CLASS_DEF_SHAPE
class shape {
  keyword class
  link-info link
}
-->

```plantuml
@startuml
left to right direction
<!-- expand: CLASS_DEF_ENTITY -->
<!-- expand: CLASS_DEF_SHAPE -->
entity <|-- shape
@enduml
```

#### shape-width 総称関数
<!-- autolink: [shape-width](#shape-width 総称関数) -->

```lisp
(defgeneric shape-width (shp)) ... )
```

　shape かその派生クラスの「幅」を数値で返します。shape クラスでのデフォルト実装はありません。
派生クラスで実装する必要があります。

#### shape-height 総称関数
<!-- autolink: [shape-height](#shape-height 総称関数) -->

```lisp
(defgeneric shape-height (shp)) ... )
```

　shape かその派生クラスの「高さ」を数値で返します。shape クラスでのデフォルト実装はありません。
派生クラスで実装する必要があります。

#### shape-topleft 総称関数
<!-- autolink: [shape-topleft](#shape-topleft 総称関数) -->

```lisp
(defgeneric shape-topleft (shp)) ... )
```

　shape かその派生クラスの「左上の座標」を point で返します。shape クラスでのデフォルト実装では、
以下から計算します。

* shape-center
* shape-width
* shape-height

#### shape-top 総称関数
<!-- autolink: [shape-top](#shape-top 総称関数) -->

```lisp
(defgeneric shape-top (shp)) ... )
```

　shape かその派生クラスの「上端の座標」を point で返します。shape クラスでのデフォルト実装では、
以下から計算します。

* shape-center
* shape-height


#### shape-topright 総称関数
<!-- autolink: [shape-topright](#shape-topright 総称関数) -->

```lisp
(defgeneric shape-topright (shp)) ... )
```

　shape かその派生クラスの「右上の座標」を point で返します。shape クラスでのデフォルト実装では、
以下から計算します。

* shape-center
* shape-width
* shape-height

#### shape-left 総称関数
<!-- autolink: [shape-left](#shape-left 総称関数) -->

```lisp
(defgeneric shape-left (shp)) ... )
```

　shape かその派生クラスの「左端の座標」を point で返します。shape クラスでのデフォルト実装では、
以下から計算します。

* shape-center
* shape-width

#### shape-center 総称関数
<!-- autolink: [shape-center](#shape-center 総称関数) -->

```lisp
(defgeneric shape-center (shp)) ... )
```

　shape かその派生クラスの「中心」を point で返します。shape クラスでのデフォルト実装はありません。
派生クラスで実装する必要があります。

#### shape-right 総称関数
<!-- autolink: [shape-right](#shape-right 総称関数) -->

```lisp
(defgeneric shape-right (shp)) ... )
```

　shape かその派生クラスの「右端の座標」を point で返します。shape クラスでのデフォルト実装では、
以下から計算します。

* shape-center
* shape-width

#### shape-bottomleft 総称関数
<!-- autolink: [shape-bottomleft](#shape-bottomleft 総称関数) -->

```lisp
(defgeneric shape-bottomleft (shp)) ... )
```

　shape かその派生クラスの「左下の座標」を point で返します。shape クラスでのデフォルト実装では、
以下から計算します。

* shape-center
* shape-width
* shape-height

#### shape-bottom 総称関数
<!-- autolink: [shape-bottom](#shape-bottom 総称関数) -->

```lisp
(defgeneric shape-bottom (shp)) ... )
```

　shape かその派生クラスの「下端の座標」を point で返します。shape クラスでのデフォルト実装では、
以下から計算します。

* shape-center
* shape-height

#### shape-bottomright 総称関数
<!-- autolink: [shape-bottomright](#shape-bottomright 総称関数) -->

```lisp
(defgeneric shape-bottomright (shp)) ... )
```

　shape かその派生クラスの「右下の座標」を point で返します。shape クラスでのデフォルト実装では、
以下から計算します。

* shape-center
* shape-width
* shape-height

#### shape-get-subcanvas 総称関数
<!-- autolink: [shape-get-subcanvas](#shape-get-subcanvas 総称関数) -->

```lisp
(defgeneric shape-get-subcanvas (shp)) ... )
```

　shape のサブキャンバスを canvas で返します。shape クラスでのデフォルト実装では、
以下から計算します。つまり、単純に shape の矩形を返します。

* shape-topleft
* shape-width
* shape-height

#### shape-cc-center 総称関数
<!-- autolink: [shape-cc-center](#shape-cc-center 総称関数) -->

```lisp
(defgeneric shape-cc-center (shp type)) ... )
```

　connector が shape `:CC` で接続するときの中心点を point で返します。type には 
`:from` または `:dest` が渡されます。shape クラスでのデフォルト実装では、
`type` を無視して `(shape-center shp)` の呼び出し結果をそのまま返します。

　この総称関数は、特殊な shape のために用意されています。例としては uml-connector が
挙げられます。これは、単一の shape で２ケ所に円を描画し、 `:from` で connector に接続
する場合と `:dest` で connector に接続する場合で異なる円を使います。

#### shape-connect-point 総称関数
<!-- autolink: [shape-connect-point](#shape-connect-point 総称関数) -->

```lisp
(defgeneric shape-connect-point (shp type1 type2 arg)) ... )
```

　shape に connector を接続する際の接続点を決定します。

| param | description                                                                          |
|:-----:|:-------------------------------------------------------------------------------------|
| shp   | 接続点を決定したい shape オブジェクトが指定されます。                                   |
| type1 | connector のどちらの端点かが `:from, :dest` で指定されます。                           |
| type2 | connector の shp への接続先が `:center, :top, :bottom, :left, :right` で指定されます。 |
| arg   | type2 が `:center` の場合、point オブジェクトで、connector の反対側の端点が指定されます。<br> \
         type2 が `:center` でない場合、-1,0,1 で補助的な接続位置が指定されます。            |

　rectangle のような矩形の shape であれば、shape 向けのデフォルト実装で問題なく
機能するはずです。円や楕円などの形状や、もっと複雑な形状の場合、独自に実装をする必要が
あるかもしれません。

### stencil.lisp
<!-- autolink: [$$](#stencil.lisp) -->

　stencil.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp
* pathutil.lisp

### stroke-info.lisp
<!-- autolink: [$$](#stroke-info.lisp) -->

　stroke-info.lisp はストロークの指定を表現するクラスです。以下のコンポーネントに依存しています。

* cl-diagram.lisp

#### *default-stroke*変数
<!-- autolink: [*default-stroke*](#*default-stroke*変数) -->

```lisp
(defparameter *default-stroke* nil)
```

　デフォルトのストロークを表現するダイナミック変数です。初期状態では
`(make-stroke :color :black :width 1 :opacity nil :linecap nil :linejoin nil :miterlimit nil  \
:dasharray nil :dashoffset nil)` の結果が設定されます。

#### stroke-info クラス
<!-- autolink: [stroke-info](#stroke-info クラス) -->

　stroke-info は 8 種類のメンバを保持します。いずれも nil が許可されます。

```lisp
(defclass stroke-info ()
  ((color      :initform nil :initarg :color)          ; (or keyword string)
   (width      :initform nil :initarg :width)          ; number
   (opacity    :initform nil :initarg :opacity)        ; number
   (linecap    :initform nil :initarg :linecap)        ; (or nil keyword)
   (linejoin   :initform nil :initarg :linejoin)       ; (or nil keyword)
   (miterlimit :initform nil :initarg :miterlimit)     ; number
   (dasharray  :initform nil :initarg :dasharray)      ; list
   (dashoffset :initform nil :initarg :dashoffset)))   ; number
```

　stroke-info クラス向けに、以下の総称関数のメソッドが実装されています。

* initialize-instance 
* check 
* to-property-strings
* to-style-strings

#### make-stroke 関数
<!-- autolink: [make-stroke](#make-stroke 関数) -->

```lisp
(defun make-stroke (&rest params) ... )
```

　stroke-info インスタンスを作成します。パラメータの数や種類に応じて作り方が異なります。

* パラメータ無しの場合、 `*default-stroke*` を返します
* パラメータ数が 1 の場合、そのパラメータを param として
	* param が stroke-info インスタンスの場合、渡されたものをそのまま返します
	* param が数値の場合、 `(make-stroke :width param)` の結果を返します
	* param がリストの場合、 `(apply #'make-stroke param)` の結果を返します
	* 上記のいずれでもない場合、 `(make-stroke :color param)` の結果を返します
* パラメータ数が 2 以上の場合、以下の関数のように振舞います。

	```lisp
	(defun make-stroke (&key color opacity rule base) ... )
	```

	* `base` はその他のパラメータが省略された場合にデフォルト値を取得するための `stroke-info` インスタンスで、これが省略された場合は `*default-stroke*` が使用されます。

	* `color` は色の指定です。指定方法の詳細は「色の指定」を参照してください。
	* `width` でストロークの幅を指定します。数値で指定します。
	* `opacity` で透明度を指定します。0 から 1 までの浮動小数点数で指定します。
	* `linecap` は${{TODO}{まだ記述されていません}}  `:butt :round :square`
	* `linejoin` は${{TODO}{まだ記述されていません}} `:miter :round :bevel`
	* `miterlimit` は${{TODO}{まだ記述されていません}}
	* `dasharray` は${{TODO}{まだ記述されていません}}
	* `dashoffset` は${{TODO}{まだ記述されていません}}

#### with-stroke マクロ
<!-- autolink: [with-stroke](#with-stroke マクロ) -->

```lisp
(defmacro with-stroke ((&rest param) &rest body) ... )
```

　`*default-stroke*` を一時的に変更します。 `params` には make-stroke に与えるパラメータと
同じものを指定します。以下のコードは、

```lisp
(with-stroke (:color :blue :width 3)
   ... )
```

　以下と同等です。

```lisp
(let ((*default-stroke* (make-stroke :color :blue :width 3)))
   ... )
```

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


## Topics
### 円弧の端点における傾きの計算方法

　arc に endmark をサポートさせることになり、arc の端点の傾きを知る必要が発生。具体的には、
以下のような円弧のベースとなる楕円上における end1, end2 の接線の傾き、つまりそれら端点を通る
接線が x 軸となす角度の sin 値および cos 値を知る必要がある。以下における end1 であれば 
接線 L1 が x 軸となす角度 θ の sin θ および cos θ である。なお、この楕円の回転角を d1 と
する。

```kaavio
(diagram (200 150)
  (grid)
  (line '(( 30   0) ( 30 150)))
  (line '((  0 120) (200 120)))
  (let ((rx 50)
		(ry 30)
		(cc '(110 60))
		(rotation 30))
	(ellipse cc rx ry :rotate rotation :stroke '(:color :lightgray :width 8))
	(arc     cc rx ry rotation  330 90 :stroke '(:color :red :width 2) :id :arc)
    (with-options (:font 10)
      (text (xy+ arc.end1  5 -5) "end1" :align :left)
      (text (xy+ arc.end2 -5 15) "end2" :align :right))
    (with-options (:stroke :none :fill :red)
	  (circle cc       3)
	  (circle arc.end1 3)
	  (circle arc.end2 3))
    (line '((137 0) (175 150)) :stroke '(:color :blue :dasharray (2 2)))
    (arc '(167 120) 10 10 0 0 250 :stroke :blue)
    (with-options (:font '(:fill :blue))
      (text (xy+ $1.center -20 23) "θ")
      (text '(115 15) "L1" :align :left))))
```

　計算を簡単にするために、ベースとなる楕円を「正規化」して考える。これは、楕円の回転をなくし、
さらに中心を原点に持ってきたものである。この楕円の x 半径を a、y 半径を b とすると、この楕円の
方程式は x^2^/a^2^ + y^2^/b^2^ = 1 で与えられる。

```kaavio
(diagram (200 150)
  (grid)
  (line '((100  0) (100 150)))
  (line '((  0 75) (200  75)))
  (let ((rx 50)
		(ry 30)
		(cc '(100 75))
		(rotation 0))
	(ellipse cc rx ry :rotate rotation :stroke '(:color :lightgray :width 8))
	(arc     cc rx ry rotation  330 90 :stroke '(:color :red :width 2) :id :arc)
    (with-options (:font 10)
      (text (xy+ arc.end1  5 -5) "end1" :align :left)
      (text (xy+ arc.end2 -5 15) "end2" :align :right))
    (with-options (:stroke :none :fill :red)
	  (circle cc       3)
	  (circle arc.end1 3)
	  (circle arc.end2 3))))
```

　ここで、正規化後の楕円における end1 を通る接線 L2 を考える。端点 end1 の座標を (k, m) と
すると、L2 は kx/a^2^ + my/b^2^ = 1 で与えられるらしい。


```kaavio
(diagram (200 150)
  (grid)
  (line '((100  0) (100 150)))
  (line '((  0 75) (200  75)))
  (let ((rx 50)
		(ry 30)
		(cc '(100 75))
		(rotation 0))
	(ellipse cc rx ry :rotate rotation :stroke '(:color :lightgray :width 8))
	(arc     cc rx ry rotation  330 90 :stroke :none :id :arc)
    (with-options (:stroke :none :fill :red)
	  (circle cc       3)
	  (circle arc.end1 3))
    (with-options (:font '(:size 10 :fill :blue))
      (text '(65 15) "L2" :align :left)
      (text (xy+ arc.end1  5 -5) "(k, m)" :align :left))
    (line '((80 0) (200 115)) :stroke '(:color :blue :dasharray (2 2)))))
```

　ここで、L1 と L2 のなす角度は楕円の回転角 d1 に等しい。そのため、この接線が x 軸となす角 d2 の 
sin および cos がわかれば、あとは d1 と合成することで最終的に
求めたい sin θ と cos θ が得られる。つまり、θ = d1 + d2 より、以下の計算をすれば良い。

* sin θ = (sin d1 x cos d2) + (cos d1 x cos d2) 
* cos θ = (cos d1 x cos d2) + (sin d1 x sin d2) 

　楕円の回転角 d1 は既知であるので、sin d1 と cos d1 は問題なく計算できる。d2 については、
先の接線 L2 が x 軸および y 軸と交わる点から簡単に計算できる。つまり、kx/a^2^ + my/b^2^ = 1 に
おいて x = 0 の場合の y と、y = 0 の場合の x を計算するのである。ただし、L2 が x 軸または y 軸の
一方と交わらない、つまり並行になる場合には注意が必要である。そのため、以下の場合分けが必要になる。

1. k = 0 の場合
2. m = 0 の場合
3. その他の場合

　1 の場合は L2 が x 軸と平行になる場合であり、sin d2 は 0、cos d2 は ±1 である。

　2 の場合は L2 が y 軸と平行になる場合であり、sin d2 は ±1、cos d2 は 0 である。

　3 の場合は L2 が x,y 軸と平行にならないため、それぞれの軸と交わる点を使って計算するのが簡単である。
kx/a^2^ + my/b^2^ = 1 に x = 0、または y = 0 を代入して整理すると、交点はそれぞれ
(0, b^2^/m) と (a^2^/k, 0) であることがわかる。2 点がわかれば、あとは計算するだけである。

## 図表一覧
<!-- embed:figure-list -->

　　

<!-- embed:table-list -->

　　

## 索引
<!-- embed:index-x -->

--------------------------------------------------------------------------------

<!-- embed:footnotes -->

