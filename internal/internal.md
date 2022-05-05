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

　canvas.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp
* point.lisp

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

### dictionary.lisp
<!-- autolink: [$$](#dictionary.lisp) -->

　dictionary.lisp は以下のコンポーネントに依存しています。

* cl-diagram.lisp

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
* point.lisp

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

　一回の呼び出しで 1 行を出力します。 `params` で与えられた複数のパラメータを順番に出力します。
このとき、

##### writer-incr-level メソッド
<!-- autolink: [writer-incr-level](#writer-incr-level メソッド) -->
```lisp
(defmethod writer-incr-level ((writer buffer-writer)) ... )
```

##### writer-decr-level メソッド
<!-- autolink: [writer-decr-level](#writer-decr-level メソッド) -->
```lisp
(defmethod writer-decr-level ((writer buffer-writer)) ... )
```

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


--------------------------------------------------------------------------------

<!-- embed:footnotes -->

