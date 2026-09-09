
## いくつかの概念と機能

### 座標と位置

　kaavio では、座標系は左上端を原点としており、水平右方向に x 軸、垂直下方向に y 軸となって
います。また、角度は時計回りになります。

<!-- snippet: GEOMETRY-SAMPLE-1
(diagram (300 150)
  (grid)
  (circle canvas.topleft 4 :fill :black)
  (let ((em (make-endmark :type :triangle :fill :black :size :small)))
    (text '(5 15) "(0, 0)")
    (line '((250  10) (275  10)) :end2 em) (text '(280  15) "x" :align :left)
    (line '(( 10 100) ( 10 130)) :end2 em) (text '( 10 145) "y" :align :center)
    (line '((100  60) (180  60)))          (text '(185  65) "0°" :align :left)
    (line '((100  60) (140 130)))
    (arc   '(100  60) 30 30 0 1 60 :end2 em)
    (text '(130  85) "θ" :align :left)))
-->

<!-- figure:  kaavio における座標系 -->
```kaavio
<!-- expand: GEOMETRY-SAMPLE-1 -->
```
<!-- figure:end -->

<!-- collapse:begin -->
　※上記画像のソースはこちら。

```lisp
<!-- expand: GEOMETRY-SAMPLE-1 -->
```
<!-- collapse:end -->

${BLANK_PARAGRAPH}

　kaavio データの中で座標を指定する方法にはいくつかあります。以下に説明します。

　具体的な数値で座標を指定する場合、 `'(50 100)` といった要領で指定します。これは即値で
座標を指定する場合の書き方ですが、Common Lisp の変数に格納した数値から座標を作成したい
場合は make-point 関数が使えます。これは `(make-point x y)` の要領で使用してください
{{fn:Lisper の方へ：ご想像通り、 `(list x y)` でもいいですし、バッククォートを使ってもかまいません。}}。

　具体的な数値を指定するのでなく、「すでに登場した要素の属性を利用して位置を指定する」ことも
できます。[$@ 章](#簡単なサンプル)の 2 つめのサンプルでは、 `app.center` や `in.right` と
いう表記が登場しました。これは、図形要素を記述する際に指定した ID を使ってその中心座標などを
参照するものです。幅や高さを持つ図形要素では９種類あり、その名前と具体的な場所は以下の通りです
{{fn:この他に `width, height` があって、その図形要素の幅と高さを取得することもできますが、あまり \
使用しません。}}。図では `topleft(tl)` などの記載がありますが、括弧内の `tl` は簡略記法で、 
`foo.topleft` を `foo.tl` と書くこともできることを意味しています。

<!-- figure:  図形要素の座標参照 - 1 -->
```kaavio
(diagram (420 170)
  (grid)
  (rect canvas.center 160 120 :stroke :gray :fill :white :id :rct)
  (with-options (:stroke :none :fill :red :font '(:fill :red))
    (circle rct.TL 3) (text (y+  $1.topleft     -3) "topleft(tl)"     :align :right)
    (circle rct.TC 3) (text (y+  $1.top         -3) "top(tc)"         :align :center)
    (circle rct.TR 3) (text (y+  $1.topright    -3) "topright(tr)"    :align :left)
    (circle rct.CL 3) (text (xy+ $1.left     -5  5) "left(cl)"        :align :right)
    (circle rct.CC 3) (text (y+  $1.top         -3) "center(cc)"      :align :center)
    (circle rct.CR 3) (text (xy+ $1.right     5  5) "right(cr)"       :align :left)
    (circle rct.BL 3) (text (y+  $1.bottomleft  13) "bottomleft(bl)"  :align :right)
    (circle rct.BC 3) (text (y+  $1.bottom      13) "bottom(bc)"      :align :center)
    (circle rct.BR 3) (text (y+  $1.bottomright 13) "bottomright(br)" :align :left)))
```
<!-- figure:end -->

${BLANK_PARAGRAPH}

　さらに、コネクタの `:style` パラメータ指定で使う `L1` や `T3` といった記法も使用できます。

<!-- figure:  図形要素の座標参照 - 2 -->
```kaavio
(diagram (420 170)
  (grid)
  (rect canvas.center 160 120 :stroke :gray :fill :white :id :rct)
  (with-options (:stroke :none :fill :red :font '(:fill :red))
    (circle rct.T1 3) (text (xy+ $1.T1  0 -3) "T1" :align :center)
    (circle rct.T2 3) (text (xy+ $1.T2  0 -3) "T2" :align :center)
    (circle rct.T3 3) (text (xy+ $1.T3  0 -3) "T3" :align :center)
    (circle rct.B1 3) (text (xy+ $1.B1  0 13) "B1" :align :center)
    (circle rct.B2 3) (text (xy+ $1.B2  0 13) "B2" :align :center)
    (circle rct.B3 3) (text (xy+ $1.B3  0 13) "B3" :align :center)
    (circle rct.L1 3) (text (xy+ $1.L1 -3  7) "L1" :align :right)
    (circle rct.L2 3) (text (xy+ $1.L2 -3  7) "L2" :align :right)
    (circle rct.L3 3) (text (xy+ $1.L3 -3  7) "L3" :align :right)
    (circle rct.R1 3) (text (xy+ $1.R1  3  7) "R1" :align :left)
    (circle rct.R2 3) (text (xy+ $1.R2  3  7) "R2" :align :left)
    (circle rct.R3 3) (text (xy+ $1.R3  3  7) "R3" :align :left)))
```
<!-- figure:end -->

${BLANK_PARAGRAPH}

　上記の記法は全て座標値を返しますが、これらにさらに `.x` や `.y` をつけて x 軸や y 軸の座標値を
取得することができます。これを利用すると、 `(rect (make-point obj1.cc.x obj2.cc.y) ...)`　などの
記述によって「縦方向を obj1 にあわせ、横方向を obj2 にあわせる」といったことができます。

　このようにして指定する座標を使う局面としてもっとも一般的なのは図形要素の位置指定です。たとえば
四角形 rect などは `position` パラメータで座標値を取ります。これは通常「その図形要素の中心点」
として使用されますが、 `pivot` パラメータがある場合はこれを中心点以外で使用することができます。
`pivot` に指定できるのは `:TL :TC :TR :CL :CC :CR :BL :BC :BR` のいずれかで、デフォルト値は 
`:CC` です。この指定により、 `position` で指定した座標が図形要素のどこに来るように描画されるか
を制御できます。以下の例では、四角形 `rct` に対して `(diamond rct.cr 60 40 :pivot :CL)` と
することで「ひし形の左端が四角形の右端にくるように位置指定」しています。

<!-- figure:  pivot パラメータの利用例 -->
```kaavio
(diagram (200 100)
  (grid)
  (rect '(70 50) 50 50 :id :rct)
  (diamond rct.cr  60 40 :pivot :CL)
  (circle rct.cr 3 :stroke :none :fill :red))
```
<!-- figure:end -->


　直線や円弧、コネクタ、およびブロック矢印では、 `center` および線の端点として `end1, end2` が
利用できます。以下のように、この場合の `center` は線の総延長のちょうど半分にあたる位置になります
（円弧の場合はベースとなる楕円の中心です）。

<!-- figure:  図形要素の座標参照 - 3 -->
```kaavio
(diagram (300 110)
  (grid)
  (line '((50 40) (100 40) (100 80) (250 80)) :stroke 1 :id :line)
  (with-options (:stroke :none :fill :red)
    (circle line.end1   3)
    (circle line.center 3)
    (circle line.end2   3))
  (with-options (:font '(:fill :red))
    (text (xy+ line.end1   0 -10) "end1"       :align :center)
    (text (xy+ line.center 0 -10) "center(cc)" :align :left)
    (text (xy+ line.end2   0 -10) "end2"       :align :center)))
```
<!-- figure:end -->

${BLANK_PARAGRAPH}

　前述の `app.center` という記法は、座標を指定すべきところではたいてい使用できますが、これを 
`(attr :app :center)` という記法で代替することもできます{{fn:Lisper の方へ： `app.center` という記法は動的に \
生成する symbol-macrolet によって、また `(attr :app :center)` については macrolet によって実現しています。 \
attr は局所関数を使って同じパターンの繰り返しを共通化する場合などに便利でしょう。}}。

　`app.center` の記法において要素名のところに `canvas` を指定することで、キャンバス全体を
ひとつの図形要素のように扱うことができます。つまり、 `canvas.cc` とすれば SVG 画像の
中心点を指定できますし、 `canvas.width` と言えば SVG 画像の幅を取得することができます。
実際には、この `canvas` が意味するのは「現在のキャンバス」なのですが、これについては
「[](#サブキャンバス)」で説明します。

* ${{TODO}{このあたりで相対座標と絶対座標について触れる：詳細はサブキャンバスの説明で、かな。}}

　`app.cc` などの記述は単独で使用するよりも、「app の中心から 100pt くらい右」といった
指定をしたい場合の方が多いでしょう。そのような場合、 `(x+ app.cc 100)` といった記述で
目的を達することができます。以下の 3 つの関数が利用できます。

```lisp
(defun x+ (pt x) ...)
(defun y+ (pt y) ...)
(defun xy+ (pt x y) ...)
```

　なお、 `(x- app.cc 100)` とは書けません。 `(x+ app.cc -100)` としてください。

${BLANK_PARAGRAPH}

### サブキャンバス
<!-- autolink: [$$](#サブキャンバス) -->
<!-- autolink: [キャンバス](#サブキャンバス) -->

　[$@ 節](#座標と位置)では、座標指定のための ID 名として `canvas` を指定すると作成中の図全体の
領域を指定できると説明しました。kaavio では、これをキャンバスと読んでいますが、その一部を
独立したキャンバスとして描画を行うことができます。これをサブキャンバスと呼びます。

　サブキャンバスを使う方法のひとつは、with-subcanvas マクロを使うことです。左上の座標と
幅・高さを与えることで、その部分領域の左上を原点とする新しい座標系が作成されます。以下に
簡単な例を示します。

<!-- snippet: SUBCANVAS-SAMPLE-1
(diagram (300 150)
  (grid)
  (circle '(50 50) 20 :stroke :brown :fill :thistle)
  (with-subcanvas ('(150 40) 100 100)
    (rect canvas.center canvas.width canvas.height :stroke :gray :fill :none)
    (circle '(50 50) 20 :stroke :navy :fill :skyblue)))
-->

```lisp
<!-- expand: SUBCANVAS-SAMPLE-1 -->
```

　上記のコードを kaavio に通すと以下が生成されます。2 回登場する circle は座標と半径が
同じ `'(50 50) 20` で指定されていますが、実際に描画された場所は異なっています。これは、後者の
（青い方の）circle が with-subcanvas マクロの配下にあるためで、このサブキャンバスの実際の領域
は rect で示されています。

<!-- figure:  サブキャンバスのサンプル -->
```kaavio
<!-- expand: SUBCANVAS-SAMPLE-1 -->
```
<!-- figure:end -->

　サブキャンバスは入力データの一部分で独自の座標系を一時的に作成するもので、それ以外の効果は
ありません。たとえば、描画順序を制御するレイヤーとは無関係ですし、サブキャンバスの矩形で
描画内容をクリッピングすることもしません。つまり、サブキャンバスの機能自体には実質的に原点を
ズラす効果しかありません
{{fn:サブキャンバスが自動的なクリッピングをしないのは、kaavio が作図指向で絵画的な効果を重視していない \
という理由があります。}}。
クリッピングを行ないたい場合は [$@ 節](#クリッピング)を参照してください。

　with-subcanvas マクロで明示的にサブキャンバスを作成するのでなく、作成した図形要素の内部を
サブキャンバスとすることもできます。これには、 `:contents` パラメータを使用します。たとえば、
先程の例と同じ作図をするには以下のように書きます。この場合、先程とは違って rect の中に 
circle が置かれることになります（rect が動けば circle も動く）。

<!-- figure:  contents パラメータを使ったサブキャンバス -->
```lisp
(diagram (300 150)
  (grid)
  (circle '(50 50) 20 :stroke :brown :fill :thistle)
  (rect '(200 90) 100 100 :stroke :gray :fill :none
     :contents
     ((circle '(50 50) 20 :stroke :navy :fill :skyblue))))
```
<!-- figure:end -->


　`:contents` パラメータによるサブキャンバスは、その図形要素の幅と高さからなる四角形に
なるのが原則です。つまり、円や楕円の場合は以下のようにサブキャンバスの方が大きくなり
ますので注意してください。

```kaavio
(diagram (300 120)
  (grid)
  (let ((st (make-stroke :width 3 :color :red :opacity 0.3 :dasharray '(10 5))))
    (circle  '(70 60) 40 :fill :lightgray :stroke :black
      :contents
      ((rect canvas.center canvas.width canvas.height :stroke st)))
    (ellipse '(200 60) 70 40 :fill :lightgray :stroke :black
      :contents
      ((rect canvas.center canvas.width canvas.height :stroke st)))))
```

　一部の図形要素では、サブキャンバスの位置が調整されている場合があります。たとえば、
cube では以下のようになります。青い点線の枠が cube の幅と高さからなる矩形で、赤い点線の
枠がサブキャンバスです。

```kaavio
(diagram (300 200)
  (grid)
  (let ((st (make-stroke :width 3 :color :red :opacity 0.5 :dasharray '(10 5))))
    (cube (xy+ canvas.center -10 10) (- canvas.width 60) (- canvas.height 60) ""
      :fill :white :fill2 :lightgray :stroke :black :id :node
      :contents
      ((rect canvas.center canvas.width canvas.height :stroke st)))
    (rect node.center node.width node.height
                      :stroke (make-stroke :color :blue :base st))))
```

　なお、 `:contents` パラメータを使わずに図形要素のサブキャンバスを利用する方法として、
with-subcanvas-of マクロが用意されています。これは既出の図形要素の ID を指定して
サブキャンバスを確立するものです。[$@](F#サブキャンバスのサンプル) と同じ
（つまり [$@](F#contents パラメータを使ったサブキャンバス) とも同じ）作図をする
サンプルを以下に示します。

<!-- figure:  with-subcanvas-of を使ったサブキャンバス -->
```lisp
(diagram (300 150)
  (grid)
  (circle '(50 50) 20 :stroke :brown :fill :thistle)
  (rect '(200 90) 100 100 :stroke :gray :fill :none :id :rct)
  (with-subcanvas-of (:rct)
     (circle '(50 50) 20 :stroke :navy :fill :skyblue)))
```
<!-- figure:end -->

${BLANK_PARAGRAPH}


　最後に with-current-canvas マクロを紹介しておきます。with-subcanvas マクロと 
with-subcanvas-of マクロは新しいサブキャンバスを確立するものでしたが、
with-current-canvas マクロは「現在のキャンバスへのアクセスを簡単にする」ものです。
キャンバスを使っていると、 `canvas.center, canvas.width, canvas.height` などを頻繁に
使うことになりますが、これらに短い名前でアクセスできるようにします。たとえば、
[本節冒頭の例](#サブキャンバス)は、以下のように書き換えることができます
（１回ずつしか使ってないのでメリットがわかりにくいですが）。

<!-- figure:  with-current-canvas の使用 -->
```lisp
(diagram (300 150)
  (grid)
  (circle '(50 50) 20 :stroke :brown :fill :thistle)
  (with-subcanvas ('(150 40) 100 100)
    (with-current-canvas ((cc center) (w width) (h height))
      (rect cc w h :stroke :gray :fill :none)
      (circle '(50 50) 20 :stroke :navy :fill :skyblue))))
```
<!-- figure:end -->


${BLANK_PARAGRAPH}

### クリッピング
<!-- autolink: [$$](#クリッピング) -->

　クリッピング機能を使えば、図面の一部だけを切り取ったように描画することができま
す。サブキャンバスからはみ出す部分が描画されないようにしたり、任意の形状（パス）
でクリッピングすることもできます。

　わざとらしい例ですが、以下のような図を考えましょう。大きな矩形の四隅に、それぞ
れはみ出すように４つの要素が置かれています。

<!-- snippet: DEFS-CLIPPING-SAMPLE-1
(diagram (280 160)
  (grid)
  (drop-shadow)
  (rect canvas.cc 160 100 :stroke :black :fill :beige :id :rct :filter :drop-shadow)
  (with-subcanvas-of (:rct)
    (rect            '( 10  10) 40 40 :stroke :navy      :fill :lightblue)
    (circle          '(150  10) 25    :stroke :maroon    :fill :darksalmon)
    (diamond         '( 10  90) 50 50 :stroke :darkgreen :fill :lightgreen)
    (regular-polygon '(150  90)  5 28 :stroke :black     :fill :lightgray)))
-->

```kaavio
<!-- expand: DEFS-CLIPPING-SAMPLE-1 -->
```

　上記図面のコードは以下の通りです。大きな矩形 `:rct` のサブキャンバス内で 4 つ
の図形要素を描画しています。

```lisp
<!-- expand: DEFS-CLIPPING-SAMPLE-1 -->
```

<!-- snippet: DEFS-CLIPPING-SAMPLE-2
(diagram (280 160)
  (grid)
  (drop-shadow)
  (rect canvas.cc 160 100 :stroke :black :fill :beige :id :rct :filter :drop-shadow)
  (with-subcanvas-of (:rct)
    (with-clipping-current-canvas    ;; ADDED
      (rect            '( 10  10) 40 40 :stroke :navy      :fill :lightblue)
      (circle          '(150  10) 25    :stroke :maroon    :fill :darksalmon)
      (diamond         '( 10  90) 50 50 :stroke :darkgreen :fill :lightgreen)
      (regular-polygon '(150  90)  5 28 :stroke :black     :fill :lightgray))))
-->

　では、クリッピング機能を使用してはみ出した部分が描画されないようにしてみましょう。
with-clipping-current-canvas マクロを使用して、4 つの要素を描画するコードを括る
だけです。以下のように。

```lisp
<!-- expand: DEFS-CLIPPING-SAMPLE-2 -->
```

　結果は以下のようになります。

<!-- figure:  with-clipping-current-canvas マクロの例 -->
```kaavio
<!-- expand: DEFS-CLIPPING-SAMPLE-2 -->
```
<!-- figure:end -->


<!-- snippet: DEFS-CLIPPING-SAMPLE-3
(diagram (280 160)
  (grid)
  (drop-shadow)
  (ellipse canvas.cc 100 60 :stroke :black :fill :beige :id :ellipse :filter :drop-shadow)
  (with-clipping-use (:ellipse)
    (rect            '( 70  40) 40 40 :stroke :navy      :fill :lightblue)
    (circle          '(210  40) 25    :stroke :maroon    :fill :darksalmon)
    (diamond         '( 70 120) 50 50 :stroke :darkgreen :fill :lightgreen)
    (regular-polygon '(210 120)  5 28 :stroke :black     :fill :lightgray)))
-->

　サブキャンバスは常に矩形なので、たとえば円形の領域で上記の方法を使うと期待通り
にはいきません。その場合、with-clipping-use マクロが使えるかもしれません。
以下の例では、楕円でクリッピングをしています。

```lisp
<!-- expand: DEFS-CLIPPING-SAMPLE-3 -->
```

　結果は以下のようになります。with-clipping-use マクロはサブキャンバスとは無関係なので、
この例では（クリッピングされる）4 つの図形要素の位置指定がこれまでと異なることに注意
してください。

<!-- figure:  with-clipping-use マクロの例 -->
```kaavio
<!-- expand: DEFS-CLIPPING-SAMPLE-3 -->
```
<!-- figure:end -->

　より複雑なパスでクリッピングを行なうことも可能です。以下では、文字を使ってクリッピング
をしています。

<!-- snippet: DEFS-CLIPPING-SAMPLE-4
(diagram (280 160)
  (grid)
  (text (y+ canvas.cc 60) "B" :align :center :id :char
        :font '(:family "Courier New" :size 140
                :stroke 3 :fill :lightgray :weight :bolder))
  (with-clipping-use (:char)
    (explosion1 (y+ canvas.cc 10) 110 110 "" :fill :pink :stroke :red)))
-->

```lisp
<!-- expand: DEFS-CLIPPING-SAMPLE-4 -->
```

<!-- figure:  文字を使ったクリッピングの例 -->
```kaavio
<!-- expand: DEFS-CLIPPING-SAMPLE-4 -->
```
<!-- figure:end -->

### 定義と再使用

　defgroup マクロと use マクロを使うことによって、複数の図形要素をひとつにまとめ、
図面の中で繰り返し使用することができます。要点を明確にするために、まずは同じ図形
（ちょっと装飾のついた四角形）を３回描画することを考えてみてください。まずは単純に
記述を３回繰り返します。座標以外は完全に同じ内容です。

<!-- snippet: DEFS-USE-SAMPLE-1
(diagram (200 100)
  (grid)
  (rect '( 50 50) 40 40 :fill :white :stroke :black
        :contents
        ((line '((0 10) (40 10)) :stroke :black)
         (line '((10 0) (10 40)) :stroke :black)))
  (rect '(100 50) 40 40 :fill :white :stroke :black
        :contents
        ((line '((0 10) (40 10)) :stroke :black)
         (line '((10 0) (10 40)) :stroke :black)))
  (rect '(150 50) 40 40 :fill :white :stroke :black
        :contents
        ((line '((0 10) (40 10)) :stroke :black)
         (line '((10 0) (10 40)) :stroke :black))))
-->

```lisp
<!-- expand: DEFS-USE-SAMPLE-1 -->
```

```kaavio
<!-- expand: DEFS-USE-SAMPLE-1 -->
```

　この場合、出力される SVG ファイル内でも（当然ながら）座標以外は同じ出力が３回繰り返されます。

```
<svg xmlns="http://www.w3.org/2000/svg" ...>
      :
    <rect x="30" y="30" width="40" height="40" ...></rect>
    <polyline ... points=" 30.0,40.0 70.0,40.0"></polyline>
    <polyline ... points=" 40.0,30.0 40.0,70.0"></polyline>

    <rect x="80" y="30" width="40" height="40" ...></rect>
    <polyline ... points=" 80.0,40.0 120.0,40.0"></polyline>
    <polyline ... points=" 90.0,30.0 90.0,70.0"></polyline>

    <rect x="130" y="30" width="40" height="40" ...></rect>
    <polyline ... points=" 130.0,40.0 170.0,40.0"></polyline>
    <polyline ... points=" 140.0,30.0 140.0,70.0"></polyline>
</svg>
```

　以下のように、Common Lisp 言語の機能を使用してループで処理することはできますが、入力データ
が短くなっても出力される SVG が短くなるわけではありません。

```lisp
(diagram (200 100)
  (grid)
  (dotimes (i 3)
    (rect `(,(* (1+ i) 50) 50) 40 40 :fill :white :stroke :black
          :contents
          ((line '((0 10) (40 10)) :stroke :black)
           (line '((10 0) (10 40)) :stroke :black)))))
```


　これに対して、defgroup マクロと use マクロを使うと SVG 上でも「一度だけ定義して複数回
描画させる」ような出力をすることができます。先程のサンプルと同じ出力をするコードは以下の
ようになります。

```lisp
(diagram (200 100)
  (grid)
  (defgroup (40 40 :icon)
    (rect canvas.center canvas.width canvas.height :fill :white :stroke :black
          :contents
          ((line `((0 10) (40 10)) :stroke :black)
           (line `((10 0) (10 40)) :stroke :black))))
  (use :icon '( 50 50))
  (use :icon '(100 50))
  (use :icon '(150 50)))
```

　上記のコードでは、 `(defgroup (40 40 :icon) ...)` によって icon という名前の定義を作成
しています。幅と高さはそれぞれ 40 です。これによって独立したキャンバスが確立され、その中
で作図を行うことができます。ここではそのキャンバスいっぱいに rect を描き、さらにその中で 
line を 2 本描いています。しかしこれは defgroup マクロの中でのこと（つまり定義を作成した
だけ）なので、これだけでは描画は行なわれません。defgroup マクロで定義した図形を実際に描画
するには、use マクロを使います。上記の例では、 `(use :icon '( 50 50))` といった記述を 3 回
行なっています。パラメータは、定義名と描画する基準座標です。

　上記のコードによって生成される SVG 画像は以下のようになります。入力データとの対応がわかると
思います。定義（defgroup）は一度だけで、それを参照（use）するタグが複数登場しています。

```
<svg xmlns='http://www.w3.org/2000/svg' ...>
    <defs>
        <g id='icon'>
            <rect x='0' y='0' width='40' height='40' ... />
            <polyline ... points=' 0.0,10.0 40.0,10.0' />
            <polyline ... points=' 10.0,0.0 10.0,40.0' />
        </g>
    </defs>
      :
    <use xlink:href='#icon' x='30' y='30' />
    <use xlink:href='#icon' x='80' y='30' />
    <use xlink:href='#icon' x='130' y='30' />
</svg>
```

　このように、defgroup マクロと use マクロを使えば繰り返し登場する図形要素の再利用が
可能になりますが、スタンプのようにまったく同じものを表示させることしかできないので
しょうか。また、use マクロで描画した要素どうしをコネクタで接続したりできないでしょうか。
最初の質問については、「基本的に定義した通りにしか描画できないけど、use マクロで contents 
パラメータが使えます」が答えになります。そして２番目の質問の答えは、「use マクロに ID を
付ければコネクタが使える」です。以下の例ではその両方をやっています。

<!-- snippet: DEFGROUP-USE-SAMPLE
(diagram (300 150)
  (grid)
  (defgroup (70 50 :frame)
    (with-current-canvas (center width height)
      (rect center width height :fill :white :stroke :black)
      (line `((0 10) (,width 10)) :stroke :black)))
  (use :frame '(75 50) :id :frame1
       :contents
       ((text (y+ canvas.center 10) "frame 1" :align :center)))
  (use :frame '(225 100) :id :frame2
       :contents
       ((text (y+ canvas.center 10) "frame 2" :align :center)))
  (connect :frame1 :frame2 :end2 :arrow))
-->

```lisp
<!-- expand: DEFGROUP-USE-SAMPLE -->
```

<!-- figure:  defgroup と use のサンプル -->
```kaavio
<!-- expand: DEFGROUP-USE-SAMPLE -->
```
<!-- figure:end -->

${BLANK_PARAGRAPH}

　注意してほしいのは、defgroup マクロで作成する定義に指定する ID と、図形要素を描画する
時に指定する ID は別モノだということです。上記の例で言えば、 `:frame` は定義の ID なので、
use マクロの最初のパラメータには使えますがコネクタの接続対象としては指定できません。逆も
同様で、たとえば rect を描いた後にその ID を指定して use することはできません。

* ${{TODO}{defgroup の中で use できない（っぽい：要確認）ことに言及する必要がある。}}

${BLANK_PARAGRAPH}

### パターンとグラデーション

　「フィル」では通常塗り潰しを指定しますが、定義した図形を敷き詰める「パターン」や、複雑な色の
変化を見せる「グラデーション」も利用できます。また、パターンとグラデーションはフィルだけでなく
ストロークで使用することも可能です。

#### パターン
<!-- autolink: [$$](#パターン) -->

　パターンを定義するには defpattern を使用します。 defpattern は defgroup に良く
似ていますが、パターンを定義するための構文です。以下のサンプルでは 5 x 5 の小さな領域
に細い青線を斜めにひくパターンを定義し、rect の中にそれを敷き詰めています。

<!-- snippet: PATTERN-1ST-SAMPLE
(diagram (140 70)
  (defpattern (:tile :width 5 :height 5 :units :userSpaceOnUse)
    (line '((5 0) (0 5)) :stroke '(:color :blue :width 0.3)))
  (rect canvas.center 100 50 :stroke :black :fill '(:url :tile)))
-->

```lisp
<!-- expand: PATTERN-1ST-SAMPLE -->
```

　上記のコードは以下の図を生成します。defpattern で定義したパターンに `:tile` という
ID をつけ、後続の rect の [fill パラメータ](#フィル)で `'(:url :tile)` という
指定をすることでパターンの使用を指示しています。

<!-- figure:  単純なパターンのサンプル -->
```kaavio
<!-- expand: PATTERN-1ST-SAMPLE -->
```
<!-- figure:end -->

${BLANK_PARAGRAPH}


　せっかくなのでサンプルをもうひとつ。センス云々はともかくとして、お望みならこんなカラフルな
パターンも作成できます。

<!-- snippet: PATTERN-2ND-SAMPLE
(diagram (140 140)
  (defpattern (:crazy :width 40 :height 40 :units :userSpaceOnUse)
    (circle '(10 10) 8     :stroke :none :fill :red)
    (rect   '(30 10) 14 14 :stroke :none :fill :blue)
    (rect   '(10 30) 12 12 :stroke :none :fill :green :rotate 45)
    (circle '(30 30) 8     :stroke :none :fill :orange))
  (rect canvas.center 120 120 :stroke :black :fill '(:url :crazy)))
-->

<!-- figure:  パターンのサンプル - 2 -->
```kaavio
<!-- expand: PATTERN-2ND-SAMPLE -->
```
<!-- figure:end -->

<!-- collapse:begin -->
　※上記画像のソースはこちら。

```lisp
<!-- expand: PATTERN-2ND-SAMPLE -->
```
<!-- collapse:end -->

${BLANK_PARAGRAPH}

　defpattern には `x, y, units, content-units, view-box` などの名前付きパラメータが
ありますが、現時点ではその詳細な説明は割愛します。今後説明を充実させる可能性はありますが、
現時点では SVG本の８章をお読みください。kaavio で SVG本の 8.1 節のサンプルを実現する
コードを以下に提示しておきます。なお、現在 preserveAspectRatio 属性には対応していません。
将来対応する可能性はありますが、未確定です。

<!-- collapse:close -->
　※「SVG エッセンシャルズ 第二版」 8.1 節の図面サンプルはこちら


__◆ 図 8.2__

<!-- snippet: BIBLE-8.2-SAMPLE
(diagram (400 170)
  (defpattern (:tile1 :x 0 :y 0 :width "20%" :height "20%" :units :objectBoundingBox)
    (raw-svg "<path d='M 0 0 Q 5 20 10 10 T 20 20' stroke='black' fill='none' />")
    (raw-svg "<path d='M 0 0 h 20 v 20 h -20 z'    stroke='gray'  fill='none' />"))
  (rect '( 70 70) 100 100 :stroke :black :fill '(:url :tile1))
  (rect '(170 60)  70  80 :stroke :black :fill '(:url :tile1))
  (rect '(300 85) 150 130 :stroke :black :fill '(:url :tile1)))
-->
```kaavio
<!-- expand: BIBLE-8.2-SAMPLE -->
```
```lisp
<!-- expand: BIBLE-8.2-SAMPLE -->
```

${BLANK_PARAGRAPH}

__◆ 図 8.3__

<!-- snippet: BIBLE-8.3-SAMPLE
(diagram (400 170)
  (defpattern (:tile2 :width 20 :height 20 :units :userSpaceOnUse)
    (raw-svg "<path d='M 0 0 Q 5 20 10 10 T 20 20' stroke='black' fill='none' />")
    (raw-svg "<path d='M 0 0 h 20 v 20 h -20 z'    stroke='gray'  fill='none' />"))
  (rect '( 70 70) 100 100 :stroke :black :fill '(:url :tile2))
  (rect '(170 60)  70  80 :stroke :black :fill '(:url :tile2))
  (rect '(300 85) 150 130 :stroke :black :fill '(:url :tile2)))
-->
```kaavio
<!-- expand: BIBLE-8.3-SAMPLE -->
```
```lisp
<!-- expand: BIBLE-8.3-SAMPLE -->
```

${BLANK_PARAGRAPH}

__◆ 図 8.4__

<!-- snippet: BIBLE-8.4-SAMPLE
(diagram (400 170)
  (defpattern (:tile3 :width ".2" :height ".2" 
               :units :objectBoundingBox :content-units :objectBoundingBox)
    (raw-svg "<path d='M 0 0 Q .05 .20 .10 .10 T .20 .20' stroke='black' stroke-width='0.01' fill='none' />")
    (raw-svg "<path d='M 0 0 h 0.20 v 0.20 h -0.2 z'      stroke='gray'  stroke-width='0.01' fill='none' />"))
  (rect '( 70 70) 100 100 :stroke :black :fill '(:url :tile3))
  (rect '(170 60)  70  80 :stroke :black :fill '(:url :tile3))
  (rect '(300 85) 150 130 :stroke :black :fill '(:url :tile3)))
-->
```kaavio
<!-- expand: BIBLE-8.4-SAMPLE -->
```
```lisp
<!-- expand: BIBLE-8.4-SAMPLE -->
```

${BLANK_PARAGRAPH}

__◆ 図 8.5__

<!-- snippet: BIBLE-8.5-SAMPLE
(diagram (140 140)
  (defpattern (:tile4 :width 20 :height 20 
               :units :userSpaceOnUse :view-box '(0 0 150 150))
    (raw-svg "<path d='M 30 100 C 50 50, 70 20, 100 100, 110, 130, 45, 150, 65, 100' stroke='black' stroke-width='5' fill='none' />"))
  (rect canvas.center 100 100 :stroke :black :fill '(:url :tile4)))
-->
```kaavio
<!-- expand: BIBLE-8.5-SAMPLE -->
```
```lisp
<!-- expand: BIBLE-8.5-SAMPLE -->
```

${BLANK_PARAGRAPH}

__◆ 図 8.6__

<!-- snippet: BIBLE-8.6-SAMPLE
(diagram (110 110)
  (defpattern (:stripe :width 6 :height 6 :units :userSpaceOnUse)
    (raw-svg "<path d='M 0 0 6 0' stroke='black' fill='none' />"))
  (defpattern (:polcadot :width 36 :height 36 :units :userSpaceOnUse)
    (circle '(18 18) 12 :fill '(:url :stripe) :stroke :black))
  (rect '(54 54) 100 100 :stroke :black :fill '(:url :polcadot)))
-->
```kaavio
<!-- expand: BIBLE-8.6-SAMPLE -->
```
```lisp
<!-- expand: BIBLE-8.6-SAMPLE -->
```

<!-- collapse:end -->

${BLANK_PARAGRAPH}

#### グラデーション
<!-- autolink: [$$](#グラデーション) -->

　グラデーションを定義するには defgradient を使用します。グラデーションには線型と円形の
２種類があります。以下のサンプルでは、青から赤に連続的に変換するグラデーションを定義し、
rect の中でそれを利用しています。

<!-- snippet: GRADIENT-1ST-SAMPLE
(diagram (140 70)
  (defgradient (:linear :gradient1)
    (0.00 :blue)
    (1.00 :red))
  (rect canvas.center 100 50 :stroke :black :fill '(:url :gradient1)))
-->

```lisp
<!-- expand: GRADIENT-1ST-SAMPLE -->
```

　上記のコードは以下の図を生成します。defgradient で定義したグラデーションに `:gradient1` と
いうID をつけ、後続の rect の [fill パラメータ](#フィル)で `'(:url :gradient1)` という
指定をすることでグラデーションの使用を指示しています。

<!-- figure:  単純なグラデーションのサンプル -->
```kaavio
<!-- expand: GRADIENT-1ST-SAMPLE -->
```
<!-- figure:end -->

${BLANK_PARAGRAPH}


　もうひとつのサンプルは円形グラデーションです。今度は単色で不透明度を変化させています。

<!-- snippet: GRADIENT-2ND-SAMPLE
(diagram (140 140)
  (grid)
  (defgradient (:radial :gradient2 :radius "70%")
    (0.00 :green 0.0)
    (1.00 :green 1.0))
  (rect '(70 70) 100 100 :stroke :black :fill '(:url :gradient2)))
-->

<!-- figure:  グラデーションのサンプル - 2 -->
```kaavio
<!-- expand: GRADIENT-2ND-SAMPLE -->
```
<!-- figure:end -->

<!-- collapse:begin -->
　※上記画像のソースはこちら。

```lisp
<!-- expand: GRADIENT-2ND-SAMPLE -->
```
<!-- collapse:end -->

${BLANK_PARAGRAPH}

　defgradient には多くの名前付きパラメータがありますが、現時点ではその詳細な説明は割愛します。
今後説明を充実させる可能性はありますが、現時点では SVG本の８章をお読みください。kaavio で 
SVG本の 8.2 節のサンプルを実現するコードを以下に提示しておきます。


<!-- collapse:close -->
　※「SVG エッセンシャルズ 第二版」 8.2 節の図面サンプルはこちら

__◆ 図 8.7__

<!-- snippet: BIBLE-8.7-SAMPLE
(diagram (240 120)
  (grid)
  (defgradient (:linear :two_hues_8_7)
    (0.0 "#ffcc00")
    (1.0 "#0099cc"))
  (rect canvas.center 200 100 :stroke :black :fill '(:url :two_hues_8_7)))
-->
```kaavio
<!-- expand: BIBLE-8.7-SAMPLE -->
```
```lisp
<!-- expand: BIBLE-8.7-SAMPLE -->
```

${BLANK_PARAGRAPH}

__◆ 図 8.8__

<!-- snippet: BIBLE-8.8-SAMPLE
(diagram (240 120)
  (grid)
  (defgradient (:linear :three_stops_8_8)
    (0.00 "#ffcc00")
    (0.33 "#cc6699")
    (1.00 "#66cc99"))
  (rect canvas.center 200 100 :stroke :black :fill '(:url :three_stops_8_8)))
-->
```kaavio
<!-- expand: BIBLE-8.8-SAMPLE -->
```
```lisp
<!-- expand: BIBLE-8.8-SAMPLE -->
```

${BLANK_PARAGRAPH}

__◆ 図 8.9__

<!-- snippet: BIBLE-8.9-SAMPLE
(diagram (240 120)
  (grid)
  (defgradient (:linear :three_stops_8_9)
    (0.00 "#906" 1.0)
    (0.50 "#906" 0.3)
    (1.00 "#906" 0.1))
  (rect canvas.center 200 100 :stroke :black :fill '(:url :three_stops_8_9)))
-->
```kaavio
<!-- expand: BIBLE-8.9-SAMPLE -->
```
```lisp
<!-- expand: BIBLE-8.9-SAMPLE -->
```

${BLANK_PARAGRAPH}

__◆ 図 8.10__

<!-- snippet: BIBLE-8.10-SAMPLE
(diagram (380 240)
  (grid)
  (defgradient (:linear :three_stops_8_10)
    (0.00 "#ffcc00")
    (0.33 "#cc6699")
    (1.00 "#66cc99"))
  (defgradient (:linear :right_to_left_8_10 :href :three_stops_8_10
                        :x1 "100%" :y1 "0%" :x2 "0%" :y2 "0%"))
  (defgradient (:linear :down_8_10          :href :three_stops_8_10
                        :x1 "0%" :y1 "0%" :x2 "0%" :y2 "100%"))
  (defgradient (:linear :up_8_10            :href :three_stops_8_10
                        :x1 "0%" :y1 "100%" :x2 "0%" :y2 "0%"))
  (defgradient (:linear :diagonal_8_10      :href :three_stops_8_10
                        :x1 "0%" :y1 "0%" :x2 "100%" :y2 "100%"))
  (rect '(140  40) 200 40  :stroke :black :fill '(:url :three_stops_8_10))
  (rect '(140  90) 200 40  :stroke :black :fill '(:url :right_to_left_8_10))
  (rect '(270 120)  40 200 :stroke :black :fill '(:url :down_8_10))
  (rect '(320 120)  40 200 :stroke :black :fill '(:url :up_8_10))
  (rect '(140 170) 200 100 :stroke :black :fill '(:url :diagonal_8_10)))
-->
```kaavio
<!-- expand: BIBLE-8.10-SAMPLE -->
```
```lisp
<!-- expand: BIBLE-8.10-SAMPLE -->
```

${BLANK_PARAGRAPH}

__◆ 図 8.11__

<!-- snippet: BIBLE-8.11-SAMPLE
(diagram (360 140)
  (grid)
  (defgradient (:linear :partial_8_11 :x1 "20%" :y1 "30%" :x2 "40%" :y2 "80%")
    (0.00 "#ffcc00")
    (0.33 "#cc6699")
    (1.00 "#66cc99"))
  (defgradient (:linear :padded_8_11    :href :partial_8_11 :spread :pad))
  (defgradient (:linear :repeated_8_11  :href :partial_8_11 :spread :repeat))
  (defgradient (:linear :reflected_8_11 :href :partial_8_11 :spread :reflect))
  (defgroup (100 100 :line_8_11)
      (line '((20 30) (40 80)) :stroke :black))
  (rect '( 70  70) 100 100 :stroke :black :fill '(:url :padded_8_11)
                           :contents ((use :line_8_11 canvas.center)))
  (rect '(180  70) 100 100 :stroke :black :fill '(:url :repeated_8_11)
                           :contents ((use :line_8_11 canvas.center)))
  (rect '(290  70) 100 100 :stroke :black :fill '(:url :reflected_8_11)
                           :contents ((use :line_8_11 canvas.center))))
-->
```kaavio
<!-- expand: BIBLE-8.11-SAMPLE -->
```
```lisp
<!-- expand: BIBLE-8.11-SAMPLE -->
```

${BLANK_PARAGRAPH}

__◆ 図 8.12__

<!-- snippet: BIBLE-8.12-SAMPLE
(diagram (140 140)
  (grid)
  (defgradient (:radial :three_stops_8_12)
    (0.00 "#f96")
    (0.50 "#9c9")
    (1.00 "#906"))
  (rect '(70 70) 100 100 :stroke :black :fill '(:url :three_stops_8_12)))
-->
```kaavio
<!-- expand: BIBLE-8.12-SAMPLE -->
```
```lisp
<!-- expand: BIBLE-8.12-SAMPLE -->
```

${BLANK_PARAGRAPH}

__◆ 図 8.13__

<!-- snippet: BIBLE-8.13-SAMPLE
(diagram (140 140)
  (grid)
  (defgradient (:radial :center_origin_8_13 :cx "0%" :cy "0%" :radius "141%")
    (0.00 "#f96")
    (0.50 "#9c9")
    (1.00 "#906"))
  (rect '(70 70) 100 100 :stroke :black :fill '(:url :center_origin_8_13)))
-->
```kaavio
<!-- expand: BIBLE-8.13-SAMPLE -->
```
```lisp
<!-- expand: BIBLE-8.13-SAMPLE -->
```

${BLANK_PARAGRAPH}

__◆ 図 8.14__

<!-- snippet: BIBLE-8.14-SAMPLE
(diagram (140 140)
  (grid)
  (defgradient (:radial :focal_set_8_14 :cx "0%" :cy "0%"
                        :fx "50%" :fy "50%" :radius "100%")
    (0.00 "#f96")
    (0.50 "#9c9")
    (1.00 "#906"))
  (rect '(70 70) 100 100 :stroke :black :fill '(:url :focal_set_8_14)))
-->
```kaavio
<!-- expand: BIBLE-8.14-SAMPLE -->
```
```lisp
<!-- expand: BIBLE-8.14-SAMPLE -->
```

${BLANK_PARAGRAPH}

__◆ 図 8.15__

<!-- snippet: BIBLE-8.15-SAMPLE
(diagram (360 140)
  (grid)
  (defgradient (:radial :three_stops_8_15 :cx "0%" :cy "0%" :radius "70%")
    (0.00 "#f96")
    (0.50 "#9c9")
    (1.00 "#906"))
  (defgradient (:radial :padded_8_15    :href :three_stops_8_15 :spread :pad))
  (defgradient (:radial :repeated_8_15  :href :three_stops_8_15 :spread :repeat))
  (defgradient (:radial :reflected_8_15 :href :three_stops_8_15 :spread :reflect))
  (rect '( 70  70) 100 100 :stroke :black :fill '(:url :padded_8_15))
  (rect '(180  70) 100 100 :stroke :black :fill '(:url :repeated_8_15))
  (rect '(290  70) 100 100 :stroke :black :fill '(:url :reflected_8_15)))
-->
```kaavio
<!-- expand: BIBLE-8.15-SAMPLE -->
```
```lisp
<!-- expand: BIBLE-8.15-SAMPLE -->
```

${BLANK_PARAGRAPH}

<!-- collapse:end -->

${BLANK_PARAGRAPH}

### テーマ
<!-- autolink: [$$](#テーマ) -->

　ストロークやフィルなど図形要素のスタイルを都度指定するのは面倒です。with-options の
ようなマクロを使うこともできますが、図形要素毎の with- 系マクロを多段に書くのもやはり
面倒です。各種図形要素に対するスタイル指定をひとまとめした設定があれば便利でしょう。

　「テーマ」はそのためのものです。以下のコードは図形要素毎の個別のスタイル設定をせずに 
[$@ 章](#一般的な図形)の図形要素を描画していますが、

<!-- snippet: THEME-SAMPLE-1
(diagram (500 300)
  (grid)
  (with-theme (:default)
    (let ((cc canvas.center))
      (document     (xy+ cc -190 -100) 70 50 "doc")
      (folder       (xy+ cc  -65 -100) "folder" :width 80 :height 50)
      (person       (xy+ cc   65 -100) 30 :label "person")
      (balloon      (xy+ cc  190 -100) "balloon"
                    (xy+ cc  160 -140) :width 80 :height 40)
      (memo         (xy+ cc -190    0) "memo" :width 80 :height 50)
      (cube         (xy+ cc  -65    0)  70 60 "cube")
      (cylinder     (xy+ cc   65    0)  70 60 "cylinder")
      (explosion1   (xy+ cc  190    0) 110 90 "explosion")
      (star         (xy+ cc -190  100) 5 70 70 "star")
      (cross        (xy+ cc  -65  100)  70 70 20)
      (block-arrow1 (xy+ cc  105  100) (xy+ cc 25 100) 20)
      (pipe         (xy+ cc  190  100) :h 80 :label "pipe"))))
-->

```lisp
<!-- expand: THEME-SAMPLE-1 -->
```

${BLANK_PARAGRAPH}

描画結果は以下のようなものになります。これは 3 行目の with-theme で default テーマを
指定したことによる効果です。

<!-- figure:  デフォルトテーマの使用例 -->
```kaavio
<!-- expand: THEME-SAMPLE-1 -->
```
<!-- figure:end -->

${BLANK_PARAGRAPH}


　この 3 行目の `(with-theme (:default) ...)` というコードは、以下と等価です。テーマを
利用することで、これらをすべて記述する手間を省くことができるわけです。なお、[$@ 章](#基本的な図形)の
図形要素は、どちらかというと複雑な図形を組み立てるための部品のため、default テーマによる
設定はありません。

```lisp
(with-options (:font '(:family "sans-serif"))
 (with-textbox-options (:stroke :black :fill :white)
  (with-document-options (:stroke :darkslategray :fill :whitesmoke)
   (with-folder-options (:stroke :darkkhaki :fill :cornsilk)
    (with-person-options (:stroke :maroon :fill :linen)
     (with-balloon-options (:stroke :navy :fill :azure)
      (with-memo-options (:stroke :darkgreen :fill :mintcream
                          :fill2 :palegreen3 :crease 30 :align :left :valign :top)
       (with-cube-options (:stroke :black :fill :lightgray :fill2 :darkgray)
        (with-cylinder-options (:stroke :black :fill :white)
         (with-explosion-options (:stroke :red :fill :pink)
          (with-cross-options (:stroke :black :fill :white)
           (with-block-arrow-options (:stroke :navy :fill :skyblue)
            (locally ... )))))))))))))
```

　テーマはそのまま使用することもできますし、[部分的にカスタマイズ](#テーマのカスタマイズ)する
こともできます。また、[イチから新しく作る](#新しいテーマの作成)ことも可能です。これらの方法に
ついては後述します。

　version 0.035 より、with-themes マクロが追加されました。これにより、複数のテーマをまとめて
適用することができます。以下は、

```lisp
(with-themes (:default :other :other-else)
  ...)
```

以下と等価です。

```lisp
(with-theme (:default)
  (with-theme (:other)
    (with-theme (:other-else)
      ...)))
```

#### 利用できるテーマ

　現在、以下のテーマが利用できます。

* default : [$$](#一般的な図形)の図形要素を設定しています。サンプルは [$@](F#デフォルトテーマの使用例) を \
参照してください。


#### 新しいテーマの作成

　register-theme を使えば、新しいテーマを作成することができます。以下は、default テーマを
定義している register-theme の使用例です。

<!-- figure:  register-theme によるテーマの作成 -->
```lisp
(register-theme (:default)
  (t           :font '(:family "sans-serif"))
  (textbox     :stroke :black         :fill :white)
  (document    :stroke :darkslategray :fill :whitesmoke)
  (folder      :stroke :darkkhaki     :fill :cornsilk)
  (person      :stroke :maroon        :fill :linen)
  (balloon     :stroke :navy          :fill :azure)
  (memo        :stroke :darkgreen     :fill :mintcream
               :fill2  :palegreen3    :crease 30 :align :left :valign :top)
  (cube        :stroke :black         :fill :lightgray :fill2 :darkgray)
  (cylinder    :stroke :black         :fill :white)
  (explosion   :stroke :red           :fill :pink)
  (cross       :stroke :black         :fill :white)
  (block-arrow :stroke :navy          :fill :skyblue))
```
<!-- figure:end -->

${BLANK_PARAGRAPH}


　register-theme に続けて、括弧内にテーマ名をキーワードで指定します。続けて、各図形要素に
指定するスタイルを指定します。このとき、

* それぞれの指定は `(textbox :stroke :black :fill :white)` のようにリストで指定します
    * その先頭要素は、 with-xxx-options マクロの xxx 部分を指定してください
    * 後続要素は、with-xxx-options のパラメータ部分をそのまま記述してください
* with-options に相当する指定は、 `(t  :font '(:family "sans-serif"))` のように `t` で始まるリストで指定してください

#### テーマのカスタマイズ

　register-theme において、「ベースとする既存のテーマ」を指定することで、差分だけを指定した
カスタムテーマを作成することができます。以下の例では、default テーマをベースとして my-theme と
いうテーマを作成しています。

<!-- figure:  register-theme でベーステーマを指定する例 -->
```lisp
(register-theme (:my-theme :default)
  (cylinder :stroke :maroon :fill :beige)
  (cross :stroke :purple :fill :lavender))
```
<!-- figure:end -->

${BLANK_PARAGRAPH}


　上記の my-theme を使用して [$@](F#デフォルトテーマの使用例) と同じ図面を描画した結果を
以下に示します。カスタマイズした内容が反映されていることがわかります。

<!-- figure:  テーマのカスタマイズ例 -->
```kaavio
(register-theme (:my-theme :default)
  (cylinder :stroke :maroon :fill :beige)
  (cross :stroke :purple :fill :lavender))

(diagram (500 300)
  (grid)
  (with-theme (:my-theme)
    (let ((cc canvas.center))
      (document     (xy+ cc -190 -100) 70 50 "doc")
      (folder       (xy+ cc  -65 -100) "folder" :width 80 :height 50)
      (person       (xy+ cc   65 -100) 30 :label "person")
      (balloon      (xy+ cc  190 -100) "balloon" (xy+ cc 160 -140) :width 80 :height 40)
      (memo         (xy+ cc -190    0) "memo" :width 80 :height 50)
      (cube         (xy+ cc  -65    0)  70 60 "cube")
      (cylinder     (xy+ cc   65    0)  70 60 "cylinder")
      (explosion1   (xy+ cc  190    0) 110 90 "explosion")
      (star         (xy+ cc -190  100) 5 70 70 "star")
      (cross        (xy+ cc  -65  100)  70 70 20)
      (block-arrow1 (xy+ cc  105  100) (xy+ cc 25 100) 20)
      (pipe         (xy+ cc  190  100) :h 80 :label "pipe"))))
```
<!-- figure:end -->

${BLANK_PARAGRAPH}

