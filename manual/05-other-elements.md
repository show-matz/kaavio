
## その他の図形要素

　ほしい図形要素がなければ、手間はかかりますがパスを使って実現できるかもしれません。図面の中に
写真などのラスタ画像を含めることもできます。あるいは、kaavio がサポートしていない SVG の
機能を利用したければ生の SVG コードを挿入することもできます{{fn:生の SVG コードを挿入したとして、 \
そしてそれが SVG 規格に完璧に適合していたとしても、お使いの（ブラウザなどの）SVG ビューワーで \
「正しく」表示できるかどうかは別問題だということを常に念頭に置いて作業してください。kaavio の \
開発では「メジャーなブラウザでサポートされていない SVG の機能を避ける」ことに大きな注意と努力が \
払われています。}}。

### パス
<!-- autolink: [$$](#パス) -->

<!-- snippet: PATH-SAMPLE
(diagram (300 100)
  (grid)
  (path '((:move-to (120 50))
          (:arc-to 30 30 0 1 1 (150 80))
          (:line-to (150 50)) :close-path) :stroke :black :fill :rosybrown))
-->

　path マクロによって直線や曲線からなる複雑な図形を描画できます。polygon マクロは直線からなる
多角形でしたが、パスでは複雑な曲線を含む図形を描画できます。ただ、その分構文も複雑です。

<!-- figure:  パスのサンプル -->
```kaavio
<!-- expand: PATH-SAMPLE -->
```
<!-- figure:end -->

　上記サンプルのソースは以下の通りです。パラメータの詳細については path マクロを参照して
ください。

```lisp
<!-- expand: PATH-SAMPLE -->
```

${BLANK_PARAGRAPH}

　ここでは、 `(path data...)` とした場合の `data` パラメータについて説明します。これは以下の
ような **ディレクティブ** キーワードを要素とするリストで指定します。点や値の指定を伴うディレ
クティブは、それ自体をリストにする必要があります。

<!-- stack:push tr style="font-size: 14;" -->

<!-- table: path の data で使用できるディレクティブ -->
| ディレクティブ  | 説明                                                                  |
|:===============|:----------------------------------------------------------------------|
| `:move-to`     | 指定した点に（線を描くことなく）移動します。                             |
| `:line-to`     | 現在の点から指定した点に向かって（線を描きながら）移動します。            |
| `:h-line-to`   | 現在の点から指定した x 座標に向かって水平線を描きながら移動します。       |
| `:v-line-to`   | 現在の点から指定した y 座標に向かって垂直線を描きながら移動します。       |
| `:arc-to`      | 現在の点から指定した点に向かって楕円弧を描きながら移動します。            |
| `:2d-curve-to` | 現在の点から指定した点に向かって二次ベジェ曲線を描きながら移動します。    |
| `:3d-curve-to` | 現在の点から指定した点に向かって三次ベジェ曲線を描きながら移動します。    |
| `:absolute`    | 後続のディレクティブを、現在のキャンバスに対する絶対座標として処理します。 |
| `:relative`    | 後続のディレクティブを、現在の点に対する相対座標として処理します。         |
| `:close-path`  | パスを閉じます。すなわち、現在の点から先頭の点までを結ぶ直線を引きます。   |
<!-- table:end -->
<!-- stack:pop tr -->

${BLANK_PARAGRAPH}

　多くの場合、開始点に移動してから直線や曲線を描いて、最後には開始点に戻ることで図形を描くことに
なります。たとえば、正方形を描く場合の data パラメータは以下のようなリストになるでしょう。

```lisp
  '((:move-to ( 50  50))
    (:line-to (100  50))
    (:line-to (100 100))
    (:line-to ( 50 100)) :close-path)
```

　以下に、各ディレクティブの詳細について説明します。

#### :move-to ディレクティブ
<!-- autolink: [:move-to](#:move-to ディレクティブ) -->

　`(:move-to pt)` という記述により、線を描くことなく指定した点 pt に移動します。また、 
`(:move-to pt1 pt2 pt3 ...)` のように複数の点を記述することができ、これは `(:move-to pt1)  \
(:line-to pt2 pt3 ...)` と等価になります。以下に例を示します。

<!-- figure:  :move-to ディレクティブのサンプル -->
```kaavio
(diagram (400 100)
  (grid)
  (path '((:move-to (200 10) (230 50) (170 50) (200 10)))
          :stroke :black :fill :rosybrown)
  (with-options (:font "Courier New")
    (textbox '(170 80) "(:move-to (200 10) (230 50)
          (170 50) (200 10))" :no-frame t :align :left )))
```
<!-- figure:end -->

　なお、:move-to で指定する点は :absolute および :relative の影響を受けます。

#### :line-to ディレクティブ
<!-- autolink: [:line-to](#:line-to ディレクティブ) -->

　`(:line-to pt)` という記述により、現在の点から直線を描きながら指定した点 pt に移動します。
`(:line-to pt1 pt2 pt3 ...)` のように複数の点を記述することができます。これは現在の点から
順番に直線を描きながら移動します。

<!-- figure:  :line-to ディレクティブのサンプル -->
```kaavio
(diagram (400 100)
  (grid)
  (path '((:move-to (200 10))
          (:line-to (230 50) (170 50) (200 10)))
          :stroke :black :fill :rosybrown)
  (with-options (:font "Courier New")
    (textbox '(170 80) "(:move-to (200 10))
(:line-to (230 50) (170 50) (200 10))" :no-frame t :align :left )))
```
<!-- figure:end -->

　なお、:line-to で指定する点は :absolute および :relative の影響を受けます。

#### :h-line-to ディレクティブ
<!-- autolink: [:h-line-to](#:h-line-to ディレクティブ) -->

　`(:h-line-to x)` という記述により、現在の点から指定した x 座標まで水平線を描きながら移動します。

<!-- figure:  :h-line-to ディレクティブのサンプル -->
```kaavio
(diagram (300 150)
  (grid)
  (path '((:move-to (100 10))
          (:h-line-to 200)
          (:v-line-to  50)
          (:h-line-to 100)
          (:v-line-to  10))
          :stroke :black :fill :rosybrown)
  (textbox '(150 100) "(:move-to (100 10))
(:h-line-to 200)
(:v-line-to  50)
(:h-line-to 100)
(:v-line-to  10)" :width 200 :font "Courier New" :no-frame t :align :left ))
```
<!-- figure:end -->

　なお、:h-line-to で指定する点は :absolute および :relative の影響を受けます。

#### :v-line-to ディレクティブ
<!-- autolink: [:v-line-to](#:v-line-to ディレクティブ) -->

　`(:v-line-to y)` という記述により、現在の点から指定した y 座標まで垂直線を描きながら移動します。

<!-- figure:  :v-line-to ディレクティブのサンプル -->
```kaavio
(diagram (300 150)
  (grid)
  (path '((:move-to (100 10))
          (:h-line-to 200)
          (:v-line-to  50)
          (:h-line-to 100)
          (:v-line-to  10))
          :stroke :black :fill :rosybrown)
  (textbox '(150 100) "(:move-to (100 10))
(:h-line-to 200)
(:v-line-to  50)
(:h-line-to 100)
(:v-line-to  10)" :width 200 :font "Courier New" :no-frame t :align :left ))
```
<!-- figure:end -->

　なお、 :v-line-to で指定する点は :absolute および :relative の影響を受けます。

#### :arc-to ディレクティブ
<!-- autolink: [:arc-to](#:arc-to ディレクティブ) -->

　:arc-to ディレクティブにより、現在の点から楕円弧を描きながら指定した点 pt に移動します。

<!-- figure:  :arc-to ディレクティブのサンプル -->
```kaavio
(diagram (300 130)
  (grid)
  (let ((p1 '(100 50))
        (p2 '(200 80)))
    (path `((:move-to ,p1)
            (:arc-to 30 30 0 0 1 ,p2))
          :stroke '(:color :black :width 2) :fill :none)
    (with-options (:stroke :none :fill :red)
      (circle p1 3)
      (circle p2 3))
    (with-options (:font "Courier New")
      (textbox '(120 110) "(:move-to (100 50))
(:arc-to 30 30 0 0 1 (200 80))" :no-frame t :align :left))))
```
<!-- figure:end -->

　:arc-to は円弧と考え方はほぼ同じですが、指定はやや複雑です。まず、パラメータ構成は以下の
通りです。

```lisp
(:arc-to rx ry x-axis-rotation large-arc-flag sweep-flag pt)
```

　`rx, ry, x-axis-rotation` は円弧での指定と同じもので、ベースとなる楕円の x 半径、y 半径、
および x 軸に対する楕円の回転角です。:arc-to では、この 3 つの値で決まる楕円の一部を使って、
「現在の点から点 `pt` に至る曲線」を描画します。

　以下のように、そのような楕円を使って 2 点を通る円弧は 4 種類描けることがわかります。

<!-- figure:  :arc-to における rx, ry, x-axis-rotation -->
```kaavio
(diagram (400 140)
  (grid)
  (let ((pt1 '(150 50))
        (pt2 '(250 90)))
    (with-options (:fill :none :stroke '(:color :lightgray :width 2))
      (path `((:move-to ,pt1) (:arc-to 80 40 10 1 1 ,pt2)))
      (path `((:move-to ,pt1) (:arc-to 80 40 10 0 1 ,pt2)))
      (path `((:move-to ,pt1) (:arc-to 80 40 10 1 0 ,pt2)))
      (path `((:move-to ,pt1) (:arc-to 80 40 10 0 0 ,pt2))))
    (with-options (:fill :red :stroke :none)
      (circle pt1 3)
      (circle pt2 3))
    (with-options (:font '(:fill :red))
      (text (xy+ pt1 -10 -5) "pt1" :align :right)
      (text (xy+ pt2  10 15) "pt2" :align :left))))
```
<!-- figure:end -->

　この 4 種類の円弧のうち、どれを使うのかを指定するのが `large-arc-flag` と `sweep-flag` です。
いずれも 0 または 1 を指定するもので、 `large-arc-flag` は「1 ならば大きい方の円弧、0 ならば
小さい方の円弧」を使うという意味で、 `sweep-flag` は「1 ならば時計回り、0 ならば反時計回り」
です。このイメージを以下に示します。

<!-- figure:  :arc-to における large-arc-flag, sweep-flag -->
```kaavio
(diagram (510 220)
  (grid)
  (defgroup (240 100 :back)
    ;(rect canvas.center canvas.width canvas.height :fill :none :stroke :lightgray)
    (with-options (:fill :none :stroke '(:color :lightgray :width 2))
      (ellipse '(110 30) 50 20)
      (ellipse '( 60 50) 50 20))
    (with-options (:fill :red :stroke :none)
      (circle '( 60 30) 3)
      (circle '(110 50) 3)))
  (with-options (:fill :none
                 :font '(:size 10)
                 :stroke '(:color :red :width 3 :opacity 0.4))
    (use :back '(130 60)
         :contents
         ((path '((:move-to (60 30)) (:arc-to 50 20 0 0 1 (110 50))))
          (text '(115 65) "large-arc-flag : 0" :align :left)
          (text '(115 80) "sweep-flag : 1"     :align :left)))
    (use :back '(380 60)
         :contents
         ((path '((:move-to (60 30)) (:arc-to 50 20 0 0 0 (110 50))))
          (text '(115 65) "large-arc-flag : 0" :align :left)
          (text '(115 80) "sweep-flag : 0"     :align :left)))
    (use :back '(130 160)
         :contents
         ((path '((:move-to (60 30)) (:arc-to 50 20 0 1 1 (110 50))))
          (text '(115 65) "large-arc-flag : 1" :align :left)
          (text '(115 80) "sweep-flag : 1"     :align :left)))
    (use :back '(380 160)
         :contents
         ((path '((:move-to (60 30)) (:arc-to 50 20 0 1 0 (110 50))))
          (text '(115 65) "large-arc-flag : 1" :align :left)
          (text '(115 80) "sweep-flag : 0"     :align :left)))))
```
<!-- figure:end -->


　パスにおける :arc-to は、始点と終点が決まっている状態で指定された形状の楕円を位置あわせするような
やり方なのでわかりにくいかもしれません。円弧のベースとなる楕円の中心点や始点・終点の角度が明らかな場合
は arc マクロの使用を検討してください。

#### :2d-curve-to ディレクティブ
<!-- autolink: [:2d-curve-to](#:2d-curve-to ディレクティブ) -->

　:2d-curve-to ディレクティブにより、現在の点から二次ベジェ曲線を描きながら指定した点 pt に移動します。

　:2d-curve-to の基本形は `(:2d-curve-to ptC pt2)` です。現在の点を `pt1` とした時に、 `ptC` を
制御点として `pt2` に至る二次ベジェ曲線を描きます。

<!-- snippet: 2D-CURVE-TO-SAMPLE-1
(diagram (300 100)
  (grid)
  (let ((pt1 '( 50 80))
        (pt2 '(250 90))
        (ptC '(230 10)))
    (with-options (:stroke '(:color :slateblue :width 2 :dasharray (3 3)))
      (line `(,pt1 ,ptC))
      (line `(,pt2 ,ptC)))
    (path `((:move-to ,pt1)
            (:2d-curve-to ,ptC ,pt2)) :stroke '(:color :black :width 2))
    (with-options (:fill :red :stroke :none)
      (circle pt1 4)
      (circle pt2 4)
      (circle ptC 4 :fill :slateblue))
    (text (xy+ pt1 -5 -5) "pt1" :align :right)
    (text (xy+ pt2  5 -5) "pt2" :align :left)
    (text (xy+ ptC 10  5) "ptC" :align :left)))
-->

<!-- figure:  :2d-curve-to ディレクティブのサンプル -->
```kaavio
<!-- expand: 2D-CURVE-TO-SAMPLE-1 -->
```
<!-- figure:end -->

<!-- collapse:begin -->
　※上記サンプルのソースはこちら。

```lisp
<!-- expand: 2D-CURVE-TO-SAMPLE-1 -->
```
<!-- collapse:end -->

　:2d-curve-to では、任意の数の点を追加することができます。つまり、 `(:2d-curve-to ptC pt2 pt3 pt4...)` 
という要領で、必要な制御点は自動的に計算されます。以下に例を示します。

<!-- snippet: 2D-CURVE-TO-SAMPLE-2
(diagram (400 200)
  (grid)
  (layer :controls)
  (let ((pt1  '( 30 120))
        (pt2  '(100 120))
        (pt3  '(200 100))
        (pt4  '(350 140))
        (ptC1 '( 90  50))
        (ptC2 '(110 190))
        (ptC3 '(290  10)))
    (with-options (:stroke '(:color :black :width 2))
      (path `((:move-to ,pt1) (:2d-curve-to ,ptC1 ,pt2 ,pt3 ,pt4))))
    (with-options (:layer :controls)
      (line `(,pt1 ,ptC1 ,pt2 ,ptC2 ,pt3 ,ptC3 ,pt4)
            :stroke '(:color :slateblue :width 2 :dasharray (3 3)))
      (with-options (:fill :red :stroke :none)
        (mapcar (lambda (p) (circle p 4)) `(,pt1 ,pt2 ,pt3 ,pt4))
        (mapcar (lambda (p) (circle p 4 :fill :slateblue)) `(,ptC1 ,ptC2 ,ptC3)))
      (text (xy+ pt1  -3 -5) "pt1"   :align :right)
      (text (xy+ pt2   5 -5) "pt2"   :align :left)
      (text (xy+ pt3   5 15) "pt3"   :align :left)
      (text (xy+ pt4   5  5) "pt4"   :align :left)
      (text (xy+ ptC1  5 -5) "ptC"   :align :left)
      (text (xy+ ptC2 10  5) "ptC'"  :align :left)
      (text (xy+ ptC3 10  7) "ptC''" :align :left))))
-->

<!-- figure:  :2d-curve-to ディレクティブのサンプル - 2 -->
```kaavio
<!-- expand: 2D-CURVE-TO-SAMPLE-2 -->
```
<!-- figure:end -->


<!-- collapse:begin -->
　※上記サンプルのソースはこちら。

```lisp
<!-- expand: 2D-CURVE-TO-SAMPLE-2 -->
```
<!-- collapse:end -->

　上の例では追加で２つの点（pt3, pt4）を指定しており、そのために必要な制御点 ptC' と ptC'' は自動的に
計算されています。具体的には、「直前の制御点を、直前の点に対して鏡映した点」が次の制御点になります。上の
例であれば ptC' は ptC を pt2 に対して鏡映した点です。

#### :3d-curve-to ディレクティブ
<!-- autolink: [:3d-curve-to](#:3d-curve-to ディレクティブ) -->

　:3d-curve-to ディレクティブにより、現在の点から三次ベジェ曲線を描きながら指定した点 pt に移動します。

　:3d-curve-to の基本形は `(:3d-curve-to ptC1 ptC2 pt2)` です。現在の点を `pt1` とした時に、 `ptC1` 
および `ptC2` を制御点として `pt2` に至る三次ベジェ曲線を描きます。

<!-- snippet: 3D-CURVE-TO-SAMPLE-1
(diagram (300 160)
  (grid)
  (let ((pt1  '( 50  80))
        (pt2  '(250  80))
        (ptC1 '(100  10))
        (ptC2 '(200 150)))
    (with-options (:stroke '(:color :slateblue :width 2 :dasharray (3 3)))
      (line `(,pt1 ,ptC1))
      (line `(,pt2 ,ptC2)))
    (path `((:move-to ,pt1)
            (:3d-curve-to ,ptC1 ,ptC2 ,pt2)) :stroke '(:color :black :width 2))
    (with-options (:fill :red :stroke :none)
      (circle pt1 4)
      (circle pt2 4)
      (circle ptC1 4 :fill :slateblue)
      (circle ptC2 4 :fill :slateblue))
    (text (xy+ pt1  -5 -5) "pt1" :align :right)
    (text (xy+ pt2   5 -5) "pt2" :align :left)
    (text (xy+ ptC1 10  5) "ptC1" :align :left)
    (text (xy+ ptC2 10  5) "ptC2" :align :left)))
-->

<!-- figure:  :3d-curve-to ディレクティブのサンプル -->
```kaavio
<!-- expand: 3D-CURVE-TO-SAMPLE-1 -->
```
<!-- figure:end -->

<!-- collapse:begin -->
　※上記サンプルのソースはこちら。

```lisp
<!-- expand: 3D-CURVE-TO-SAMPLE-1 -->
```
<!-- collapse:end -->

　:2d-curve-to と比較すると、:3d-curve-to は複雑な曲線を描くことができます。SVG本のサンプルと
同じものを以下に示します。

<!-- snippet: 3D-CURVE-TO-SAMPLE-2
(diagram (450 200)
  (grid)
  (labels ((defgrp-impl (id p1 p2 c1 c2)
             (defgroup (150 100 id)
               (path `((:move-to ,p1)
                       (:3d-curve-to ,c1 ,c2 ,p2))
                     :stroke '(:color :black :width 2))
               (with-options (:stroke :none :fill :red)
                 (circle p1 3)
                 (circle p2 3)
                 (circle c1 3 :fill :slateblue)
                 (circle c2 3 :fill :slateblue))
               (line `(,c1 ,p1) :stroke :slateblue)
               (line `(,c2 ,p2) :stroke :slateblue))))
    (defgrp-impl :sample1 '( 40 70) '(110 70) '( 10  30) '(140 30))
    (defgrp-impl :sample2 '( 40 70) '(110 70) '( 60  30) '( 90 30))
    (defgrp-impl :sample3 '( 40 70) '(110 70) '(110  30) '( 40 30))
    (defgrp-impl :sample4 '( 40 50) '(110 50) '( 10  10) '(140 90))
    (defgrp-impl :sample5 '( 40 50) '(110 50) '( 60  10) '( 90 90))
    (defgrp-impl :sample6 '( 40 50) '(110 50) '(110  10) '( 40 90))
    (use :sample1 '( 75  50))
    (use :sample2 '(225  50))
    (use :sample3 '(375  50))
    (use :sample4 '( 75 150))
    (use :sample5 '(225 150))
    (use :sample6 '(375 150))))
-->

<!-- figure:  :3d-curve-to ディレクティブのサンプル -2 -->
```kaavio
<!-- expand: 3D-CURVE-TO-SAMPLE-2 -->
```
<!-- figure:end -->

<!-- collapse:begin -->
　※上記サンプルのソースはこちら。

```lisp
<!-- expand: 3D-CURVE-TO-SAMPLE-2 -->
```
<!-- collapse:end -->



　:3d-curve-to では、任意の数の制御点と移動先の組みを追加することができます。つまり、 
`(:3d-curve-to ptC1 ptC2 pt2 ptC3 pt3 ptC4 pt4...)` という要領で、必要な制御点は自動的に計算され
ます。以下に例を示します。

<!-- snippet: 3D-CURVE-TO-SAMPLE-3
(diagram (250 150)
  (grid)
  (layer :controls #|:none|#)
  (let ((pt1   '( 10  90))
        (pt2   '(100  90))
        (pt3   '(230  70))
        (ptC1  '( 40  20))
        (ptC2  '( 80  40))
        (ptC2d '(120 140))
        (ptC3  '(200  20)))
    (path `((:move-to ,pt1)
            (:3d-curve-to ,ptC1 ,ptC2 ,pt2 ,ptC3 ,pt3))
          :stroke '(:color :black :width 2))
    (with-options (:layer :controls)
      (with-options (:stroke '(:color :slateblue :width 2 :dasharray (3 3)))
        (line `(,pt1  ,ptC1))
        (line `(,ptC2 ,ptC2d))
        (line `(,pt3  ,ptC3)))
      (with-options (:fill :red :stroke :none)
        (mapcar (lambda (p) (circle p 4)) `(,pt1 ,pt2 ,pt3))
        (mapcar (lambda (p) (circle p 4 :fill :slateblue))
                `(,ptC1 ,ptC2 ,ptC2d ,ptC3)))
      (with-options (:font 10)
        (text (xy+ pt1   -5 15) "pt1"   :align :left)
        (text (xy+ pt2    5 -5) "pt2"   :align :left)
        (text (xy+ pt3   -5 15) "pt3"   :align :left)
        (text (xy+ ptC1   5 -5) "ptC1"  :align :left)
        (text (xy+ ptC2  10  5) "ptC2"  :align :left)
        (text (xy+ ptC2d 10  5) "ptC2'" :align :left)
        (text (xy+ ptC3  10  7) "ptC3"  :align :left)))))
-->

<!-- figure:  :3d-curve-to ディレクティブのサンプル - 3 -->
```kaavio
<!-- expand: 3D-CURVE-TO-SAMPLE-3 -->
```
<!-- figure:end -->


<!-- collapse:begin -->
　※上記サンプルのソースはこちら。

```lisp
<!-- expand: 3D-CURVE-TO-SAMPLE-3 -->
```
<!-- collapse:end -->

　上の例では追加で制御点 ptC3 と点 pt3 を指定しており、そのために必要な制御点 pt2C' は自動的に
計算されています。具体的には、「直前の制御点を、直前の点に対して鏡映した点」が次の制御点になります。上の
例であれば ptC2' は ptC2 を pt2 に対して鏡映した点です。


#### :absolute ディレクティブ
<!-- autolink: [:absolute](#:absolute ディレクティブ) -->

　後続のディレクティブを、現在のキャンバスに対する絶対座標として処理します。以下の例では、
三角形の描画を :absolute 指定によって描画しています。同じ図形を :relative で描画する 
[$@](F#:relative ディレクティブのサンプル) と比較してみてください。

<!-- figure:  :absolute ディレクティブのサンプル -->
```kaavio
(diagram (300 150)
  (grid)
  (path '((:move-to (120 10))
          :absolute
          (:h-line-to 180)
          (:line-to (150 50))
          :close-path)
          :stroke :black :fill :rosybrown)
  (textbox '(150 100) "(:move-to (120 10))
:absolute
(:h-line-to 180)
(:line-to (150 50))
:close-path" :width 200 :font "Courier New" :no-frame t :align :left ))
```
<!-- figure:end -->


#### :relative ディレクティブ
<!-- autolink: [:relative](#:relative ディレクティブ) -->

　後続のディレクティブを、現在の点に対する相対座標として処理します。以下の例では、三角形の描画を 
:relative 指定によって描画しています。同じ図形を :absolute で描画する [$@](F#:absolute ディレクティブのサンプル) 
と比較してみてください。

<!-- figure:  :relative ディレクティブのサンプル -->
```kaavio
(diagram (300 150)
  (grid)
  (path '((:move-to (120 10))
          :relative
          (:h-line-to 60)
          (:line-to (-30 40))
          :close-path)
          :stroke :black :fill :rosybrown)
  (textbox '(150 100) "(:move-to (120 10))
:relative
(:h-line-to 60)
(:line-to (-30 40))
:close-path" :width 200 :font "Courier New" :no-frame t :align :left ))
```
<!-- figure:end -->

#### :close-path ディレクティブ
<!-- autolink: [:close-path](#:close-path ディレクティブ) -->

　現在の点から現在のサブパス（:move-to によって開始された一連の描画）の開始点まで直線を描き
ながら移動します。以下の例では、１回のパスで 2 つの三角形を描画していますが、それぞれの三角形の
最後の直線を :close-path で描画しています。

<!-- figure:  :close-path ディレクティブのサンプル -->
```kaavio
(diagram (300 150)
  (grid)
  (path '((:move-to ( 60 10))
          (:line-to (120 10) ( 90 50))
          :close-path
          (:move-to (210 10))
          (:line-to (180 50) (240 50))
          :close-path)
          :stroke :black :fill :rosybrown)
  (textbox '(150 100)
"(:move-to ( 60 10))
(:line-to (120 10) ( 90 50))
:close-path
(:move-to (210 10))
(:line-to (180 50) (240 50))
:close-path"
 :width 280 :font "Courier New" :no-frame t :align :left ))
```
<!-- figure:end -->

${BLANK_PARAGRAPH}

### 画像ファイルの埋め込み
<!-- autolink: [$$](#画像ファイルの埋め込み) -->

　image マクロを使うことで、SVG 図面の中に他の画像を埋め込むことができます。以下の形式の画像
ファイルをサポートします。

* PNG ( *.png )
* JPEG ( *.jpg, *.jpeg )
* GIF ( *.gif )
* BMP ( *.bmp )

　典型的には、image マクロの使用は以下のような記述になるでしょう。必須パラメータは、画像を
配置する中心位置と画像ファイル名だけです。画像のサイズは kaavio が自分で調べるため、
基本的には指定する必要がありません{{fn:このことは、kaavio が動作する時点で指定された画像ファイル \
が「そこにある」必要がある、ということを意味します。 `width, height` の両方が明示的に指定された場合に \
限り、kaavio は実際の画像サイズを調べません。}}。

```lisp
(image '(400 400) "./foo.png")
```

　image マクロのパラメータ詳細は [$@ 節](#macro image)を参照してください。オプションのパラメータ 
`width` または `height` のどちらかを指定した場合、画像ファイルの縦横比（アスペクト比）を維持
した状態で指定サイズで表示します。 `width` と `height` の両方を指定した場合、縦横比の維持は
保証されず、指定されたサイズで表示します。

${BLANK_PARAGRAPH}

### 生の SVG コード片の挿入
<!-- autolink: [$$](#生の SVG コード片の挿入) -->

<!-- snippet: RAW-SVG-SAMPLE
(diagram (200 100)
  (grid)
  (raw-svg "<rect x='50' y='25' width='100' height='50' fill='linen' stroke='brown' stroke-width='1' />"))
-->

　raw-svg マクロによって、図面の中に任意の SVG コード片を挿入することができます。サンプル
としては雑ですが、以下のようにすれば、

```lisp
<!-- expand: RAW-SVG-SAMPLE -->
```

　文字列として raw-svg に渡したコード片がそのまま挿入されます。パラメータの詳細については 
raw-svg マクロを参照してください。

<!-- figure:  raw-svg のサンプル -->
```kaavio
<!-- expand: RAW-SVG-SAMPLE -->
```
<!-- figure:end -->

${BLANK_PARAGRAPH}

