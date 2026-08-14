
## 基本的な図形

　SVG 規格における基本図形（とそれに類するもの）から紹介します。以下のサンプルはそれぞれの説明項目への
リンクになっています。

<!-- define: HASH_RECT          = '[](#四角形)' -->
<!-- define: HASH_CIRCLE        = '[](#正円)' -->
<!-- define: HASH_ELLIPSE       = '[](#楕円)' -->
<!-- define: HASH_REGULAR_POLYGON = '[](#正多角形)' -->
<!-- define: HASH_POLYGON       = '[](#多角形)' -->
<!-- define: HASH_LINE          = '[](#直線)' -->
<!-- define: HASH_ARC           = '[](#円弧)' -->
<!-- define: HASH_TEXT          = '[](#テキスト)' -->
<!-- define: HASH_DIAMOND       = '[](#ひし形)' -->
<!-- define: HASH_PARALLELOGRAM = '[](#平行四辺形)' -->
<!-- define: HASH_2D_CURVE      = '[](#二次ベジェ曲線)' -->
<!-- define: HASH_3D_CURVE      = '[](#三次ベジェ曲線)' -->

```kaavio
(diagram (800 230)
  (glow-shadow :id :foo-filter)
  ;(grid)
  (let ((w  80)
        (h 100)
        (bgclr :white)) ;;(make-fill :color :lightgray :opacity 0.4 )));;
    (defgroup (w h :rect-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (rect `(,(/ w 2) ,(/ w 2)) 50 50 :fill :skyblue :stroke :blue)
      (text `(,(/ w 2) ,(- h 5)) "四角形" :align :center))
    (defgroup (w h :circle-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (circle `(,(/ w 2) ,(/ w 2)) 25 :fill :bisque :stroke :brown)
      (text `(,(/ w 2) ,(- h 5)) "円" :align :center))
    (defgroup (w h :ellipse-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (ellipse `(,(/ w 2) ,(/ w 2)) 30 20 :fill :beige :stroke :olive)
      (text `(,(/ w 2) ,(- h 5)) "楕円" :align :center))
    (defgroup (w h :regular-polygon-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (regular-polygon `(,(/ w 2) ,(/ w 2)) 5 25 :fill :gray :stroke :black)
      (text `(,(/ w 2) ,(- h 5)) "正多角形" :align :center))
    (defgroup (w h :polygon-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (polygon '((40.00 10.00) (32.75 31.50) (10.25 31.50)
                 (28.25 45.00) (21.75 66.50) (40.00 53.75)
                 (58.25 66.50) (51.75 45.00) (69.75 31.50)
                 (47.25 31.50)) :stroke :red :fill :lightpink)
      (text `(,(/ w 2) ,(- h 5)) "多角形" :align :center))
    (defgroup (w h :line-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (line '((20 20) (60 20) (20 65) (60 65)) :stroke :black)
      (text `(,(/ w 2) ,(- h 5)) "線" :align :center))
    (defgroup (w h :arc-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (arc `(,(/ w 2) ,(+ 5 (/ w 2))) 25 25 0 120 60 :stroke '(:color :navy :width 8))
      (text `(,(/ w 2) ,(- h 5)) "円弧" :align :center))
    (defgroup (w h :text-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (text `(,(/ w 2) 55) "Text" :align :center
            :font '(:family "Times New Roman" :size 30 :style :italic :filter :foo-filter))
      (text `(,(/ w 2) ,(- h 5)) "テキスト" :align :center))
    (defgroup (w h :diamond-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (diamond `(,(/ w 2) ,(/ w 2)) 60 60 :fill :plum3 :stroke '(:color :red3 :width 2))
      (text `(,(/ w 2) ,(- h 5)) "ひし形" :align :center))
    (defgroup (w h :parallelogram-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (parallelogram `(,(/ w 2) ,(/ w 2)) 70 40 :h 20 :fill :lightsteelblue :stroke '(:color :darkslateblue :width 2))
      (text `(,(/ w 2) ,(- h 5)) "平行四辺形" :align :center))
    (defgroup (w h :2d-curve-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (2d-curve '((10 60) (60 -20) (70 60)) :stroke '(:color :navy :width 3))
      (text `(,(/ w 2) ,(- h 20)) "二次" :align :center)
      (text `(,(/ w 2) ,(- h 5)) "ベジェ曲線" :align :center))
    (defgroup (w h :3d-curve-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (3d-curve '((10 60) (50 -40) (30 120) (70 10)) :stroke '(:color :brown :width 3))
      (text `(,(/ w 2) ,(- h 20)) "三次" :align :center)
      (text `(,(/ w 2) ,(- h 5)) "ベジェ曲線" :align :center))
    (use :rect-grp          '(100  60) :link "${HASH_RECT}")
    (use :circle-grp        '(200  60) :link "${HASH_CIRCLE}")
    (use :ellipse-grp       '(300  60) :link "${HASH_ELLIPSE}")
    (use :regular-polygon-grp '(400  60) :link "${HASH_REGULAR_POLYGON}")
    (use :polygon-grp       '(500  60) :link "${HASH_POLYGON}")
    (use :line-grp          '(600  60) :link "${HASH_LINE}")
    (use :arc-grp           '(700  60) :link "${HASH_ARC}")
    (use :text-grp          '(100 170) :link "${HASH_TEXT}")
    (use :diamond-grp       '(200 170) :link "${HASH_DIAMOND}")
    (use :parallelogram-grp '(300 170) :link "${HASH_PARALLELOGRAM}")
    (use :2d-curve-grp      '(400 170) :link "${HASH_2D_CURVE}")
    (use :3d-curve-grp      '(500 170) :link "${HASH_3D_CURVE}")))
```

### 四角形
<!-- autolink: [$$](#四角形) -->

<!-- snippet: RECTANGLE-SAMPLE
(diagram (300 100)
  (grid)
  (rect '(150 50) 150 60 :rx 10 :stroke :navy :fill :skyblue))
-->

　rect マクロによって四角形を描画できます。角を丸くすることもできます。

<!-- figure:  rect のサンプル -->
```kaavio
<!-- expand: RECTANGLE-SAMPLE -->
```
<!-- figure:end -->


　上記サンプルのソースは以下の通りです。パラメータの詳細については rect マクロを参照して
ください。

```lisp
<!-- expand: RECTANGLE-SAMPLE -->
```

${BLANK_PARAGRAPH}

### 正円
<!-- autolink: [$$](#正円) -->

<!-- snippet: CIRCLE-SAMPLE
(diagram (300 100)
  (grid)
  (circle '(150 50) 30 :stroke :brown :fill :bisque))
-->

　circle マクロによって正円を描画できます。楕円を描画したい場合は ellipse マクロを使用
してください。

<!-- figure:  circle のサンプル -->
```kaavio
<!-- expand: CIRCLE-SAMPLE -->
```
<!-- figure:end -->

　上記サンプルのソースは以下の通りです。パラメータの詳細については circle マクロを参照
してください。

```lisp
<!-- expand: CIRCLE-SAMPLE -->
```

${BLANK_PARAGRAPH}

### 楕円
<!-- autolink: [$$](#楕円) -->

<!-- snippet: ELLIPSE-SAMPLE
(diagram (300 100)
  (grid)
  (ellipse '(150 50) 60 30 :stroke :olive :fill :beige))
-->

　ellipse マクロによって楕円を描画できます。正円を描画したい場合は circle マクロを使用
してください。

<!-- figure:  ellipse のサンプル -->
```kaavio
<!-- expand: ELLIPSE-SAMPLE -->
```
<!-- figure:end -->

　上記サンプルのソースは以下の通りです。パラメータの詳細については ellipse マクロを参照
してください。

```lisp
<!-- expand: ELLIPSE-SAMPLE -->
```

${BLANK_PARAGRAPH}

### 正多角形
<!-- autolink: [$$](#正多角形) -->

<!-- snippet: REGULAR-POLYGON-SAMPLE
(diagram (400 100)
  (grid)
  (regular-polygon (x+ canvas.cc -130) 5 40 :fill :gray :stroke :black)
  (regular-polygon (x+ canvas.cc    0) 6 40 :fill :gray :stroke :black)
  (regular-polygon (x+ canvas.cc  130) 8 40 :fill :gray :stroke :black))
-->

　regular-polygon マクロによって正多角形、すなわち五角形や六角形などを描画できます。

<!-- figure:  regular-polygon のサンプル -->
```kaavio
<!-- expand: REGULAR-POLYGON-SAMPLE -->
```
<!-- figure:end -->

　上記サンプルのソースは以下の通りです。パラメータの詳細については regular-polygon マクロを
参照してください。

```lisp
<!-- expand: REGULAR-POLYGON-SAMPLE -->
```

${BLANK_PARAGRAPH}

### 多角形
<!-- autolink: [$$](#多角形) -->

<!-- snippet: POLYGON-SAMPLE
(diagram (300 100)
  (grid)
  (polygon '((150.00 10.00) (139.85 40.10) (108.35 40.10)
             (133.55 59.00) (124.45 89.10) (150.00 71.25)
             (175.55 89.10) (166.45 59.00) (191.65 40.10)
             (160.15 40.10)) :stroke :red :fill :lightpink))
-->

　polygon マクロによって多角形、すなわち複数の直線からなる形状を描画できます。正多角形
を描画する場合には regular-polygon マクロを使用した方が良いでしょう。

<!-- figure:  polygon のサンプル -->
```kaavio
<!-- expand: POLYGON-SAMPLE -->
```
<!-- figure:end -->

　上記サンプルのソースは以下の通りです。パラメータの詳細については polygon マクロを参照
してください。

```lisp
<!-- expand: POLYGON-SAMPLE -->
```

${BLANK_PARAGRAPH}

### 直線
<!-- autolink: [$$](#直線) -->

<!-- snippet: LINE-SAMPLE
(diagram (300 100)
  (grid)
  (line '((100 50) (125 50)
          (130 30) (140 70)
          (150 30) (160 70)
          (170 30) (180 70)
          (185 50) (210 50)) :stroke :red))
-->

　line マクロによって直線（または複数の直線からなる折線）を描画できます。図形要素
どうしを接続したい場合はコネクタを使用した方が良いでしょう。

<!-- figure:  line のサンプル -->
```kaavio
<!-- expand: LINE-SAMPLE -->
```
<!-- figure:end -->

　上記サンプルのソースは以下の通りです。パラメータの詳細については line マクロを参照
してください。

```lisp
<!-- expand: LINE-SAMPLE -->
```

${BLANK_PARAGRAPH}

### 円弧
<!-- autolink: [$$](#円弧) -->

<!-- snippet: ARC-SAMPLE
(diagram (300 100)
  (grid)
  (arc '(150 55) 25 25 0 30 300 :stroke '(:color :navy :width 8)))
-->

　arc マクロによって円弧を描画できます。パスにおける `:arc-to` の機能を単独の図形要素に
したものです。端点に終端マークをつけることもできます。

<!-- figure:  arc のサンプル -->
```kaavio
<!-- expand: ARC-SAMPLE -->
```
<!-- figure:end -->


　上記サンプルのソースは以下の通りです。

```lisp
<!-- expand: ARC-SAMPLE -->
```

　パラメータの詳細については arc マクロを参照してください。ここでは、必須パラメー
タに限定して簡単に説明します。 `(arc center rx ry x-axis-rotation degree1 degree2)` と
した場合、 `center` を中心とした x 半径 `rx` 、y 半径 `ry` の楕円を `x-axis-rotation` だけ
回転させたものの上で、角度 `degree1` から（時計回りに） `degree2` までの部分弧を描きます。

　以下に例を示します。 `(100 50)` を中心とした `rx=40, ry=30` の楕円を 45 度回転させたものを
ベースとします。これはライトグレーの太い楕円で描画されています。このうち、0 度から 90 度までの部分を
円弧として（赤い線で）描画しています。つまり、これは `(arc '(100 50) 40 30 45 0 90)` による描画
となります。

<!-- figure:  arc のサンプル - 2 -->
```kaavio
(diagram (200 100)
   (grid)
   (let ((rx 40)
         (ry 30)
         (rotate 45)
         (st1 (make-stroke :color :lightgray :width 8 :opacity 0.4))
         (st2 (make-stroke :color :red :width 2)))
     (line    '(( 50 0) (150 100)) :stroke :lightgray)
     (line    '((150 0) ( 50 100)) :stroke :lightgray)
     (circle  '(100 50) 2 :fill :red :stroke :none)
     (ellipse '(100 50) rx ry :stroke st1 :fill :none :rotate rotate)
     (arc     '(100 50) rx ry rotate 0 90 :stroke st2)))
```
<!-- figure:end -->


　正円をベースとした円弧を描画したい場合、 `rx` と `ry` を同じ値に指定します。この場合、回転
させることに意味はないので、 `x-axis-rotation` は 0 にしてください。

　marcro arc を使用した円弧の描画は、「中心と角度」が明らかな場合に使用します。そうではなく、
円弧の開始点と終了点が明らかな場合は、path マクロを使用した方が良いでしょう。



${BLANK_PARAGRAPH}

　補助線を表示する場合のサンプルを以下に示します。 `debug` パラメータにキーワードで色名を与えると、
円弧のベースとなる楕円の中心と、円弧の始点・終点を結ぶ直線を明示します。 `:debug t` とすれば
デフォルトの赤色が使用されます。調整の際に使用すると便利です。

<!-- snippet: ARC-DEBUG-SAMPLE
(diagram (400 200)
  (grid)
  (arc canvas.center 150 60 20 0 90 :end2 :arrow
       :stroke '(:color :navy :width 2) :debug :red))
-->

<!-- figure:  arc における補助線のサンプル -->
```kaavio
<!-- expand: ARC-DEBUG-SAMPLE -->
```
<!-- figure:end -->


<!-- collapse:begin -->
　※上記サンプルのソースはこちら。

```lisp
<!-- expand: ARC-DEBUG-SAMPLE -->
```
<!-- collapse:end -->

${BLANK_PARAGRAPH}

　最後に、arc で終端マークを使用する場合の注意点について説明します。終端マークはその端点の向き
にあわせて描画されますが、終端マークそのものの形状は考慮されません。その結果として、arc が描く
曲線の端点付近のカーブが急な場合、望ましくない結果になる場合があります。以下の例では、横長の
楕円ベースで 0° から 90° の円弧を描いて `:triangle` 指定の終端マークを付加していますが、始点
側の終端マークは矢印に見えない状態になってしまっています。現状では、これは注意が必要ではあるもの
の仕様として扱われます。arc で終端マークを使用する場合は注意してください。

<!-- figure:  arc における終端マークの例 -->
```kaavio
(diagram (200 100)
  (grid)
  (ellipse canvas.center 80 30 :stroke '(:color :lightgray :width 8))
  (let ((em (make-endmark :type :triangle :size :small)))
    (arc canvas.center 80 30 0 0 90 :stroke :red :end1 em :end2 em)))
```
<!-- figure:end -->

${BLANK_PARAGRAPH}


### テキスト
<!-- autolink: [$$](#テキスト) -->

<!-- snippet: TEXT-SAMPLE
(diagram (300 100)
  (grid)
  (text '(150 70) "Text" :align :center
        :font '(:family "Times New Roman"
                :size 48 :weight :bold :fill :green :style :italic)))
-->

　text マクロによってテキストを描画できます。

<!-- figure:  text のサンプル -->
```kaavio
<!-- expand: TEXT-SAMPLE -->
```
<!-- figure:end -->

　上記サンプルのソースは以下の通りです。パラメータの詳細については text マクロを参照
してください。

```lisp
<!-- expand: TEXT-SAMPLE -->
```

${BLANK_PARAGRAPH}

　position と align の関係を以下に示します。以下において、赤い点が position で、
align 指定はテキストで示されています。

<!-- figure:  テキストの position とアライメント指定の関係 -->
```kaavio
(diagram (300 100)
  (grid)
  (labels ((impl (y text align)
             (circle `(150 ,y) 3 :stroke :none :fill :red)
             (text   `(150 ,y) text :align align)))
    (impl 30 "align :left"   :left)
    (impl 60 "align :center" :center)
    (impl 90 "align :right " :right)))
```
<!-- figure:end -->

${BLANK_PARAGRAPH}

### ひし形
<!-- autolink: [$$](#ひし形) -->

<!-- snippet: DIAMOND-SAMPLE
(diagram (300 100)
  (grid)
  (diamond '(150 50) 150 60 :stroke '(:color :red3 :width 2) :fill :plum3))
-->

　diamond マクロによってひし形を描画できます。

<!-- figure:  diamond のサンプル -->
```kaavio
<!-- expand: DIAMOND-SAMPLE -->
```
<!-- figure:end -->

　上記サンプルのソースは以下の通りです。パラメータの詳細については diamond マクロを参照
してください。

```lisp
<!-- expand: DIAMOND-SAMPLE -->
```

${BLANK_PARAGRAPH}

### 平行四辺形
<!-- autolink: [$$](#平行四辺形) -->

<!-- snippet: PARALLELOGRAM-SAMPLE
(diagram (200 100)
  (grid)
  (parallelogram canvas.center 150 60 :h 40
                 :fill :lightsteelblue
                 :stroke '(:color :darkslateblue :width 2)))
-->

　parallelogram マクロによって平行四辺形を描画できます。

<!-- figure:  parallelogram のサンプル -->
```kaavio
<!-- expand: PARALLELOGRAM-SAMPLE -->
```
<!-- figure:end -->

　上記サンプルのソースは以下の通りです。パラメータの詳細については parallelogram マクロを
参照してください。

```lisp
<!-- expand: PARALLELOGRAM-SAMPLE -->
```

${BLANK_PARAGRAPH}

　`direction` および `offset` パラメータについて説明します。 `direction` が `:H` の場合、
上側の角を右または左方向に押して変形させたような形になります。その長さを指定するのが `offset` 
です。 `offset` が０以上の場合は左上の角を右方向に、負数の場合は右上の角を左方向に押すイメージ
です。 `direction` が `:V` の場合は、同じ要領で左側の角を上または下に押すイメージになります。
以下のように。

<!-- figure:  parallelogram における direction と offset -->
```kaavio
(diagram (400 280 :fill :white)
  ;(grid)
  (with-options (:stroke '(:color :red :dasharray (2 2)) :fill :none)
    (rect '(100  70) 160  80 :id :r1)
    (rect '(300  70) 160  80 :id :r2)
    (rect '(100 200) 160 120 :id :r3)
    (rect '(300 200) 160 120 :id :r4))
  (with-options (:stroke :navy :fill :lightcyan :font 10)
    (dolist (e '((:r1 :h  40  40  0   0   0)
                 (:r2 :h -40 120  0 160   0)
                 (:r3 :v  40   0 40   0   0)
                 (:r4 :v -40   0 80   0 120)))
      (destructuring-bind (id dir offset x y x2 y2) e
        (with-subcanvas-of (id)
          (parallelogram canvas.center canvas.width canvas.height dir offset
              :contents
              ((block-arrow1 `(,x2 ,y2) `(,x ,y) 5 :size 15 :stroke :none :fill :red)
               (paragraph canvas.center
                          (format nil "direction = :~A~%~A" dir
                                  (if (< offset 0) "offset < 0" "0 <= offset"))
                          :align :center :valign :center))))))))
```
<!-- figure:end -->



　ただし、 `width / height` で指定されたサイズはあくまで赤い点線の矩形であることに注意して
ください。また、サブキャンバスやコネクタによる接続点は現状では赤い点線の矩形ベースで計算され
ますが、この挙動は将来変更される可能性があります。

${BLANK_PARAGRAPH}

### 二次ベジェ曲線
<!-- autolink: [$$](#二次ベジェ曲線) -->

<!-- snippet: 2D-CURVE-SAMPLE
(diagram (300 100)
  (grid)
  (let ((pt1 '( 60 90))
        (ptC '(  0 20))
        (pt2 '(280 20)))
    (2d-curve `(,pt1 ,ptC ,pt2)
              :end1 :arrow :end2 :arrow
              :debug nil :stroke '(:color :slateblue :width 4))))
-->

　2d-curve マクロによって二次ベジェ曲線を描画できます。パスにおける `:2d-curve-to` の機能を
単独の図形要素にしたものです。端点に終端マークをつけることもできます。

<!-- figure:  2d-curve のサンプル -->
```kaavio
<!-- expand: 2D-CURVE-SAMPLE -->
```
<!-- figure:end -->


　上記サンプルのソースは以下の通りです。パラメータの詳細については 2d-curve マクロを参照して
ください。

```lisp
<!-- expand: 2D-CURVE-SAMPLE -->
```

${BLANK_PARAGRAPH}

　補助線を表示する場合のサンプルを以下に示します。 `debug` パラメータにキーワードで色名を与えると、
二次ベジェ曲線を構成する点とそれらを結ぶ直線を明示します。 `:debug t` とすればデフォルトの赤色が
使用されます。通常は目に見えない制御点も可視化されるので、調整の際に使用すると便利です。

<!-- snippet: 2D-CURVE-DEBUG-SAMPLE
(diagram (500 250)
  (grid)
  (let ((pt1 '( 50 150))
        (ptC '(100  30))
        (pt2 '(150 130))
        (pt3 '(250 180))
        (pt4 '(450 130)))
    (2d-curve `(,pt1 ,ptC ,pt2 ,pt3 ,pt4)
              :end1 :rect :end2 :triangle
              :debug t :stroke '(:color :darkslategray :width 3))))
-->

<!-- figure:  2d-curve における補助線のサンプル -->
```kaavio
<!-- expand: 2D-CURVE-DEBUG-SAMPLE -->
```
<!-- figure:end -->


<!-- collapse:begin -->
　※上記サンプルのソースはこちら。

```lisp
<!-- expand: 2D-CURVE-DEBUG-SAMPLE -->
```
<!-- collapse:end -->

${BLANK_PARAGRAPH}

### 三次ベジェ曲線
<!-- autolink: [$$](#三次ベジェ曲線) -->

<!-- snippet: 3D-CURVE-SAMPLE
(diagram (300 100)
  (grid)
  (let ((p1 '( 20  90))
        (c1 '(150 -50))
        (c2 '(150 150))
        (p2 '(280  10)))
    (3d-curve `(,p1 ,c1 ,c2 ,p2)
              :end1 :arrow :end2 :arrow
              :debug nil :stroke '(:color :slateblue :width 4))))
-->

　3d-curve マクロによって三次ベジェ曲線を描画できます。パスにおける `:3d-curve-to` の機能を
単独の図形要素にしたものです。端点に終端マークをつけることもできます。

<!-- figure:  3d-curve のサンプル -->
```kaavio
<!-- expand: 3D-CURVE-SAMPLE -->
```
<!-- figure:end -->


　上記サンプルのソースは以下の通りです。パラメータの詳細については 3d-curve マクロを参照して
ください。

```lisp
<!-- expand: 3D-CURVE-SAMPLE -->
```

${BLANK_PARAGRAPH}

　補助線を表示する場合のサンプルを以下に示します。 `debug` パラメータにキーワードで色名を与えると、
三次ベジェ曲線を構成する点とそれらを結ぶ直線を明示します。 `:debug t` とすればデフォルトの赤色が
使用されます。通常は目に見えない制御点も可視化されるので、調整の際に使用すると便利です。

<!-- snippet: 3D-CURVE-DEBUG-SAMPLE
(diagram (400 220)
    (grid)
    (let ((pt1  '( 30 130))
          (ptC1 '( 80  10))
          (ptC2 '(130  70))
          (pt2  '(130 110))
          (ptC3 '(130 210))
          (pt3  '(230 110))
          (ptC4 '(180 130))
          (pt4  '(380 130)))
      (3d-curve `(,pt1 ,ptC1 ,ptC2 ,pt2 ,ptC3 ,pt3 ,ptC4 ,pt4)
                :end1 :rect :end2 :triangle
                :debug t :stroke '(:color :slateblue :width 4))))
-->

<!-- figure:  3d-curve における補助線の サンプル -->
```kaavio
<!-- expand: 3D-CURVE-DEBUG-SAMPLE -->
```
<!-- figure:end -->


<!-- collapse:begin -->
　※上記サンプルのソースはこちら。

```lisp
<!-- expand: 3D-CURVE-DEBUG-SAMPLE -->
```
<!-- collapse:end -->

${BLANK_PARAGRAPH}

