<!-- define: APPNAME = diagram -->
<!-- define: BLANK_PARAGRAPH = '　　' -->
<!-- define: TODO = '@((background:red;color:white;)(ToDo : %1))' -->

<!-- title:${APPNAME} readme -->    
<!-- style:./default.css -->			
<!-- config:term-link-in-header -->			

<!-- filter:diagram  = bash ./diagram.sh  %in %out -->
<!-- filter:plantuml = bash ./plantuml.sh %in %out -->

<!-- config:write-comment -->			
<!-- config:header-numbering 2 4 -->			

# README - ${APPNAME}

　この文書は、 **${APPNAME}** のためのマニュアル文書です。


## Table of contents

<!-- embed:toc 2 4 -->

${BLANK_PARAGRAPH}

## ${APPNAME} とは

　${APPNAME} は、テキストベースの作図ツールです。テキスト形式で作成したデータファイルを入力として、
SVG 形式{{fn:SVG は Scalable Vector Graphics の略です。}}の画像ファイルを生成します。

　入力データの記述には Common LISP 言語を使用します。これは、${APPNAME} 自身が Common LISP で
書かれているからですが、このツールを使ってみたいからといって知りもしない言語をマスターしたいとは思わない
でしょう。${{TODO}{まだ記述されていません。}}


## 簡単なサンプル

　簡単なサンプルから始めましょう。以下のような入力を ${APPNAME} に与えると、

<!-- snippet: FIRST-SAMPLE
(diagram (:w 300 :h 150)
  (grid)
  (rectangle '( 50  50) 80 60 :fill :powderblue :id :x)
  (circle    '(250 100) 40    :fill :moccasin   :id :y)
  (connector :x :y :end2 :arrow))
-->

```lisp
<!-- expand: FIRST-SAMPLE -->
```

${BLANK_PARAGRAPH}


　以下のような画像が生成されます。

```diagram
<!-- expand: FIRST-SAMPLE -->
```
Figure. 簡単なサンプル


　「入力を ${APPNAME} に与える」というのは、具体的には以下のように入力ファイルと出力ファイルの名前を
パラメータとして ${APPNAME} を起動することを意味します。${{TODO}{まだ記述されていません。}}

```sh
diagram ./input.digram ./output.svg
```


　先程の入力データをもう一度みてみましょう。

```lisp
<!-- expand: FIRST-SAMPLE -->
```

* diagram で、幅 300、高さ 150 の画像を作成します
* grid で、背景にグリッド線を描画しています
* rectangle で、左上から (50, 50) の位置に幅 80、高さ 60 の矩形を作成し、これに x という ID をつけます
* circle で、左上から (250, 100) の位置に半径 40 の円を作成し、これに y という ID をつけます
* connector で、x から y に向かって接続線を引き、終端の形状を矢印にしています


${BLANK_PARAGRAPH}

　次のサンプルはもう少し複雑です。

<!-- snippet: SECOND-SAMPLE
(diagram (:w 300 :h 200)
  (grid)
  (with-fill (:color :honeydew)
    (rectangle '( 50  50) 50 50               :id :a1)
    (circle    (point/x+ a1.center 100) 25    :id :a2)
    (rectangle (point/y+ a2.center 100) 50 50 :id :a3)
    (circle    (point/x+ a3.center 100) 25    :id :a4))
  (connector :a1 :a2 :end2 :arrow)
  (connector :a2 :a3 :end2 :arrow)
  (connector :a3 :a4 :end2 :arrow)
  (connector :a3 :a1 :end2 :arrow :style :LB))
-->


```lisp
<!-- expand: SECOND-SAMPLE -->
```

```diagram
<!-- expand: SECOND-SAMPLE -->
```
Figure. 簡単なサンプル-2


* diagram と grid は先程と同じなので省略。
* with-fill で、ここから先は「デフォルトの塗りつぶしを `honeydew` にしています
* drectangle で、左上から (50, 50) の位置に一辺 50 の正方形を作成し、これに a1 という ID をつけます
* a1 の中央（center）から x軸（水平）方向に 100 の位置に、circle で 半径 40 の円を作成し、これに a2 と \
いう ID をつけます
* connector で、x から y に向かって接続線を引き、終端の形状を矢印にしています


## 基本的な図形

### 四角形

<!-- snippet: RECTANGLE-SAMPLE
(diagram (:w 300 :h 100)
  (grid)
  (rectangle '(150 50) 150 60 :rx 10 :ry 10 :fill :skyblue :stroke :blue))
-->

```diagram
<!-- expand: RECTANGLE-SAMPLE -->
```
Figure. rectangle のサンプル


```lisp
<!-- expand: RECTANGLE-SAMPLE -->
```



### 円

　${{TODO}{まだ記述されていません。}}

### 楕円

　${{TODO}{まだ記述されていません。}}

### 線

　${{TODO}{まだ記述されていません。}}

### コネクタ

　${{TODO}{まだ記述されていません。}}

### テキスト

　${{TODO}{まだ記述されていません。}}

### テキストボックス

　テキストボックスは、[$$](#四角形) と [$$](#テキスト) を組み合わせたようなものです。

<!-- snippet: TEXTBOX-SAMPLE
(diagram (:w 200 :h 100)
  (grid)
  (textbox '(100 50) "test text.~%multi line."
                     :rx 10 :ry 10 :stroke :black :fill :white))
-->

```diagram
<!-- expand: TEXTBOX-SAMPLE -->
```
Figure. テキストボックスのサンプル

```lisp
<!-- expand: TEXTBOX-SAMPLE -->
```

　${{TODO}{まだ記述されていません。}}



## 色の指定

　${APPNAME} は SVG 形式で図形を生成するため、色の指定は SVG の規格に準拠します。

* #rrggbb 表記による、6 桁の16進指定。rr、gg、bb は順に赤、緑、青の成分で、 00〜ff の範囲で \
指定します。
* #rgb 表記による、3 桁の16進指定。r、g、b は順に赤、緑、青の成分で、 0〜f の範囲で指定します。 \
これは #rrggbb の簡略表記で、#136 は #113366 に相当します。
* [$$](#rgb関数)による指定。これは `(rgb r g b)` の要領で使用します。r、g、b は順に赤、緑、青の \
成分で、それぞれ 0〜255 の整数または 0.0〜1.0 の小数点数で指定します。0.0〜1.0 の指定の場合、 \
それに 255 をかけた値が指定されます。
* 色名での指定。使用できる色の名前とサンプルは [$@ 節](#色の名前) を参照してください。

${BLANK_PARAGRAPH}


## 線・塗りつぶし・文字

　ここでは、図形の線や塗りつぶし、およびフォントの指定方法を紹介します。

### ストローク

　${{TODO}{まだ記述されていません。}}

```lisp
(make-stroke :blue)    ;; equal to (make-stroke :color :blue)

(make-stroke 10)       ;; equal to (make-stroke :width 10)

(make-stroke :color :red :width 3)

(make-stroke '(:color :red :width 3))

```


### フィル

　${{TODO}{まだ記述されていません。}}

### フォント

　${{TODO}{まだ記述されていません。}}


${BLANK_PARAGRAPH}

## レイヤー

　${{TODO}{まだ記述されていません。}}


${BLANK_PARAGRAPH}

## 座標と位置

　${{TODO}{まだ記述されていません。}}

<!-- snippet: GEOMETRY-SAMPLE
(diagram (:w 300 :h 200)
  (grid)
  (text '(10 20) "(0, 0)")
  (rectanglecircle (point/xy+ canvas.topleft 2 2)     2 :stroke :red :fill :red)
  (circle canvas.topright    2 :stroke :red :fill :red)
  (circle canvas.bottomleft  2 :stroke :red :fill :red)
  (circle canvas.bottomright 2 :stroke :red :fill :red))
-->

```lisp
<!-- expand: GEOMETRY-SAMPLE -->
```


```diagram
<!-- expand: GEOMETRY-SAMPLE -->
```
Figure. xxxのサンプル


${BLANK_PARAGRAPH}

## サブキャンバス

　${{TODO}{まだ記述されていません。}}


<!-- snippet: SUBCANVAS-SAMPLE
(diagram (:w 300 :h 200)
  (grid)
  (circle (point/xy+ canvas.topleft 50 50) 20 :stroke :brown :fill :wheat)
  (with-subcanvas ('(150 50) 100 100)
    (rectangle canvas.center
               canvas.width canvas.height :stroke :gray :fill :lightgray)
    (circle (point/xy+ canvas.topleft 50 50) 20 :stroke :brown :fill :wheat)))
-->

```lisp
<!-- expand: SUBCANVAS-SAMPLE -->
```


```diagram
<!-- expand: SUBCANVAS-SAMPLE -->
```
Figure. サブキャンバスのサンプル


${BLANK_PARAGRAPH}

## リファレンス

　${{TODO}{まだ記述されていません。}}

### 関数とマクロ

#### *default-fill*変数

　${{TODO}{まだ記述されていません。}}

#### *default-stroke*変数

　${{TODO}{まだ記述されていません。}}

#### diagram マクロ

　${{TODO}{まだ記述されていません。}}

#### make-fill 関数

　${{TODO}{まだ記述されていません。}}

```lisp
(defun make-fill &key :color :opacity :rule)
```

Table. make-fill 関数のパラメータ
| parameter   | default 値 | description          |
|:============|:===========|:---------------------|
| :color      | `:none`    | 塗りつぶしの色を指定します。[$@ 節](#色の指定)参照。 |
| :opacity    | 1.0        | 0 から 1 までで透明度を指定します。 |
| :rule       | `:nonzero` | 塗りつぶしのルールを `:nonzero` または `:evenodd` で指定します。|


${BLANK_PARAGRAPH}

<!-- snippet: FILL-OPACITY-SAMPLE
(diagram (:w 400 :h 100)
  (text '(200 55) "this is test text." :align :center)
  (with-fill (:color :red)
    (rectangle '(150 50) 30 30 :fill (make-fill :opacity 0.2))
    (text      '(150 80) "0.2" :align :center)
    (rectangle '(200 50) 30 30 :fill (make-fill :opacity 0.5))
    (text      '(200 80) "0.5" :align :center)
    (rectangle '(250 50) 30 30 :fill (make-fill :opacity 0.8))
    (text      '(250 80) "0.8" :align :center)))
-->

```diagram
<!-- expand: FILL-OPACITY-SAMPLE -->
```
Figure. fill における opacity のサンプル




<!-- collapse:begin -->
[$@](F#fill における opacity のサンプル) のソースはこちら

```lisp
<!-- expand: FILL-OPACITY-SAMPLE -->
```
<!-- collapse:end -->




<!-- snippet: FILL-RULE-SAMPLE
(diagram (:w 400 :h 120)
  (with-subcanvas ('(75 0) 100 100)
    (polygon '((50 10) (20 90) (90 40) (10 40) (80 90))
             :fill (make-fill :color :skyblue :rule :nonzero))
    (text '(50 110) ":nonzero" :align :center))
  (with-subcanvas ('(225 0) 100 100)
    (polygon '((50 10) (20 90) (90 40) (10 40) (80 90))
             :fill (make-fill :color :skyblue :rule :evenodd))
    (text '(50 110) ":evenodd" :align :center)))
-->

```diagram
<!-- expand: FILL-RULE-SAMPLE -->
```
Figure. fill における rule のサンプル

<!-- collapse:begin -->
[$@](F#fill における rule のサンプル) のソースはこちら

```lisp
<!-- expand: FILL-RULE-SAMPLE -->
```
<!-- collapse:end -->


#### make-stroke 関数

```lisp
(defun make-stroke &key :color :width :opacity :linecap
                        :linejoin :miterlimit :dasharray :dashoffset)
```

Table. make-stroke 関数のパラメータ
| parameter   | default 値 | description          |
|:============|:===========|:---------------------|
| :color      | :black     | ストロークの色を指定します。[$@ 節](#色の指定)参照。 |
| :width      | 1          |                      |
| :opacity    | nil        |                      |
| :linecap    | nil        |                      |
| :linejoin   | nil        |                      |
| :miterlimit | nil        |                      |
| :dasharray  | nil        |                      |
| :dashoffset | nil        |                      |


${BLANK_PARAGRAPH}


```diagram
(diagram (:w 400 :h 100)
	(with-subcanvas ('(0 0) 100 100)
	  (with-stroke (:color :black :width 4 :dasharray '(8 4))
		(line '((30 20) (70 20)))
		(line '((30 40) (70 40)) :stroke '(:dashoffset 2))
		(line '((30 60) (70 60)) :stroke '(:dashoffset 4))
		(line '((30 80) (70 80)) :stroke '(:dashoffset 6))))
	(with-subcanvas ('(100 0) 100 100)
	  (with-stroke (:color :black :width 8)
		(line '((30 20) (70 20)))
		(line '((30 40) (70 40)) :stroke '(:linecap   :butt))
		(line '((30 60) (70 60)) :stroke '(:linecap  :round))
		(line '((30 80) (70 80)) :stroke '(:linecap :square))))
	(with-subcanvas ('(200 0) 100 100)
	  (with-stroke (:color :black :width 12)
		(line '(( 30 60) ( 45 45) ( 60 60)) :stroke '(:linejoin :miter))
		(line '(( 90 60) (105 45) (120 60)) :stroke '(:linejoin :round))
		(line '((150 60) (165 45) (180 60)) :stroke '(:linejoin :bevel)))))
```
Figure. dashoffset, linecap, linejoin のサンプル



#### rgb関数

```lisp
(rgb r g b)
```

#### with-fillマクロ

#### with-fontマクロ

#### with-strokeマクロ

#### with-subcanvasマクロ

### 色の名前

<!-- snippet: COLOR-NAME-SAMPLE
(let ((svg-width   750)
      (svg-height 1000))
  (diagram (:w svg-width :h svg-height)
    (let ((*default-fill*   (make-fill :color :white))
          (*default-font*   (make-font :family "monospace" :size 10 :width-spice 0.85))
          (*default-stroke* (make-stroke :color :black :width 1)))
      ;; back ground
      (rectangle (make-point (/ svg-width 2) (/ svg-height 2))
                 svg-width svg-height :fill :white :stroke :white)
      (let ((x 15)
            (y  0))
        (labels ((imp (lst)
                   (incf y 20)
                   (when (< svg-height (+ y 10))
                     (incf x 250)
                     (setf y  20))
                   (rectangle (list x y)  15  15 :fill (fourth lst))
                   (text (list (+ x 10)  (+ y 4))
                         (apply #'format  nil "#~2,'0x~2,'0x~2,'0x:~A" lst))))
          (mapcar #'imp '((  0   0   0 "black"               )
                          (  0   0 128 "navy"                )
                          (  0   0 139 "darkblue"            )
                          (  0   0 205 "mediumblue"          )
                          (  0   0 255 "blue"                )
                          (  0 100   0 "darkgreen"           )
                          (  0 128   0 "green"               )
                          (  0 128 128 "teal"                )
                          (  0 139 139 "darkcyan"            )
                          (  0 191 255 "deepskyblue"         )
                          (  0 206 209 "darkturquoise"       )
                          (  0 250 154 "mediumspringgreen"   )
                          (  0 255   0 "lime"                )
                          (  0 255 127 "springgreen"         )
                          (  0 255 255 "aqua"                )
                          (  0 255 255 "cyan"                )
                          ( 25  25 112 "midnightblue"        )
                          ( 30 144 255 "dodgerblue"          )
                          ( 32 178 170 "lightseagreen"       )
                          ( 34 139  34 "forestgreen"         )
                          ( 46 139  87 "seagreen"            )
                          ( 47  79  79 "darkslategray"       )
                          ( 47  79  79 "darkslategrey"       )
                          ( 50 205  50 "limegreen"           )
                          ( 60 179 113 "mediumseagreen"      )
                          ( 64 224 208 "turquoise"           )
                          ( 65 105 225 "royalblue"           )
                          ( 70 130 180 "steelblue"           )
                          ( 72  61 139 "darkslateblue"       )
                          ( 72 209 204 "mediumturquoise"     )
                          ( 75   0 130 "indigo"              )
                          ( 85 107  47 "darkolivegreen"      )
                          ( 95 158 160 "cadetblue"           )
                          (100 149 237 "cornflowerblue"      )
                          (102 205 170 "mediumaquamarine"    )
                          (105 105 105 "dimgray"             )
                          (105 105 105 "dimgrey"             )
                          (106  90 205 "slateblue"           )
                          (107 142  35 "olivedrab"           )
                          (112 128 144 "slategray"           )
                          (112 128 144 "slategrey"           )
                          (119 136 153 "lightslategray"      )
                          (119 136 153 "lightslategrey"      )
                          (123 104 238 "mediumslateblue"     )
                          (124 252   0 "lawngreen"           )
                          (127 255   0 "chartreuse"          )
                          (127 255 212 "aquamarine"          )
                          (128   0   0 "maroon"              )
                          (128   0 128 "purple"              )
                          (128 128   0 "olive"               )
                          (128 128 128 "gray"                )
                          (128 128 128 "grey"                )
                          (135 206 235 "skyblue"             )
                          (135 206 250 "lightskyblue"        )
                          (138  43 226 "blueviolet"          )
                          (139   0   0 "darkred"             )
                          (139   0 139 "darkmagenta"         )
                          (139  69  19 "saddlebrown"         )
                          (143 188 143 "darkseagreen"        )
                          (144 238 144 "lightgreen"          )
                          (147 112 219 "mediumpurple"        )
                          (148   0 211 "darkviolet"          )
                          (152 251 152 "palegreen"           )
                          (153  50 204 "darkorchid"          )
                          (154 205  50 "yellowgreen"         )
                          (160  82  45 "sienna"              )
                          (165  42  42 "brown"               )
                          (169 169 169 "darkgray"            )
                          (169 169 169 "darkgrey"            )
                          (173 216 230 "lightblue"           )
                          (173 255  47 "greenyellow"         )
                          (175 238 238 "paleturquoise"       )
                          (176 196 222 "lightsteelblue"      )
                          (176 224 230 "powderblue"          )
                          (178  34  34 "firebrick"           )
                          (184 134  11 "darkgoldenrod"       )
                          (186  85 211 "mediumorchid"        )
                          (188 143 143 "rosybrown"           )
                          (189 183 107 "darkkhaki"           )
                          (192 192 192 "silver"              )
                          (199  21 133 "mediumvioletred"     )
                          (205  92  92 "indianred"           )
                          (205 133  63 "peru"                )
                          (210 105  30 "chocolate"           )
                          (210 180 140 "tan"                 )
                          (211 211 211 "lightgray"           )
                          (211 211 211 "lightgrey"           )
                          (216 191 216 "thistle"             )
                          (218 112 214 "orchid"              )
                          (218 165  32 "goldenrod"           )
                          (219 112 147 "palevioletred"       )
                          (220  20  60 "crimson"             )
                          (220 220 220 "gainsboro"           )
                          (221 160 221 "plum"                )
                          (222 184 135 "burlywood"           )
                          (224 255 255 "lightcyan"           )
                          (230 230 250 "lavender"            )
                          (233 150 122 "darksalmon"          )
                          (238 130 238 "violet"              )
                          (238 232 170 "palegoldenrod"       )
                          (240 128 128 "lightcoral"          )
                          (240 230 140 "khaki"               )
                          (240 248 255 "aliceblue"           )
                          (240 255 240 "honeydew"            )
                          (240 255 255 "azure"               )
                          (244 164  96 "sandybrown"          )
                          (245 222 179 "wheat"               )
                          (245 245 220 "beige"               )
                          (245 245 245 "whitesmoke"          )
                          (245 255 250 "mintcream"           )
                          (248 248 255 "ghostwhite"          )
                          (250 128 114 "salmon"              )
                          (250 235 215 "antiquewhite"        )
                          (250 240 230 "linen"               )
                          (250 250 210 "lightgoldenrodyellow")
                          (253 245 230 "oldlace"             )
                          (255   0   0 "red"                 )
                          (255   0 255 "fuchsia"             )
                          (255   0 255 "magenta"             )
                          (255  20 147 "deeppink"            )
                          (255  69   0 "orangered"           )
                          (255  99  71 "tomato"              )
                          (255 105 180 "hotpink"             )
                          (255 127  80 "coral"               )
                          (255 140   0 "darkorange"          )
                          (255 160 122 "lightsalmon"         )
                          (255 165   0 "orange"              )
                          (255 182 193 "lightpink"           )
                          (255 192 203 "pink"                )
                          (255 215   0 "gold"                )
                          (255 218 185 "peachpuff"           )
                          (255 222 173 "navajowhite"         )
                          (255 228 181 "moccasin"            )
                          (255 228 196 "bisque"              )
                          (255 228 225 "mistyrose"           )
                          (255 235 205 "blanchedalmond"      )
                          (255 239 213 "papayawhip"          )
                          (255 240 245 "lavenderblush"       )
                          (255 245 238 "seashell"            )
                          (255 248 220 "cornsilk"            )
                          (255 250 205 "lemonchiffon"        )
                          (255 250 240 "floralwhite"         )
                          (255 250 250 "snow"                )
                          (255 255   0 "yellow"              )
                          (255 255 224 "lightyellow"         )
                          (255 255 240 "ivory"               )
                          (255 255 255 "white"               ))))))))
-->

```diagram
<!-- expand: COLOR-NAME-SAMPLE -->
```
Figure. 色の名前とサンプル

<!-- collapse:begin -->
[$@](F#色の名前とサンプル) のソースはこちら

```lisp
<!-- expand: COLOR-NAME-SAMPLE -->
```
<!-- collapse:end -->


## 更新履歴

　更新履歴です。


--------------------------------------------------------------------------------

<!-- embed:footnotes -->

