<!-- define: APPNAME = diagram -->
<!-- define: BLANK_PARAGRAPH = '　　' -->
<!-- define: TODO = '@((background:red;color:white;)(ToDo : %1))' -->

<!-- title:${APPNAME} readme -->    
<!-- style:./default.css -->			

<!-- config:write-comment -->			
<!-- config:header-numbering 2 4 -->			
<!-- config:entity-numbering-depth 1 -->
<!-- <!-- config:term-link-in-header -->

<!-- filter:diagram  = bash ./diagram.sh  %in %out -->
<!-- filter:plantuml = bash ./plantuml.sh %in %out -->

<!-- PIM の eoPS3MRF1gI というデータで過去にマニュアル作ろうとした形跡があるぞ  -->

# README - ${APPNAME}

　この文書は、 **${APPNAME}** のためのマニュアル文書です。

<!-- anchor: toc-link-target -->
```raw
<h2>Table of contents</h2>
```
<!-- embed:toc-x 2 4 -->
<!-- toc-link: top 'A#toc-link-target' -->

--------------------------------------------------------------------------------

${BLANK_PARAGRAPH}

## ${APPNAME} とは

　${APPNAME} は、テキストベースの作図ツールです。テキスト形式で作成したデータファイルを入力として、
SVG 形式{{fn:SVG は Scalable Vector Graphics の略です。}}の画像ファイルを生成します。

　${APPNAME} の入力データの記述方法には違和感を感じるかもしれません。これは、${APPNAME} が 
Common LISP 言語で実装されており、入力データも Common LISP 上に作成された DSL(domain specific 
language) で記述するためです。しかし、このツールを使ってみたいからといって、知りもしない言語の
お勉強から始めたいとは思わないでしょう。そのため、このマニュアルではサンプルをたくさん提示し、
Common LISP の詳細には極力立ち入らないようにします{{fn:もともと LISPer だという方で DSL の詳細を知りたい方は、 \
申し訳ありませんがコードを直接参照してください。}}。

## 簡単なサンプル

　簡単なサンプルから始めましょう。以下のような入力を ${APPNAME} に与えると、

<!-- snippet: FIRST-SAMPLE
(diagram (300 150)
  (grid)
  (rect   '( 50  50) 80 60 :fill :powderblue :id :x)
  (circle '(250 100) 40    :fill :moccasin   :id :y)
  (connect :x :y :end2 :arrow))
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


　「入力を ${APPNAME} に与える」というのは、具体的には入力データを記述したファイルの名前を
パラメータとして ${APPNAME} を起動することを意味します。作成される SVG 画像は標準出力に
書き出されるので、ファイルにリダイレクトしてください。以下のように。

```sh
diagram ./input.digram > ./output.svg
```

　入力ファイル名が与えられない場合、${APPNAME} は標準入力からデータを読み取ろうとします。そのため、
以下のように書くこともできます。

```sh
cat ./input.digram | diagram > ./output.svg
```

　余談ですが、SVG 画像には gzip 圧縮した svgz という形式もあります。以下のように出力を gzip に
通してやれば作成できます。

```sh
cat ./input.digram | diagram | gzip > ./output.svgz
```

　では、改めて、先ほどの入力データをもう一度みてみましょう。

```lisp
<!-- expand: FIRST-SAMPLE -->
```

　初めての ${APPNAME} データなので、順番に内容をみていきましょう。まずは雰囲気で理解してください。

* `diagram` で「幅 300、高さ 150」の画像を作成
* `grid` で背景にグリッド線を描画
* `rect` で四角形を作成 - 中心位置は左上から (50, 50)、大きさは幅 80、高さ 60
	* `:fill` で塗り潰しの色を powderblue に指定
	* これに x という ID を設定
* `circle` で円を作成 - 中心位置は左上から (250, 100)、半径は 40
	* `:fill` で塗り潰しの色を moccasin に指定
	* これに y という ID を設定
* `connect` で、x から y に向かって接続線を描画
	* `:end2` で終端の形状を arrow に設定

${BLANK_PARAGRAPH}

　では、ここで四角形を描画した行に注目してみましょう。

```lisp
(rect  '(50 50) 80 60 :fill :powderblue :id :x)
```

　全体を括る括弧の中にいくつかのデータが書かれていて、最初は `rect` で始まっています。この
最初の `rect` が「四角形を描画せよ」という指示で、残りは何処にどのような四角形を描くかの
指示です。続く `'(50 50) 80 60` は位置（座標）、幅、高さの指定です。 `'(50 50)` という表記
は、位置を即値（具体的な値）で記述する時のお約束だと（今は）理解しておいてください。座標の指定
方法には色々ありますが、のちほど順番に説明します。

　その後ろに続く `:fill :powderblue` や `:id :x` といったものは、全て「名前付きパラメータ
{{fn:Lisper の方へ：要するにキーワードパラメータです。}}」です。${APPNAME} では多くの
パラメータが省略可能で、それらの省略可能パラメータを指定する場合はパラメータ名も書いてあげる
必要があります。なお、 `:fill` のようにコロンで始まる名前は「キーワード」と呼ばれるもので、
省略可能パラメータの名前はキーワードで指定します。

　では、この `rect` の省略可能なパラメータにはどんなものがあるのでしょうか。以下に `rect` 
の全体を示します。

```lisp
(defmacro rect (center width height
                &key rx ry fill stroke rotate link layer id filter contents) ...)
```

　先頭の `defmacro` はひとまず気にしないでください。 `rect` に続く括弧の中がパラメータの
全体で、 `center width height` が必須パラメータ、 `&key` に続く全てが省略可能なパラメータ
です。 `fill` と `id` はもう使いました。その他のパラメータについてはまた別のところで説明
します。

${BLANK_PARAGRAPH}

　次のサンプルはもう少し複雑です。

<!-- snippet: SECOND-SAMPLE
(diagram (450 150)
  (grid)
  (drop-shadow)
  (with-shape-filter (:drop-shadow)
    (textbox (y+ canvas.center 20) "diagram" :height 40 :fill :cornsilk :id :app)
    (with-options (:stroke '(:color :navy :width 2)
                   :fill   '(:color :skyblue :opacity 0.3))
      (document (x+ app.center -175) 80 60 "input~%file" :id :in)
      (document (x+ app.center  175) 80 60 "svg~%image"  :id :out))
    (with-options (:fill :white)
      (balloon (xy+ app.center 110 -60) "Made with LISP." app.topright)
      (block-arrow1  in.right app.left 15 :margin 10)
      (block-arrow1 app.right out.left 15 :margin 10))))
-->

```diagram
<!-- expand: SECOND-SAMPLE -->
```
Figure. 簡単なサンプル-2


　このサンプルは、以下のコードで生成されています。

```lisp
<!-- expand: SECOND-SAMPLE -->
```

　こちらも、ざっくりした説明をしておきます。

* diagram と grid は先程と同じなので省略
* drop-shadow という種類の「フィルタ」の使用を宣言
* with-shape-filter でデフォルトのフィルタを drop-shadow に設定
	* textbox でテキストボックスを作成 : 場所は画像の中心（canvas.center）から y 軸方向に 20、 \
テキストは "diagram"、これに app という ID を設定
	* with-options で、デフォルトの線を太さ 2 の `navy` に、デフォルトの塗りつぶしを不透明度 0.3 の `skyblue` にそれぞれ設定
		* document でドキュメントを作成 : 場所は app の中心（app.center）から x 軸方向に -175、 \
幅と高さは 80 60、テキストは "input~%file"、これに in という ID を設定
		* 上記と同じ要領で out という ID のドキュメントを作成
	* with-options で、デフォルトの塗りつぶしを `white` に設定
		* balloon で app の右上付近に吹き出しを作成 : テキストは "Made with LISP."、接続点は app の \
右上端（app.topright）
		* block-arrow1 で in と app の間にブロック矢印を作成
		* 上記と同じ要領で app と out の間にブロック矢印を作成


${BLANK_PARAGRAPH}

　この 2 つめのサンプルには、新しいポイントがいくつかあります。もう少し詳しく説明します。

* `drop-shadow` で宣言し、 `with-shape-filter` でデフォルトを設定しているのを「フィルタ」といいます。 \
四角形や円に表示されている影が drop shadow です。
* `with-options` を使って、デフォルトの塗り潰しや線を指定しています。 `:stroke` や `:fill` を \
毎回指定する必要がなくなります。
	* `:stroke` では線の色 `navy` の他に `:width` で線の太さを指定しています。
	* `:fill` では塗り潰しの色 `skyblue` の他に `:opacity` で不透明度を指定しています。これは、0（完全に透明）から 1  \
（完全に不透明）までを指定します。ドキュメントの塗り潰しが少し透けているのがわかると思います。
* `:id` を使って付与した ID を使って `app.center` などと書くことで既出の要素の「中心座標」を指定できます。 \
これは `'(50 50)` といった座標表記の代わりになります。
	* `canvas` は特別な ID で、現在描画中の「キャンバス」を意味します。今の時点では、生成する画像の四角形全体だと \
理解しておいてください。
	* `(y+ canvas.center 20)` といった表記によってある位置から x 軸や y 軸に指定されただけ移動した座標を計算する \
ことができます。
* 複数行のテキスト扱うことができる要素では、 `"input~%file"` のように ~% を使って改行を表します。

${BLANK_PARAGRAPH}

　サンプルは以上です。雰囲気は掴めたと思うので、あとは続いて各種の図形要素について説明します。

## 基本的な図形

　SVG 規格における基本図形（一部例外あり）から紹介します。以下のサンプルはサブセクションへのリンクに
なっています。

<!-- define: HASH_RECT    = '[](#四角形)' -->
<!-- define: HASH_CIRCLE  = '[](#円)' -->
<!-- define: HASH_ELLIPSE = '[](#楕円)' -->
<!-- define: HASH_POLYGON = '[](#多角形)' -->
<!-- define: HASH_LINE    = '[](#線)' -->
<!-- define: HASH_ARC     = '[](#円弧)' -->
<!-- define: HASH_TEXT    = '[](#テキスト)' -->

```diagram
(diagram (800 120)
  (glow-shadow :id :foo-filter)
  ;(grid)
  (let ((w  80)
        (h 100)
        (bgclr :white)) ;;(make-fill :color :lightgray :opacity 0.4 )));;
    (defs (w h :rect-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (rect `(,(/ w 2) ,(/ w 2)) 50 50 :fill :skyblue :stroke :blue)
	  (text `(,(/ w 2) ,(- h 5)) "四角形" :align :center))
    (defs (w h :circle-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (circle `(,(/ w 2) ,(/ w 2)) 25 :fill :bisque :stroke :brown)
	  (text `(,(/ w 2) ,(- h 5)) "円" :align :center))
    (defs (w h :ellipse-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (ellipse `(,(/ w 2) ,(/ w 2)) 30 20 :fill :beige :stroke :olive)
	  (text `(,(/ w 2) ,(- h 5)) "楕円" :align :center))
    (defs (w h :polygon-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (polygon '((40.00 10.00) (32.75 31.50) (10.25 31.50)
                 (28.25 45.00) (21.75 66.50) (40.00 53.75)
                 (58.25 66.50) (51.75 45.00) (69.75 31.50)
                 (47.25 31.50)) :stroke :red :fill :lightpink)
	  (text `(,(/ w 2) ,(- h 5)) "多角形" :align :center))
	(defs (w h :line-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (line '((20 20) (60 20) (20 65) (60 65)) :stroke :black)
	  (text `(,(/ w 2) ,(- h 5)) "線" :align :center))
    (defs (w h :arc-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (arc `(,(/ w 2) ,(+ 5 (/ w 2))) 25 25 0 120 60 :stroke '(:color :navy :width 8))
	  (text `(,(/ w 2) ,(- h 5)) "円弧" :align :center))
    (defs (w h :text-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (text `(,(/ w 2) 55) "Text" :align :center
            :font '(:family "Times New Roman" :size 30 :style :italic :filter :foo-filter))
	  (text `(,(/ w 2) ,(- h 5)) "テキスト" :align :center))
    (use :rect-grp    '(100 60) :link "${HASH_RECT}")
    (use :circle-grp  '(200 60) :link "${HASH_CIRCLE}")
    (use :ellipse-grp '(300 60) :link "${HASH_ELLIPSE}")
    (use :polygon-grp '(400 60) :link "${HASH_POLYGON}")
    (use :line-grp    '(500 60) :link "${HASH_LINE}")
    (use :arc-grp     '(600 60) :link "${HASH_ARC}")
    (use :text-grp    '(700 60) :link "${HASH_TEXT}")))
```

### 四角形
<!-- autolink: [rect](#四角形) -->

<!-- snippet: RECTANGLE-SAMPLE
(diagram (300 100)
  (grid)
  (rect '(150 50) 150 60 :rx 10 :stroke :navy :fill :skyblue))
-->

　`rect` によって四角形を描画できます。

```diagram
<!-- expand: RECTANGLE-SAMPLE -->
```
Figure. rect のサンプル


<!-- collapse:begin -->
　※上記サンプルのソースはこちら。

```lisp
<!-- expand: RECTANGLE-SAMPLE -->
```
<!-- collapse:end -->

　rect のパラメータ構成は以下の通りです。

```lisp
(defmacro rect (center width height
                &key rx ry fill stroke rotate link layer id filter contents) ... )
```

<!-- stack:push tr style="font-size: 14;" -->

Table. rect のパラメータ
| パラメータ | 説明                                                                           |
|:==========|:--------------------------------------------------------------------------------------|
| center    | rect の中心点を指定します。詳細は「[](#座標と位置)」を参照してください。                 |
| width     | rect の幅を数値で指定します。                                                        |
| height    | rect の高さを数値で指定します。                                                      |
| rx, ry    | rect 角を丸くしたい場合に、角の x 半径（rx）と y 半径（ry）を数値で指定します。<br> \
rx と ry のどちらかだけを指定すると、もう一方も同じであると見なされます。 |
| fill      | rect 内部の塗り潰しを指定します。詳細は「[](#フィル)」を参照してください。           |
| stroke    | rect を描画する線を指定します。詳細は「[](#ストローク)」を参照してください。         |
| rotate    | rect を回転させる場合、その角度を指定します。詳細は「[](#回転)」を参照してください。 |
| link      | rect をリンクにする場合、リンク先を指定します。詳細は「[](#リンク)」を参照してください。 |
| layer     | rect をレイヤーに所属される場合、その名前をキーワードで指定します。<br> \
詳細は「[](#レイヤー)」を参照してください。           |
| id        | rect に ID を付与したい場合、その名前をキーワードで指定します。<br> \
詳細は「[](#IDと参照)」を参照してください。            |
| filter    | rect の描画にフィルタ効果を適用したい場合、その名前を指定します。<br> \
詳細は「[](#フィルタ)」を参照してください。 |
| contents  | rect の内部をサブキャンバスとした描画をしたい場合、その内容を指定します。<br> \
詳細は「[](#サブキャンバス)」を参照してください。 |

<!-- stack:pop tr -->

### 円
<!-- autolink: [circle](#円) -->

<!-- snippet: CIRCLE-SAMPLE
(diagram (300 100)
  (grid)
  (circle '(150 50) 30 :stroke :brown :fill :bisque))
-->

　`circle` によって円を描画できます。

```diagram
<!-- expand: CIRCLE-SAMPLE -->
```
Figure. circle のサンプル


<!-- collapse:begin -->
　※上記サンプルのソースはこちら。

```lisp
<!-- expand: CIRCLE-SAMPLE -->
```
<!-- collapse:end -->

　circle のパラメータ構成は以下の通りです。

```lisp
(defmacro circle (center radius
                  &key fill stroke link layer id filter contents) ... )
```

<!-- stack:push tr style="font-size: 14;" -->

Table. circle のパラメータ
| パラメータ | 説明                                                                           |
|:==========|:--------------------------------------------------------------------------------------|
| center    | circle の中心点を指定します。詳細は「[](#座標と位置)」を参照してください。                 |
| radius    | circle の半径を数値で指定します。                                                        |
| fill      | circle 内部の塗り潰しを指定します。詳細は「[](#フィル)」を参照してください。           |
| stroke    | circle を描画する線を指定します。詳細は「[](#ストローク)」を参照してください。         |
| link      | circle をリンクにする場合、リンク先を指定します。詳細は「[](#リンク)」を参照してください。 |
| layer     | circle をレイヤーに所属される場合、その名前をキーワードで指定します。<br> \
詳細は「[](#レイヤー)」を参照してください。           |
| id        | circle に ID を付与したい場合、その名前をキーワードで指定します。<br> \
詳細は「[](#IDと参照)」を参照してください。            |
| filter    | circle の描画にフィルタ効果を適用したい場合、その名前を指定します。<br> \
詳細は「[](#フィルタ)」を参照してください。 |
| contents  | circle の内部をサブキャンバスとした描画をしたい場合、その内容を指定します。<br> \
詳細は「[](#サブキャンバス)」を参照してください。 |

<!-- stack:pop tr -->

### 楕円
<!-- autolink: [ellipse](#楕円) -->

<!-- snippet: ELLIPSE-SAMPLE
(diagram (300 100)
  (grid)
  (ellipse '(150 50) 60 30 :stroke :olive :fill :beige))
-->

　`ellipse` によって楕円を描画できます。

```diagram
<!-- expand: ELLIPSE-SAMPLE -->
```
Figure. ellipse のサンプル


<!-- collapse:begin -->
　※上記サンプルのソースはこちら。

```lisp
<!-- expand: ELLIPSE-SAMPLE -->
```
<!-- collapse:end -->

　ellipse のパラメータ構成は以下の通りです。

```lisp
(defmacro ellipse (center rx ry
                   &key fill stroke link layer id filter contents) ... )
```

<!-- stack:push tr style="font-size: 14;" -->

Table. ellipse のパラメータ
| パラメータ | 説明                                                                           |
|:==========|:--------------------------------------------------------------------------------------|
| center    | ellipse の中心点を指定します。詳細は「[](#座標と位置)」を参照してください。                 |
| rx        | ellipse の x 軸方向の半径を数値で指定します。                                             |
| ry        | ellipse の y 軸方向の半径を数値で指定します。                                             |
| fill      | ellipse 内部の塗り潰しを指定します。詳細は「[](#フィル)」を参照してください。           |
| stroke    | ellipse を描画する線を指定します。詳細は「[](#ストローク)」を参照してください。         |
| rotate    | ellipse を回転させる場合、その角度を指定します。詳細は「[](#回転)」を参照してください。 |
| link      | ellipse をリンクにする場合、リンク先を指定します。詳細は「[](#リンク)」を参照してください。 |
| layer     | ellipse をレイヤーに所属される場合、その名前をキーワードで指定します。<br> \
詳細は「[](#レイヤー)」を参照してください。           |
| id        | ellipse に ID を付与したい場合、その名前をキーワードで指定します。<br> \
詳細は「[](#IDと参照)」を参照してください。            |
| filter    | ellipse の描画にフィルタ効果を適用したい場合、その名前を指定します。<br> \
詳細は「[](#フィルタ)」を参照してください。 |
| contents  | ellipse の内部をサブキャンバスとした描画をしたい場合、その内容を指定します。<br> \
詳細は「[](#サブキャンバス)」を参照してください。 |

<!-- stack:pop tr -->

### 多角形
<!-- autolink: [polygon](#多角形) -->

<!-- snippet: POLYGON-SAMPLE
(diagram (300 100)
  (grid)
  (polygon '((150.00 10.00) (139.85 40.10) (108.35 40.10)
             (133.55 59.00) (124.45 89.10) (150.00 71.25)
             (175.55 89.10) (166.45 59.00) (191.65 40.10)
             (160.15 40.10)) :stroke :red :fill :lightpink))
-->

　`polygon` によって多角形、すなわち複数の直線からなる形状を描画できます。

```diagram
<!-- expand: POLYGON-SAMPLE -->
```
Figure. polygon のサンプル


<!-- collapse:begin -->
　※上記サンプルのソースはこちら。

```lisp
<!-- expand: POLYGON-SAMPLE -->
```
<!-- collapse:end -->

　polygon のパラメータ構成は以下の通りです。

```lisp
(defmacro polygon (points &key fill stroke link layer id filter contents) ... )
```

<!-- stack:push tr style="font-size: 14;" -->

Table. polygon のパラメータ
| パラメータ | 説明                                                                           |
|:==========|:--------------------------------------------------------------------------------------|
| points    | polygon を構成する点のリストを指定します。詳細は「[](#座標と位置)」を参照してください。   |
| fill      | polygon 内部の塗り潰しを指定します。詳細は「[](#フィル)」を参照してください。           |
| stroke    | polygon を描画する線を指定します。詳細は「[](#ストローク)」を参照してください。         |
| link      | polygon をリンクにする場合、リンク先を指定します。詳細は「[](#リンク)」を参照してください。 |
| layer     | polygon をレイヤーに所属される場合、その名前をキーワードで指定します。<br> \
詳細は「[](#レイヤー)」を参照してください。           |
| id        | polygon に ID を付与したい場合、その名前をキーワードで指定します。<br> \
詳細は「[](#IDと参照)」を参照してください。            |
| filter    | polygon の描画にフィルタ効果を適用したい場合、その名前を指定します。<br> \
詳細は「[](#フィルタ)」を参照してください。 |
| contents  | polygon の内部をサブキャンバスとした描画をしたい場合、その内容を指定します。<br> \
詳細は「[](#サブキャンバス)」を参照してください。 |

<!-- stack:pop tr -->

### 線
<!-- autolink: [line](#線) -->

<!-- snippet: LINE-SAMPLE
(diagram (300 100)
  (grid)
  (line '((100 50) (125 50)
          (130 30) (140 70)
          (150 30) (160 70)
          (170 30) (180 70)
          (185 50) (210 50)) :stroke :red))
-->

　`line` によって直線（または複数の線分からなる折線）を描画できます。

```diagram
<!-- expand: LINE-SAMPLE -->
```
Figure. line のサンプル


<!-- collapse:begin -->
　※上記サンプルのソースはこちら。

```lisp
<!-- expand: LINE-SAMPLE -->
```
<!-- collapse:end -->

　line のパラメータ構成は以下の通りです。

```lisp
(defmacro line (points &key stroke label end1 end2 layer filter id) ... )
```

<!-- stack:push tr style="font-size: 14;" -->

Table. line のパラメータ
| パラメータ | 説明                                                                           |
|:==========|:--------------------------------------------------------------------------------------|
| points    | line を構成する点のリストを指定します。詳細は「[](#座標と位置)」を参照してください。    |
| stroke    | line を描画する線を指定します。詳細は「[](#ストローク)」を参照してください。            |
| label     | line にラベルをつける場合に指定します。詳細は「[](#ラベル)」を参照してください。        |
| end1,end2 | line の終端に装飾を付ける場合は指定します。詳細は「[](#終端マーク)」を参照してください。 |
| layer     | line をレイヤーに所属される場合、その名前をキーワードで指定します。<br> \
詳細は「[](#レイヤー)」を参照してください。           |
| id        | line に ID を付与したい場合、その名前をキーワードで指定します。<br> \
詳細は「[](#IDと参照)」を参照してください。            |
| filter    | line の描画にフィルタ効果を適用したい場合、その名前を指定します。<br> \
詳細は「[](#フィルタ)」を参照してください。 |

<!-- stack:pop tr -->

### 円弧
<!-- autolink: [arc](#円弧) -->

<!-- snippet: ARC-SAMPLE
(diagram (300 100)
  (grid)
  (arc '(150 55) 25 25 0 30 300 :stroke '(:color :navy :width 8)))
-->

　`arc` によって円弧を描画できます。

```diagram
<!-- expand: ARC-SAMPLE -->
```
Figure. arc のサンプル


<!-- collapse:begin -->
　※上記サンプルのソースはこちら。

```lisp
<!-- expand: ARC-SAMPLE -->
```
<!-- collapse:end -->

　arc のパラメータ構成は以下の通りです。

```lisp
(defmacro arc (center rx ry x-axis-rotation degree1 degree2
                                            &key stroke layer filter id) ... )
```

　各パラメータの説明を以下の表に示します。簡単に説明すると、 `center` を中心とし
た x 半径が `rx` 、y 半径が `ry` の楕円を `x-axis-rotation` だけ回転させたもののうち、
角度 `degree1` から（時計回りに） `degree2` までの部分弧を描きます。

<!-- stack:push tr style="font-size: 14;" -->

Table. arc のパラメータ
| パラメータ       | 説明                                                                                 |
|:================|:--------------------------------------------------------------------------------------|
| center          | ベースとなる楕円の中心点を指定します。詳細は「[](#座標と位置)」を参照してください。   |
| rx, ry          | ベースとなる楕円の x 軸方向の半径、および y 軸方向の半径を数値で指定します。              |
| x-axis-rotation | ベースとなる楕円の回転角（x 軸に対してどれだけ回転させるか）を指定します。                |
| degree1         | arc で描く円弧を「時計回りに描く」場合の開始角度を数値で指定します。                      |
| degree2         | arc で描く円弧を「時計回りに描く」場合の終了角度を数値で指定します。                      |
| stroke          | arc を描画する線を指定します。詳細は「[](#ストローク)」を参照してください。            |
| layer           | arc をレイヤーに所属される場合、その名前をキーワードで指定します。<br> \
詳細は「[](#レイヤー)」を参照してください。           |
| filter          | arc の描画にフィルタ効果を適用したい場合、その名前を指定します。<br> \
詳細は「[](#フィルタ)」を参照してください。 |
| id              | arc に ID を付与したい場合、その名前をキーワードで指定します。<br> \
詳細は「[](#IDと参照)」を参照してください。            |

<!-- stack:pop tr -->

${BLANK_PARAGRAPH}

　以下に例を示します。 `(100 50)` を中心とした `rx=40, ry=30` の楕円を 45 度回転させたものが
ベースで、これはライトグレーの太い楕円で描画されています。このうち、0 度から 90 度までの部分を
円弧として描画しています。つまり、これは `(arc '(100 50) 40 30 45 0 90)` による描画となります。

```diagram
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
Figure. arc のサンプル - 2


　正円をベースとした円弧を描画したい場合、 `rx` と `ry` を同じ値に指定します。この場合、回転
させることに意味はないので、 `x-axis-rotation` は 0 にしてください。

　arc を使用した円弧の描画は、「中心と角度」が明らかな場合に使用します。そうではなく、円弧の
開始点と終了点が明らかな場合は、「[](#パス)」を使用した方が良いでしょう。

### テキスト
<!-- autolink: [text](#テキスト) -->

<!-- snippet: TEXT-SAMPLE
(diagram (300 100)
  (grid)
  (glow-shadow :color-matrix '(0 0 0 0   0
                               0 0 0 0.4 0
                               0 0 0 0   0
                               0 0 0 1   0))
  (text '(150 70) "Text" :align :center
        :font '(:family "Times New Roman" :size 48
                :fill :green :style :italic :filter :glow-shadow)))
-->

　`text` によってテキストを描画できます。

```diagram
<!-- expand: TEXT-SAMPLE -->
```
Figure. text のサンプル


<!-- collapse:begin -->
　※上記サンプルのソースはこちら。

```lisp
<!-- expand: TEXT-SAMPLE -->
```
<!-- collapse:end -->

　text のパラメータ構成は以下の通りです。なお、text では __複数行に渡るテキストは描画できません__ 。
複数行のテキストを描画したい場合は、paragraph を使用してください。

```lisp
(defmacro text (position text &key align font link layer id) ... )
```

${BLANK_PARAGRAPH}

<!-- stack:push tr style="font-size: 14;" -->

Table. text のパラメータ
| パラメータ | 説明                                                                           |
|:==========|:--------------------------------------------------------------------------------------|
| position  | text で描くテキストの基準点を指定します。詳細は「[](#座標と位置)」を参照してください。     |
| text      | text で描くテキストを文字列で指定します。                                                |
| align     | text で描くテキストのアライメントを `:left :center :right` のいずれかで指定します。  |
| font      | text で描くテキストを「時計回りに描く」場合の終了角度を数値で指定します。                      |
| link      | text をリンクにする場合、リンク先を指定します。詳細は「[](#リンク)」を参照してください。 |
| layer     | text をレイヤーに所属される場合、その名前をキーワードで指定します。<br> \
詳細は「[](#レイヤー)」を参照してください。           |
| id        | text に ID を付与したい場合、その名前をキーワードで指定します。<br> \
詳細は「[](#IDと参照)」を参照してください。            |

<!-- stack:pop tr -->

${BLANK_PARAGRAPH}

　position と align の関係を以下に示します。以下において、赤い点が position で、
align 指定はテキストで示されています。

```diagram
(diagram (300 100)
  (grid)
  (labels ((impl (y text align)
             (circle `(150 ,y) 3 :stroke :none :fill :red)
             (text   `(150 ,y) text :align align)))
    (impl 30 "align :left"   :left)
    (impl 60 "align :center" :center)
    (impl 90 "align :right " :right)))
```
Figure. text の align サンプル

## パス
<!-- autolink: [path](#パス) -->

<!-- snippet: PATH-SAMPLE
(diagram (300 100)
  (grid)
  (path '((:move-to (120 50))
          (:arc-to 30 30 0 1 1 (150 80))
          (:line-to (150 50)) :close-path) :stroke :black :fill :rosybrown))
-->

　`path` によって直線や曲線からなる複雑な図形を描画できます。

```diagram
<!-- expand: PATH-SAMPLE -->
```
Figure. path のサンプル


<!-- collapse:begin -->
　※上記サンプルのソースはこちら。

```lisp
<!-- expand: PATH-SAMPLE -->
```
<!-- collapse:end -->

　path のパラメータ構成は以下の通りです。

```lisp
(defmacro path (data &key fill stroke layer filter id) ... )
```

<!-- stack:push tr style="font-size: 14;" -->

Table. path のパラメータ
| パラメータ | 説明                                                                           |
|:==========|:--------------------------------------------------------------------------------------|
| data      | path を構成するデータを指定します。詳細は後述します。                                |
| fill      | path 内部の塗り潰しを指定します。詳細は「[](#フィル)」を参照してください。           |
| stroke    | path を描画する線を指定します。詳細は「[](#ストローク)」を参照してください。         |
| layer     | path をレイヤーに所属される場合、その名前をキーワードで指定します。<br> \
詳細は「[](#レイヤー)」を参照してください。           |
| id        | path に ID を付与したい場合、その名前をキーワードで指定します。<br> \
詳細は「[](#IDと参照)」を参照してください。            |
| filter    | path の描画にフィルタ効果を適用したい場合、その名前を指定します。<br> \
詳細は「[](#フィルタ)」を参照してください。 |

<!-- stack:pop tr -->

${BLANK_PARAGRAPH}

　data パラメータについて説明します。data パラメータは以下のような **ディレクティブ** キーワードを
要素とするリストで指定します。点や値の指定を伴うディレクティブは、それ自体をリストにする必要があります。

<!-- stack:push tr style="font-size: 14;" -->

Table. path の data で使用できるディレクティブ
| ディレクティブ  | 説明                                                                  |
|:===============|:----------------------------------------------------------------------|
| `:move-to`     | 指定した点に（線を描くことなく）移動します。                             |
| `:line-to`     | 現在の点から指定した点に向かって（線を描きながら）移動します。            |
| `:h-line-to`   | 現在の点から指定した x 座標に向かって水平線を描きながら移動します。       |
| `:v-line-to`   | 現在の点から指定した y 座標に向かって垂直線を描きながら移動します。       |
| `:arc-to`      |      |
| `:2d-curve-to` |      |
| `:3d-curve-to` |      |
| `:absolute`    | 後続のディレクティブを、現在のキャンバスに対する絶対座標として処理します。 |
| `:relative`    | 後続のディレクティブを、現在の点に対する相対座標として処理します。         |
| `:close-path`  | パスを閉じます。すなわち、現在の点から先頭の点までを結ぶ直線を引きます。   |

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

### :move-to ディレクティブ
<!-- autolink: [:move-to](#:move-to ディレクティブ) -->

　`(:move-to pt)` という記述により、線を描くことなく指定した点 pt に移動します。また、 
`(:move-to pt1 pt2 pt3 ...)` のように複数の点を記述することができ、これは `(:move-to pt1)  \
(:line-to pt2 pt3 ...)` と等価になります。以下に例を示します。

```diagram
(diagram (400 100)
  (grid)
  (path '((:move-to (200 10) (230 50) (170 50) (200 10)))
          :stroke :black :fill :rosybrown)
  (text '(200 80) "(:move-to (200 10) (230 50) (170 50) (200 10))" :align :center ))
```
Figure. :move-to ディレクティブのサンプル

　なお、:move-to で指定する点は :absolute および :relative の影響を受けます。

### :line-to ディレクティブ
<!-- autolink: [:line-to](#:line-to ディレクティブ) -->

　`(:line-to pt)` という記述により、現在の点から直線を描きながら指定した点 pt に移動します。
`(:line-to pt1 pt2 pt3 ...)` のように複数の点を記述することができます。これは現在の点から
順番に直線を描きながら移動します。

```diagram
(diagram (400 100)
  (grid)
  (path '((:move-to (200 10))
          (:line-to (230 50) (170 50) (200 10)))
          :stroke :black :fill :rosybrown)
  (text '(70 75) "(:move-to (200 10))" :align :left )
  (text '(70 95) "(:line-to (230 50) (170 50) (200 10))" :align :left))
```
Figure. :line-to ディレクティブのサンプル

　なお、:line-to で指定する点は :absolute および :relative の影響を受けます。

### :h-line-to ディレクティブ
<!-- autolink: [:h-line-to](#:h-line-to ディレクティブ) -->

${{TODO}{まだ記述されていません。}}

### :v-line-to ディレクティブ
<!-- autolink: [:v-line-to](#:v-line-to ディレクティブ) -->

${{TODO}{まだ記述されていません。}}

### :arc-to ディレクティブ
<!-- autolink: [:arc-to](#:arc-to ディレクティブ) -->

${{TODO}{まだ記述されていません。}}

```
'(arc-to rx ry x-axis-rotation large-arc-flag sweep-flag pt)
```

```diagram
(diagram (400 100)
  (grid)
  (path '((:move-to (150 50))
          (:arc-to 30 30 0 0 1 (250 80)) :close-path)
          :stroke :black :fill :rosybrown)
;  (text '(70 75) "(:move-to (200 10))" :align :left )
;  (text '(70 95) "(:line-to (230 50) (170 50) (200 10))" :align :left)
)
```
Figure. :arc-to ディレクティブのサンプル

```diagram
(diagram (400 140)
  (grid)
  (with-options (:fill :none :stroke '(:color :lightgray :width 2))
    (ellipse '(250 50) 100 40)
    (ellipse '(150 90) 100 40))
  (with-options (:fill :red :stroke :none)
    (circle '(150 50) 3)
    (circle '(250 90) 3))
  (with-options (:font '(:fill :red))
    (text '(145  45) "pt1" :align :right)
    (text '(255 105) "pt2" :align :left)))
```

　上記を見ればわかる通り、指定した２つの点を通過する半径 rx, ry の楕円には４種類あります。

```diagram
(diagram (510 220)
  (grid)
  (defs (240 100 :back)
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

```diagram
(diagram (200 200)
  (grid)
  ;(centered-to-svg 100 100 40 20 30 90 0)
  (ellipse '(100 100) 40 20 :stroke '(:color :lightgray :width 3))
  (with-options (:fill :none :stroke :red)
    (path '((:move-to (134.64101615137756 110.0))
            (:arc-to 40 20 0 0 1 (80.0 117.32050807568878))))))
```

```diagram
(diagram (200 200)
  (grid)
  ;(centered-to-svg 100 100 40 20 30 90 45)
  (ellipse '(100 100) 40 20 :rotate 45 :stroke '(:color :lightgray :width 3))
  (with-options (:fill :none :stroke :red)
    (path '((:move-to (117.42382961596631 131.56596523969725))
            (:arc-to 40 20 45 0 1 (73.61041566235316 98.10531309018495))))))
```
### :2d-curve-to ディレクティブ
<!-- autolink: [:2d-curve-to](#:2d-curve-to ディレクティブ) -->

${{TODO}{まだ記述されていません。}}

### :3d-curve-to ディレクティブ
<!-- autolink: [:3d-curve-to](#:3d-curve-to ディレクティブ) -->

${{TODO}{まだ記述されていません。}}

### :absolute ディレクティブ
<!-- autolink: [:absolute](#:absolute ディレクティブ) -->

${{TODO}{まだ記述されていません。}}

### :relative ディレクティブ
<!-- autolink: [:relative](#:relative ディレクティブ) -->

${{TODO}{まだ記述されていません。}}

### :close-path ディレクティブ
<!-- autolink: [:close-path](#:close-path ディレクティブ) -->

${{TODO}{まだ記述されていません。}}

## その他の図形

　基本的な図形を組み合わせて作成される、複合的な図形を紹介します。以下のサンプルはサブセクションへの
リンクになっています。

<!-- define: HASH_CONNECTOR  = '[](#コネクタ)' -->
<!-- define: HASH_PARAGRAPH  = '[](#パラグラフ)' -->
<!-- define: HASH_TEXTBOX    = '[](#テキストボックス)' -->
<!-- define: HASH_DOCUMENT   = '[](#ドキュメント)' -->
<!-- define: HASH_FOLDER     = '[](#フォルダ)' -->
<!-- define: HASH_BALLOON    = '[](#吹き出し)' -->
<!-- define: HASH_MEMO       = '[](#メモ)' -->
<!-- define: HASH_CYLINDER   = '[](#円柱)' -->
<!-- define: HASH_EXPLOSION  = '[](#爆発)' -->
<!-- define: HASH_BLOCKARROW = '[](#ブロック矢印)' -->
<!-- define: HASH_BRACE      = '[](#波括弧)' -->
<!-- define: HASH_TABLE      = '[](#テーブル)' -->

```diagram
(diagram (800 240)
  ;(grid)
  (let ((w 100)
        (h 100)
        (bgclr :white)) ;; (make-fill :color :lightgray :opacity 0.4 )));;
	(defs (w h :connect-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (rect   '(20 20) 20 20 :fill :white :stroke :black :id :r1)
      (circle '(80 60) 10    :fill :white :stroke :black :id :r2)
      (connect :r1 :r2 :stroke :black)
	  (text `(,(/ w 2) ,(- h 5)) "コネクタ" :align :center))
	(defs (w h :paragraph-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
	  (paragraph (y+ canvas.center -35) "this is~%multi line~%text." :align :center :font 16)
	  (text `(,(/ w 2) ,(- h 5)) "パラグラフ" :align :center))
	(defs (w h :textbox-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
	  (textbox (y+ canvas.center -10) "this is~%textbox." :rx 5 :ry 5 :align :center :fill :white)
	  (text `(,(/ w 2) ,(- h 5)) "テキストボックス" :align :center))
	(defs (w h :document-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
	  (document (y+ canvas.center -10) 80 60 "this is~%document."
                                       :align :center :stroke :navy :fill :skyblue)
	  (text `(,(/ w 2) ,(- h 5)) "ドキュメント" :align :center))
	(defs (w h :folder-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
	  (folder (y+ canvas.center -5) "this is~%folder."
                                   :align :center :height 50 :stroke :darkkhaki :fill :cornsilk)
	  (text `(,(/ w 2) ,(- h 5)) "フォルダ" :align :center))
	(defs (w h :balloon-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
	  (balloon (y+ canvas.center -15) "this is~%balloon." '(10 75)
                                                    :fill :honeydew :stroke :forestgreen)
	  (text `(,(/ w 2) ,(- h 5)) "吹き出し" :align :center))
	(defs (w h :memo-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
	  (memo (y+ canvas.center -15) "this is~%memo." :width 80 :height 60
                       :valign :top :align :left :stroke :red :fill :lightpink)
	  (text `(,(/ w 2) ,(- h 5)) "メモ" :align :center))
	(defs (w h :cylinder-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
	  (cylinder (y+ canvas.center -10) 65 60 "this is~%cylinder." 
                                       :stroke :darkgray :fill :lightgray)
	  (text `(,(/ w 2) ,(- h 5)) "円柱" :align :center))
	(defs (w h :explosion-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
	  (explosion1 (y+ canvas.center -10) 90 80 "bomb!!" 
                                       :stroke :red :fill :lightpink)
	  (text `(,(/ w 2) ,(- h 5)) "爆発" :align :center))
	(defs (w h :blockarrow-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
	  (block-arrow1 '(0 40) '(100 40) 20 :margin 5 :stroke :brown :fill :burlywood)
	  (text `(,(/ w 2) ,(- h 5)) "ブロック矢印" :align :center))
	(defs (w h :brace-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
	  (brace (y+ canvas.center -20) :upper 80 30 :r 10 :text "this is brace." :stroke :navy)
	  (text `(,(/ w 2) ,(- h 5)) "波括弧" :align :center))
	(defs (w h :table-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
	  (table (y+ canvas.center -10) '(10 10 10 10) '(20 20 20 20) :fills '(:rc :white :r0 :skyblue) :stroke :navy)
	  (text `(,(/ w 2) ,(- h 5)) "テーブル" :align :center))
    (use :connect-grp    '( 70  60) :link "${HASH_CONNECTOR}")
    (use :paragraph-grp  '(200  60) :link "${HASH_PARAGRAPH}")
    (use :textbox-grp    '(330  60) :link "${HASH_TEXTBOX}")
    (use :document-grp   '(460  60) :link "${HASH_DOCUMENT}")
    (use :folder-grp     '(590  60) :link "${HASH_FOLDER}")
    (use :balloon-grp    '(720  60) :link "${HASH_BALLOON}")
    (use :memo-grp       '( 70 180) :link "${HASH_MEMO}")
    (use :cylinder-grp   '(200 180) :link "${HASH_CYLINDER}")
    (use :explosion-grp  '(330 180) :link "${HASH_EXPLOSION}")
    (use :blockarrow-grp '(460 180) :link "${HASH_BLOCKARROW}")
    (use :brace-grp      '(590 180) :link "${HASH_BRACE}")
    (use :table-grp      '(720 180) :link "${HASH_TABLE}")))
```

### コネクタ

　${{TODO}{まだ記述されていません。}}

　${{TODO}{connect でもいいよ。}}

### パラグラフ
<!-- autolink: [paragraph](#パラグラフ) -->

### テキストボックス

　テキストボックスは、[$$](#四角形) と [$$](#テキスト) を組み合わせたようなものです。

<!-- snippet: TEXTBOX-SAMPLE
(diagram (200 100)
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

### ドキュメント

　ドキュメントは、テキストボックスと良く似ていますが、${{TODO}{まだ記述されていません。}}

<!-- snippet: DOCUMENT-SAMPLE
(diagram (200 100)
  (grid)
  (document '(100 50) 100 70 "document~%name"
                    :stroke :navy :fill :skyblue))
-->

```diagram
<!-- expand: DOCUMENT-SAMPLE -->
```
Figure. ドキュメントのサンプル

```lisp
<!-- expand: DOCUMENT-SAMPLE -->
```

　${{TODO}{まだ記述されていません。}}

### フォルダ

　フォルダは、テキストボックスと良く似ていますが、${{TODO}{まだ記述されていません。}}

<!-- snippet: FOLDER-SAMPLE
(diagram (200 100)
  (grid)
  (folder '(100 50) "folder.~%multi line."
                    :stroke :darkkhaki :fill :cornsilk))
-->

```diagram
<!-- expand: FOLDER-SAMPLE -->
```
Figure. フォルダのサンプル

```lisp
<!-- expand: FOLDER-SAMPLE -->
```

　${{TODO}{まだ記述されていません。}}

### 吹き出し

　吹き出しはテキストボックスと良く似ていますが、${{TODO}{まだ記述されていません。}}

<!-- snippet: BALLOON-SAMPLE
(diagram (200 100)
  (grid)
  (rect '(30 70) 30 30 :fill :gray :stroke :black :id :r)
  (balloon '(130 50) "balloon text.~%multi line." r.right
                                   :fill :skyblue :stroke :navy))
-->

```diagram
<!-- expand: BALLOON-SAMPLE -->
```
Figure. 吹き出しのサンプル

```lisp
<!-- expand: BALLOON-SAMPLE -->
```

　${{TODO}{まだ記述されていません。}}

<!-- stack:push tr style="font-size: 14;" -->

Table. 吹き出しに関するデフォルト設定変数
| variable                   | value   | description                        |
|:==========================:|:=======:|:-----------------------------------|
| `*default-balloon-round*`  | 10      | ${{TODO}{まだ記述されていません。}} |
| `*default-balloon-align*`  | :center | ${{TODO}{まだ記述されていません。}} |
| `*default-balloon-valign*` | :center | ${{TODO}{まだ記述されていません。}} |
| `*default-balloon-margin*` | 10      | ${{TODO}{まだ記述されていません。}} |
| `*default-balloon-filter*` | nil     | `:filter` パラメータを省略した場合に適用されるデフォルト設定です。<br> \
この設定も nil の場合、 `*default-shape-filter*` 設定が使用されます。 |

<!-- stack:pop tr -->

### メモ

　メモはテキストボックスと良く似ていますが、${{TODO}{まだ記述されていません。}}

<!-- snippet: MEMO-SAMPLE
(diagram (200 100)
  (grid)
  (memo '(100 50) "memo text.~%multi line."
                  :width 150 :height 80 :crease 30
                  :align :left :valign :top :fill :lightpink :stroke :red))
-->

```diagram
<!-- expand: MEMO-SAMPLE -->
```
Figure. メモのサンプル

```lisp
<!-- expand: MEMO-SAMPLE -->
```

　${{TODO}{まだ記述されていません。}}

### 円柱

　円柱は${{TODO}{まだ記述されていません。}}

<!-- snippet: CYLINDER-SAMPLE
(diagram (200 100)
  (grid)
  (cylinder canvas.center 80 60 "cylinder~%text"
                                   :fill :honeydew :stroke :forestgreen))
-->

```diagram
<!-- expand: CYLINDER-SAMPLE -->
```
Figure. 円柱のサンプル

```lisp
<!-- expand: CYLINDER-SAMPLE -->
```

　${{TODO}{まだ記述されていません。}}

### 爆発

　${{TODO}{まだ記述されていません。}}

<!-- snippet: EXPLOSION-SAMPLE
(diagram (350 150)
  (grid)
  (explosion1 '(100 75) 140 120 "explosion1" :fill :pink :stroke :red)
  (explosion2 '(250 75) 140 120 "explosion2" :fill :pink :stroke :red))
-->

```diagram
<!-- expand: EXPLOSION-SAMPLE -->
```
Figure. 爆発のサンプル

```lisp
<!-- expand: EXPLOSION-SAMPLE -->
```

　${{TODO}{まだ記述されていません。}}

### ブロック矢印

　${{TODO}{まだ記述されていません。}}

<!-- snippet: BLOCKARROW-SAMPLE
(diagram (300 150)
  (grid)
  (with-options (:fill :skyblue
                 :stroke '(:color :navy :width 2))
    (block-arrow1 '(50  40) '(250  40) 20)
    (block-arrow2 '(50 110) '(250 110) 20)))
-->

```diagram
<!-- expand: BLOCKARROW-SAMPLE -->
```
Figure. ブロック矢印のサンプル

```lisp
<!-- expand: BLOCKARROW-SAMPLE -->
```

　${{TODO}{まだ記述されていません。}}

```lisp
(defmacro block-arrow1 (pt1 pt2 width &key size length margin
                                     fill stroke link layer filter id) ...)
(defmacro block-arrow2 (pt1 pt2 width &key size length margin
                                     fill stroke link layer filter id) ...)
```

```diagram
(diagram (400 120)
	(grid)
	(let ((pt1 '( 50 50))
		  (pt2 '(350 50)))
	  (circle pt1 4 :stroke :none :fill :red)
	  (circle pt2 4 :stroke :none :fill :red)
	  (with-options (:font '(:fill :red :size 10))
		(text (y+ $2.center 15) "pt1" :align :center)
		(text (y+ $2.center 15) "pt2" :align :center))
	  (line `(,pt1 ,pt2) :stroke '(:color :red :width 0.5 :dasharray (4 4)))
	  (block-arrow1 pt1 pt2 30 :margin 30 :length 70 :size 60
					:stroke :navy :fill '(:color :skyblue :opacity 0.3))
	  (with-options (:stroke '(:color :gray :dasharray '(3 3)))
		(line `(,pt1 ,(y+ pt1 -30)))
		(line `(,pt2 ,(y+ pt2 -30)))
		(line `(,(x+ pt2 -30) ,(xy+ pt2 -30 -30)))
		(line '((250 20) (220  20)))
		(line '((250 80) (220  80)))
		(line '((250 80) (250 100)))
		(line '((320 50) (320 100))))
	  (with-options (:stroke :brown)
		(let ((em (make-endmark :type :arrow :size :small)))
		  (line '((230  20) (230  80)) :end1 em :end2 em)
		  (line '((150  35) (150  65)) :end1 em :end2 em)
		  (line '((250  90) (320  90)) :end1 em :end2 em)
		  (line `(,(y+  pt1     -15) ,(xy+ pt1 30 -15)) :end1 em :end2 em)
		  (line `(,(xy+ pt2 -30 -20) ,(y+  pt2    -20)) :end1 em :end2 em)))
	  (with-options (:font '(:fill :brown :size 10))
		(text '(150  30) "width"  :align :center)
		(text '(230  95) "size"   :align :right)
		(text '(325 100) "length" :align :left)
		(text '( 60  25) "margin" :align :left)
		(text '(330  25) "margin" :align :left))))
```
Figure. ブロック矢印のパラメータ

* `size` が省略された場合、デフォルト値として `width` の２倍が使用される。
* `length` が省略された場合、デフォルト値として `size` と同じ値が使用される。
* `margin` が省略された場合、デフォルト値として 0 が使用される。

### 波括弧

　${{TODO}{まだ記述されていません。}}

<!-- snippet: BRACE-SAMPLE
(diagram (400 300)
   (grid)
   (with-options (:font   '(:fill :navy :size 16)
                  :stroke '(:color :navy :width 2))
       (brace '(200  40) :upper  240  60 :r 20 :point 150 :text "upper brace" )
       (brace '(200 260) :bottom 240  60 :r 20 :point  60 :text "bottom brace")
       (brace '(360 150) :right   60 200 :r 20 :point 150 :text "right brace" )
       (brace '( 40 150) :left    60 200 :r 20 :point  60 :text "left brace"  )))
-->

```diagram
<!-- expand: BRACE-SAMPLE -->
```
Figure. 波括弧のサンプル

```lisp
<!-- expand: BRACE-SAMPLE -->
```

　${{TODO}{まだ記述されていません。}}

```lisp
(defmacro brace (center direction width height
                        &key r point text font stroke layer filter id) ...)
```

```diagram
(diagram (400 150)
  (grid)
  (with-options (:font   '(:fill :navy :size 16)
                 :stroke '(:color :navy :width 2))
    (brace '(200 70) :upper  240  60 :r 30 :point 150))
  (with-options (:stroke '(:color :gray :dasharray '(3 3)))
    (line '(( 80  10) ( 80 130)))
    (line '((320  40) (320  10)))
    (line '((320  40) (350  40)))
    (line '((230 100) (350 100)))
    (line '((110  70) (110 100)))
    (line '((230 100) (230 130))))
  (with-options (:stroke :brown)
    (let ((em (make-endmark :type :arrow :size :small)))
      (line '(( 80  20) (320  20)) :end1 em :end2 em)
      (line '((340  40) (340 100)) :end1 em :end2 em)
      (line '(( 80  90) (110  90)) :end1 em :end2 em)
      (line '(( 80 120) (230 120)) :end1 em :end2 em)))
  (with-options (:font '(:fill :brown))
    (text '(200  35) "width"  :align :center)
    (text '(345  75) "height" :align :left)
    (text '( 95 105) "r"      :align :center)
    (text '(155 135) "point"  :align :center)))
```
Figure. 波括弧のパラメータ

* `r` が省略された場合、デフォルト値として `height` の 1/3 が指定される
* `point` が省略された場合、デフォルト値として `width` の 1/2 が指定される
* `point` が左右端に近過ぎる場合、 `r` が自動調整される

### テーブル

　`table` を使うことで、表を描画することができます。以下の例では、３行４列の表を作成しています。

<!-- snippet: TABLE-SAMPLE
(diagram (320 120)
   (table '(160 60) '(30 30 30) '(75 75 75 75)
          :stroke :black :fills '(:rc :white :r0 :skyblue)
          :texts '((:foo :bar :baz :quux)
                   (5 6 42 -123)
                   ("asdf" "qwer" "zxcv" "hjkl"))))
-->

```diagram
<!-- expand: TABLE-SAMPLE -->
```
Figure. テーブルのサンプル


　上記のサンプルは以下のコードで生成しています。

```lisp
<!-- expand: TABLE-SAMPLE -->
```

　`table` のパラメータ構成は以下のようになっています。


```lisp
(defmacro table (center rows cols &key stroke fills font texts layer id) ...)
```

　それぞれのパラメータについて以下に説明します。

<!-- stack:push tr style="font-size: 14;" -->

Table. table のパラメータ
| parameter | description                              |
|:=========:|:-----------------------------------------|
| center    | 作成するテーブルの中心座標を指定します。 |
| rows      | 行数と各行の高さを数値のリストで指定します。リストの長さが行数、リスト要素の数値が行の高さです。 |
| cols      | 列数と各列の幅を数値のリストで指定します。リストの長さが列数、リスト要素の数値が列の幅です。 |
| stroke    | 罫線の描画方法をストローク情報で指定します。省略可能で、省略した場合はデフォルトのストローク情報が使用 \
されます。 |
| fills     | 表、行、列、またはセル個別の背景色を指定します。位置を示すキーワードとフィル情報の２つの値を繰り返す \
リストで指定してください。位置は、表全体であれば `:rc` 、列や行全体を指定する場合は `:rN` や `:cM` を指定します。 \
ここで、 `N,M` は行や列の番号を示す整数です（上または左から０で始まります）。単独のセルを指定する場合、同じ要領で  \
`:rNcM` と指定してください。 `fills` パラメータ全体が省略された場合、表の背景は塗り潰されません。 |
| font      | 表内でテキストを描画する際に使用するフォントを指定します。省略できます。省略した場合、  \
`*default-table-font*, *default-font*` の順でデフォルトフォントが使用されます。また、 `texts` パラメータ \
指定の中でセル毎に個別にフォントを指定することもできます。 |
| texts     | 表内の各セルに設定するテキストをリストで指定します。正確には、行のリストを連ねたリストで指定します。 \
テキスト情報はキーワードなどのシンボル、数値、文字列を指定できますが、アライメントやフォント情報を指定する場合は  \
テキスト情報自体をリストにする必要があります。詳細は後述します。 |
| layer     | テーブルが所属するレイヤーをキーワードで指定します。省略できます。  |
| id        | テーブルの ID をキーワードで指定します。省略できます。  |

<!-- stack:pop tr -->

${BLANK_PARAGRAPH}

　`texts` パラメータについて説明します。まず、 `texts` パラメータそのものを省略した場合、すべてのセルにおいて
テキストは指定されなかったものとして扱われます。指定する場合、典型的には前述の例のように「リストのリスト」として
指定することになります。

```lisp
  :texts '((:foo :bar :baz :quux)
           (5 6 42 -123)
           ("asdf" "qwer" "zxcv" "hjkl"))
```

　この例では 3 行 x 4 列全てのセルにテキストを指定していますが、空のままにしておきたいセルには `nil` を指定
してください。見ての通り、 `:texts` といっても数値やシンボルも指定することができます。ただし、改行を含む
複数行のテキストを表示させることはできません。それが必要な場合は、後述する [$$](#with-table-cell を使ったセル内描画)
を利用してください。

　表示させるテキストのフォント情報やアライメントを指定したい場合、個々のデータ自体をリストで指定する必要が
あります。

* データはリストの先頭要素として指定します。
* 水平方向のアライメントは、 `:align` に続けて `:left, :center, :right` のいずれかを指定します。これを省略した場合、  \
`data` が数値であれば右寄せ、文字列であれば左寄せ、キーワードなどのシンボルであれば中央揃えになります。
* 垂直方向のアライメントは、 `:valign` に続けて `:top, :center, :bottom` のいずれかを指定します。デフォルトで  \
`:center` 指定になります
* フォントは、 `:font` に続けてフォント情報を指定します。これを省略した場合、その時点でのデフォルトフォントが使用 \
されます。

　`:align` および `:valign` のサンプルを以下に示します。

<!-- snippet: TABLE-ALIGN-SAMPLE
(diagram (480 240)
  (with-options (:font '(:size 12))
    (table '(240 120) '(40 60 60 60) '(100 120 120 120)
           :stroke :navy :id :tbl
           :fills '(:rc :white :r0 :skyblue :c0 :skyblue)
           :texts `((nil        :|:left|   :|:center|   :|:right|)
                    (:|:top|    ("top-left"      :align :left   :valign :top)
                                ("top-center"    :align :center :valign :top)
                                ("top-right"     :align :right  :valign :top))
                    (:|:center| ("center-left"   :align :left   :valign :center)
                                ("center-center" :align :center :valign :center)
                                ("center-right"  :align :right  :valign :center))
                    (:|:bottom| ("botom-left"    :align :left   :valign :bottom)
                                ("bottom-center" :align :center :valign :bottom)
                                ("bottom-right"  :align :right  :valign :bottom))))
    (with-table-cell (:tbl 0 0)
      (text (xy+ canvas.center -1 -2)  ":align" :align :left)
      (text (xy+ canvas.left    3 15) ":valign" :align :left)
      (let ((w canvas.width)
           (h canvas.height))
        (line `((0 0) (,w ,h)) :stroke :navy)))))
-->

```diagram
<!-- expand: TABLE-ALIGN-SAMPLE -->
```
Figure. テーブルにおけるテキストの align と valign パラメータ

<!-- collapse:begin -->
　※上記サンプルのソースはこちら。

```lisp
<!-- expand: TABLE-ALIGN-SAMPLE -->
```
<!-- collapse:end -->

　データを（リストでなく）直接指定した場合でも、数値ならば右寄せ、文字列ならば左寄せ、シンボルならば中央揃えという
デフォルトの設定は行なわれるため、多くの場合で個別セルのテキストデータ指定をリストで行なう必要は無いでしょう。
キーワードを使うと全て大文字で表示されてしまいますが、 `:|Foo|` のようにバーティカルバーで括ると文字の大小を維持
したまま表示されます（[$@](F#テーブルにおけるテキストの align と valign パラメータ) のコードを参照）。

#### with-table-cell を使ったセル内描画

　`table` の id とセルの行・列番号を指定して `with-table-cell` を使用することで、該当するセルを
サブキャンバスとした描画ができます。

```lisp
(defmacro with-table-cell ((id r c) &body body) ...)
```

　以下の例では、2 x 2 の空のテーブルを作成し、そのうちの２つのセル内部に図形を描画しています。

<!-- snippet: WITH-TABLE-CELL-SAMPLE
(diagram (220 220)
    (table '(110 110) '(100 100) '(100 100)
               :stroke :navy :fills '(:rc :white) :id :tbl)
    (with-table-cell (:tbl 1 0)
      (circle canvas.center 30 :fill :lightcyan :stroke :blue))
    (with-table-cell (:tbl 0 1)
      (rect canvas.center 50 50 :fill :lightpink :stroke :red :rotate 45)))
-->

```diagram
<!-- expand: WITH-TABLE-CELL-SAMPLE -->
```
Figure. with-table-cell の使用例


　上記のサンプルは以下のコードで生成されています。

```lisp
<!-- expand: WITH-TABLE-CELL-SAMPLE -->
```

　`with-table-cell` は事実上、指定したテーブルの指定セル領域を指定した `with-subcanvas` として
機能します。そのため、 `canvas` を使ってその中心座標や幅、高さ情報にアクセスできます。

${BLANK_PARAGRAPH}

## 色の指定

　${APPNAME} は SVG 形式で図形を生成するため、色の指定は SVG の規格に準拠します。

* #rrggbb 表記による、6 桁の16進指定。rr、gg、bb は順に赤、緑、青の成分で、 00〜ff の範囲で \
指定します。
* #rgb 表記による、3 桁の16進指定。r、g、b は順に赤、緑、青の成分で、 0〜f の範囲で指定します。 \
これは #rrggbb の簡略表記で、たとえば #136 は #113366 に相当します。
* [$$](#rgb関数)による指定。これは `(rgb r g b)` の要領で使用します。r、g、b は順に赤、緑、青の \
成分で、それぞれ 0〜255 の整数または 0.0〜1.0 の小数点数で指定します。0.0〜1.0 の指定の場合、 \
それに 255 をかけた値が指定されます。
* 色名での指定。 `:black` など先頭にコロンをつけたキーワードの形式で指定します。使用できる色の名前と \
サンプルは [$@ 節](#色の名前) を参照してください。

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

## 回転

　${{TODO}{まだ記述されていません}}

<!-- snippet: ROTATE-SAMPLE
(diagram (400 200)
  (grid)
  (rect '(200 100) 100 70 :fill :lightgray :stroke :black :rotate 30))
-->

```lisp
<!-- expand: ROTATE-SAMPLE -->
```

${BLANK_PARAGRAPH}


　以下のような画像が生成されます。

```diagram
<!-- expand: ROTATE-SAMPLE -->
```
Figure. 回転のサンプル


## フィルタ

　${{TODO}{まだ記述されていません}}

<!-- snippet: FILTER-SAMPLE
(diagram (400 200)
  (grid)
  (drop-shadow)
  (glow-shadow :color-matrix '(0 0 0 0   0
                               0 0 0 0.6 0
                               0 0 0 0.3 0
                               0 0 0 0.5 0))
  (rect '(200 100) 100 70 :fill :lightgray :stroke :black :filter :drop-shadow)
  (text '(200 180) "sample text" :align :center
                   :font (make-font :size 24 :fill :cadetblue :filter :glow-shadow)))
-->

```lisp
<!-- expand: FILTER-SAMPLE -->
```

${BLANK_PARAGRAPH}


　以下のような画像が生成されます。

```diagram
<!-- expand: FILTER-SAMPLE -->
```
Figure. フィルタのサンプル

## defs と use

　${{TODO}{まだ記述されていません}}

<!-- snippet: DEFS-USE-SAMPLE
(diagram (400 200)
  (grid)
  (defs (70 50 :frame)
    (rect canvas.center canvas.width canvas.height :fill :white :stroke :black)
    (line '((0 10) (70 10)) :stroke :black))
  (use :frame '(100 70) :id :frame1
       :contents
       ((text (y+ canvas.center 10) "frame 1" :align :center)))
  (use :frame '(300 130) :id :frame2
       :contents
       ((text (y+ canvas.center 10) "frame 2" :align :center)))
  (connect :frame1 :frame2 :end2 :arrow))
-->

```lisp
<!-- expand: DEFS-USE-SAMPLE -->
```

${BLANK_PARAGRAPH}


　以下のような画像が生成されます。

```diagram
<!-- expand: DEFS-USE-SAMPLE -->
```
Figure. defs と use のサンプル

## レイヤー

　${{TODO}{まだ記述されていません。}}


${BLANK_PARAGRAPH}

## IDと参照

　${{TODO}{まだ記述されていません。}}

${BLANK_PARAGRAPH}

## 座標と位置

　${{TODO}{まだ記述されていません。}}

<!-- snippet: GEOMETRY-SAMPLE
(diagram (300 200)
  (grid)
  (text '(10 20) "(0, 0)")
  (circle (xy+ canvas.topleft 2 2) 2 :stroke :red :fill :red)
  (circle canvas.topright          2 :stroke :red :fill :red)
  (circle canvas.bottomleft        2 :stroke :red :fill :red)
  (circle canvas.bottomright       2 :stroke :red :fill :red))
-->

```lisp
<!-- expand: GEOMETRY-SAMPLE -->
```


```diagram
<!-- expand: GEOMETRY-SAMPLE -->
```
Figure. xxxのサンプル


${BLANK_PARAGRAPH}

## リンク

　${{TODO}{まだ記述されていません。}}

${BLANK_PARAGRAPH}

## ラベル

　${{TODO}{まだ記述されていません。}}

${BLANK_PARAGRAPH}

## 終端マーク

　${{TODO}{まだ記述されていません。}}

${BLANK_PARAGRAPH}

## サブキャンバス

　${{TODO}{まだ記述されていません。}}


<!-- snippet: SUBCANVAS-SAMPLE
(diagram (300 200)
  (grid)
  (circle (xy+ canvas.topleft 50 50) 20 :stroke :brown :fill :wheat)
  (with-subcanvas ('(150 50) 100 100)
    (rect canvas.center
          canvas.width canvas.height :stroke :gray :fill :lightgray)
    (circle (xy+ canvas.topleft 50 50) 20 :stroke :brown :fill :wheat)))
-->

```lisp
<!-- expand: SUBCANVAS-SAMPLE -->
```


```diagram
<!-- expand: SUBCANVAS-SAMPLE -->
```
Figure. サブキャンバスのサンプル


${BLANK_PARAGRAPH}

## UML
### アクティビティ図
<!-- autolink: [$$](#アクティビティ図) -->

* uml-action
* uml-action-param
* uml-activity-final
* uml-activity-partitions
* uml-activity-start
* uml-connector
* uml-decision-merge
* uml-expansion-region
* uml-flow
* uml-flow-final
* uml-fork-join
* uml-pin
* uml-signal-receipt
* uml-signal-sending
* uml-time-event
* uml-diagram
* uml-note

### クラス図
<!-- autolink: [$$](#クラス図) -->

* uml-role-info
* uml-multiplicity-info
* uml-keyword-info
* uml-association
* uml-composition
* uml-aggregation
* uml-dependency
* uml-diagram
* uml-generalization
* uml-realization
* uml-note
* uml-interface
* uml-class
* uml-package

### パッケージ図
<!-- autolink: [$$](#パッケージ図) -->

* uml-keyword-info
* uml-dependency
* uml-package
* uml-realization
* uml-diagram
* uml-note

### ステートマシーン図
<!-- autolink: [$$](#ステートマシーン図) -->

* uml-state-begin
* uml-state-end
* uml-state-history
* uml-transition-spec
* uml-transition
* uml-state
* uml-diagram
* uml-note

### ユースケース図
<!-- autolink: [$$](#ユースケース図) -->

* uml-actor
* uml-usecase
* uml-association
* uml-generalization
* uml-diagram
* uml-note

### UML のダイアグラム要素
#### uml-action-param
<!-- autolink: [$$](#uml-action-param) -->

<!-- snippet: UML-ACTION-PARAM-SAMPLE
(diagram (400 200)
  (grid)
  (uml-action canvas.center "action" :id :act1 :width 300 :height 160
    :contents ((uml-action-param "param" :act1 :L  :fill :cornsilk :id :prm1)
               (uml-action-param "param" :act1 :R1 :fill :cornsilk :id :prm2)
               (uml-action act1.center "process" :id :inner-act)))
  (uml-flow :prm1 :inner-act)
  (uml-flow :inner-act :prm2))
-->

```diagram
<!-- expand: UML-ACTION-PARAM-SAMPLE -->
```
Figure. uml-action-param 要素

<!-- collapse:close -->
上記サンプルのコードはこちら。
```lisp
<!-- expand: UML-ACTION-PARAM-SAMPLE -->
```
<!-- collapse:end -->

```lisp
(defmacro uml-action-param (name target position
                            &key keyword width height
                                 margin font fill stroke link layer id) ...)
```

#### uml-action
<!-- autolink: [$$](#uml-action) -->

<!-- snippet: UML-ACTION-SAMPLE
(diagram (500 100)
  (grid)
  (uml-activity-start '( 30 50) :id :start)
  (let ((*uml-action-fill* :cornsilk))
    (uml-action (x+ start.center 150) "action1"             :id :act1)
    (uml-action (x+ act1.center  150) "action~%2nd" :rake t :id :act2))
  (uml-activity-final '(470 50) :id :final)
  (connector :start :act1 :end2 :arrow)
  (connector :act1  :act2 :end2 :arrow)
  (connector :act2 :final :end2 :arrow))
-->

```diagram
<!-- expand: UML-ACTION-SAMPLE -->
```
Figure. uml-action 要素

<!-- collapse:close -->
上記サンプルのコードはこちら。
```lisp
<!-- expand: UML-ACTION-SAMPLE -->
```
<!-- collapse:end -->

```lisp
(defmacro uml-action (center text &key keyword width height
                                       margin corner-r rake
                                       font fill stroke link layer id contents) ...)
```

#### uml-activity-final
<!-- autolink: [$$](#uml-activity-final) -->

#### uml-activity-partitions
<!-- autolink: [$$](#uml-activity-partitions) -->

<!-- snippet: UML-ACTIVITY-PARTITIONS-SAMPLE
(diagram (520 500)
  (grid)
  (let ((*uml-action-fill* :cornsilk))
    (uml-activity-partitions
      '(10 10) '(("Fullfillment" 130) ("Customer Service" 240) ("Finance" 130)) 460
      :lines :min :fill :none :stroke :black
      :contents
      ((("Customer Service")
        (uml-activity-start '(70 30) :id :start)
        (uml-action (y+  $1.center  60) "Receive~%Order" :id :rcv-order)
        (uml-fork-v (y+  $1.center  50) :length 60 :id :fork)
        (uml-action (xy+ $1.center 100 40) "Send~%Invoice" :id :send-invoice)
        (uml-join-v (y+  $2.center 180) :length 60 :id :join)
        (uml-action (y+  $1.center 50) "Close~%Order" :id :close-order)
        (uml-activity-final (y+ $1.center 70) :id :final))
       (("Fullfillment")
        (uml-action (x+ send-invoice.center -240) "Fill Order" :id :fill-order)
        (uml-action (y+ $1.center 70) "Deliver~%Order" :id :deliver-order))
       (("Finance")
        (uml-action (x+ $1.center 380) "Receive~%Payment" :id :rcv-payment))))
    (uml-flow :start         :rcv-order)
    (uml-flow :rcv-order     :fork)
    (uml-flow :fork          :fill-order   :style :B1R)
    (uml-flow :fork          :send-invoice :style :B3L)
    (uml-flow :send-invoice  :rcv-payment  :style :RT)
    (uml-flow :fill-order    :deliver-order)
    (uml-flow :deliver-order :join         :style :BT1)
    (uml-flow :rcv-payment   :join         :style :BT3)
    (uml-flow :join          :close-order)
    (uml-flow :close-order   :final)))
-->

```diagram
<!-- expand: UML-ACTIVITY-PARTITIONS-SAMPLE -->
```
Figure. uml-activity-partitions 要素

<!-- collapse:close -->
上記サンプルのコードはこちら。
```lisp
<!-- expand: UML-ACTIVITY-PARTITIONS-SAMPLE -->
```
<!-- collapse:end -->

```lisp
(defmacro uml-activity-partitions (topleft vertical horizontal
                                   &key lines margin font
                                        fill stroke layer contents) ...)
```

#### uml-activity-start
<!-- autolink: [$$](#uml-activity-start) -->

#### uml-actor
<!-- autolink: [$$](#uml-actor) -->

#### uml-aggregation
<!-- autolink: [$$](#uml-aggregation) -->

#### uml-association
<!-- autolink: [$$](#uml-association) -->

#### uml-class
<!-- autolink: [$$](#uml-class) -->

#### uml-component
<!-- autolink: [$$](#uml-component) -->

#### uml-composition
<!-- autolink: [$$](#uml-composition) -->

#### uml-connection-common
<!-- autolink: [$$](#uml-connection-common) -->

#### uml-connector
<!-- autolink: [$$](#uml-connector) -->

#### uml-decision-merge
<!-- autolink: [$$](#uml-decision-merge) -->

#### uml-dependency
<!-- autolink: [$$](#uml-dependency) -->

#### uml-diagram
<!-- autolink: [$$](#uml-diagram) -->

#### uml-expansion-region
<!-- autolink: [$$](#uml-expansion-region) -->

#### uml-flow-final
<!-- autolink: [$$](#uml-flow-final) -->

#### uml-flow
<!-- autolink: [$$](#uml-flow) -->

#### uml-fork-join
<!-- autolink: [$$](#uml-fork-join) -->

#### uml-generalization
<!-- autolink: [$$](#uml-generalization) -->

#### uml-interface
<!-- autolink: [$$](#uml-interface) -->

#### uml-keyword-info
<!-- autolink: [$$](#uml-keyword-info) -->

#### uml-multiplicity-info
<!-- autolink: [$$](#uml-multiplicity-info) -->

#### uml-node
<!-- autolink: [$$](#uml-node) -->

#### uml-note
<!-- autolink: [$$](#uml-note) -->

#### uml-package
<!-- autolink: [$$](#uml-package) -->

#### uml-pin
<!-- autolink: [$$](#uml-pin) -->

#### uml-realization
<!-- autolink: [$$](#uml-realization) -->

#### uml-role-info
<!-- autolink: [$$](#uml-role-info) -->

#### uml-signal-receipt
<!-- autolink: [$$](#uml-signal-receipt) -->

#### uml-signal-sending
<!-- autolink: [$$](#uml-signal-sending) -->

#### uml-state-begin
<!-- autolink: [$$](#uml-state-begin) -->

#### uml-state-end
<!-- autolink: [$$](#uml-state-end) -->

#### uml-state-history
<!-- autolink: [$$](#uml-state-history) -->

#### uml-state
<!-- autolink: [$$](#uml-state) -->

#### uml-time-event
<!-- autolink: [$$](#uml-time-event) -->

#### uml-transition-spec
<!-- autolink: [$$](#uml-transition-spec) -->

#### uml-transition
<!-- autolink: [$$](#uml-transition) -->

#### uml-usecase
<!-- autolink: [$$](#uml-usecase) -->

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
(diagram (400 100)
  (text '(200 55) "this is test text." :align :center)
  (with-options (:fill :red)
    (rect '(150 50) 30 30 :fill (make-fill :opacity 0.2))
    (text '(150 80) "0.2" :align :center)
    (rect '(200 50) 30 30 :fill (make-fill :opacity 0.5))
    (text '(200 80) "0.5" :align :center)
    (rect '(250 50) 30 30 :fill (make-fill :opacity 0.8))
    (text '(250 80) "0.8" :align :center)))
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
(diagram (400 120)
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
(diagram (400 100)
	(with-subcanvas ('(0 0) 100 100)
	  (with-options (:stroke '(:color :black :width 4 :dasharray '(8 4)))
		(line '((30 20) (70 20)))
		(line '((30 40) (70 40)) :stroke '(:dashoffset 2))
		(line '((30 60) (70 60)) :stroke '(:dashoffset 4))
		(line '((30 80) (70 80)) :stroke '(:dashoffset 6))))
	(with-subcanvas ('(100 0) 100 100)
	  (with-options (:stroke '(:color :black :width 8))
		(line '((30 20) (70 20)))
		(line '((30 40) (70 40)) :stroke '(:linecap   :butt))
		(line '((30 60) (70 60)) :stroke '(:linecap  :round))
		(line '((30 80) (70 80)) :stroke '(:linecap :square))))
	(with-subcanvas ('(200 0) 100 100)
	  (with-options (:stroke '(:color :black :width 12))
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

 [$@ 節](#色の指定)ではいくつかの色の指定方法を紹介しています。ここでは、そのうち色の名前について
使用できるものを紹介します。まず、以下は SVG 規格に記載されている色名のサンプルです{{fn:SVG 2 の規格において \
色名に関する節は削除され、CSS 規格を参照することになりました。}}。

<!-- snippet: COLOR-NAME-SAMPLE
(let ((svg-width   750)
      (svg-height 1000))
  (diagram (svg-width svg-height :fill :white)
    (let ((*default-fill*   (make-fill :color :white))
          (*default-font*   (make-font :family "monospace" :size 10 :width-spice 0.85))
          (*default-stroke* (make-stroke :color :black :width 1)))
      (let ((x 15)
            (y  0))
        (labels ((imp (lst)
                   (incf y 20)
                   (when (< svg-height (+ y 10))
                     (incf x 250)
                     (setf y  20))
                   (rect (list x y)  15  15 :fill (fourth lst))
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

${BLANK_PARAGRAPH}

　${APPNAME} では、上記に加えて以下の名前も使用することができます。これらは規格に含まれていない
ため、 `#RRGGBB` 形式に変換されて出力されます{{fn:[$@](F#色の名前とサンプル - 2) に記載されているのは、Emacs  \
エディタで定義されている色名のうち、規格に含まれないものの一覧です。このうち、green(#00FF00), grey(#BEBEBE),  \
maroon(#B03060), purple(#A020F0) については規格側にも同じ名前があるものの異なる値です。これらは「規格優先」 \
としているため、たとえば `:green` を指定した場合は規格の #008000 が使用され、#00FF00 は使用されません。}}。


<!-- snippet: EXTERNAL-COLOR-NAME-SAMPLE
(let ((svg-width   760)
      (svg-height 2140))
  (diagram (svg-width svg-height :fill :white)
    (let ((*default-fill*   (make-fill :color :white))
          (*default-font*   (make-font :family "monospace" :size 10 :width-spice 0.85))
          (*default-stroke* (make-stroke :color :black :width 1)))
      (let ((x 15)
            (y  0))
        (labels ((imp (lst)
                   (incf y 20)
                   (when (< svg-height (+ y 10))
                     (incf x 190)
                     (setf y  20))
                   (rect (list x y)  15  15
                         :fill (format nil "#~2,'0x~2,'0x~2,'0x"
                                       (first lst) (second lst) (third lst)))
                   (text (list (+ x 10)  (+ y 4))
                         (apply #'format  nil "#~2,'0x~2,'0x~2,'0x:~A" lst))))
          (mapcar #'imp '((255 239 219 "antiquewhite1"   )
                          (238 223 204 "antiquewhite2"   )
                          (205 192 176 "antiquewhite3"   )
                          (139 131 120 "antiquewhite4"   )
                          (127 255 212 "aquamarine1"     )
                          (118 238 198 "aquamarine2"     )
                          (102 205 170 "aquamarine3"     )
                          ( 69 139 116 "aquamarine4"     )
                          (240 255 255 "azure1"          )
                          (224 238 238 "azure2"          )
                          (193 205 205 "azure3"          )
                          (131 139 139 "azure4"          )
                          (255 228 196 "bisque1"         )
                          (238 213 183 "bisque2"         )
                          (205 183 158 "bisque3"         )
                          (139 125 107 "bisque4"         )
                          (  0   0 255 "blue1"           )
                          (  0   0 238 "blue2"           )
                          (  0   0 205 "blue3"           )
                          (  0   0 139 "blue4"           )
                          (255  64  64 "brown1"          )
                          (238  59  59 "brown2"          )
                          (205  51  51 "brown3"          )
                          (139  35  35 "brown4"          )
                          (255 211 155 "burlywood1"      )
                          (238 197 145 "burlywood2"      )
                          (205 170 125 "burlywood3"      )
                          (139 115  85 "burlywood4"      )
                          (152 245 255 "cadetblue1"      )
                          (142 229 238 "cadetblue2"      )
                          (122 197 205 "cadetblue3"      )
                          ( 83 134 139 "cadetblue4"      )
                          (127 255   0 "chartreuse1"     )
                          (118 238   0 "chartreuse2"     )
                          (102 205   0 "chartreuse3"     )
                          ( 69 139   0 "chartreuse4"     )
                          (255 127  36 "chocolate1"      )
                          (238 118  33 "chocolate2"      )
                          (205 102  29 "chocolate3"      )
                          (139  69  19 "chocolate4"      )
                          (255 114  86 "coral1"          )
                          (238 106  80 "coral2"          )
                          (205  91  69 "coral3"          )
                          (139  62  47 "coral4"          )
                          (255 248 220 "cornsilk1"       )
                          (238 232 205 "cornsilk2"       )
                          (205 200 177 "cornsilk3"       )
                          (139 136 120 "cornsilk4"       )
                          (  0 255 255 "cyan1"           )
                          (  0 238 238 "cyan2"           )
                          (  0 205 205 "cyan3"           )
                          (  0 139 139 "cyan4"           )
                          (255 185  15 "darkgoldenrod1"  )
                          (238 173  14 "darkgoldenrod2"  )
                          (205 149  12 "darkgoldenrod3"  )
                          (139 101   8 "darkgoldenrod4"  )
                          (202 255 112 "darkolivegreen1" )
                          (188 238 104 "darkolivegreen2" )
                          (162 205  90 "darkolivegreen3" )
                          (110 139  61 "darkolivegreen4" )
                          (255 127   0 "darkorange1"     )
                          (238 118   0 "darkorange2"     )
                          (205 102   0 "darkorange3"     )
                          (139  69   0 "darkorange4"     )
                          (191  62 255 "darkorchid1"     )
                          (178  58 238 "darkorchid2"     )
                          (154  50 205 "darkorchid3"     )
                          (104  34 139 "darkorchid4"     )
                          (193 255 193 "darkseagreen1"   )
                          (180 238 180 "darkseagreen2"   )
                          (155 205 155 "darkseagreen3"   )
                          (105 139 105 "darkseagreen4"   )
                          (151 255 255 "darkslategray1"  )
                          (141 238 238 "darkslategray2"  )
                          (121 205 205 "darkslategray3"  )
                          ( 82 139 139 "darkslategray4"  )
                          (255  20 147 "deeppink1"       )
                          (238  18 137 "deeppink2"       )
                          (205  16 118 "deeppink3"       )
                          (139  10  80 "deeppink4"       )
                          (  0 191 255 "deepskyblue1"    )
                          (  0 178 238 "deepskyblue2"    )
                          (  0 154 205 "deepskyblue3"    )
                          (  0 104 139 "deepskyblue4"    )
                          ( 30 144 255 "dodgerblue1"     )
                          ( 28 134 238 "dodgerblue2"     )
                          ( 24 116 205 "dodgerblue3"     )
                          ( 16  78 139 "dodgerblue4"     )
                          (255  48  48 "firebrick1"      )
                          (238  44  44 "firebrick2"      )
                          (205  38  38 "firebrick3"      )
                          (139  26  26 "firebrick4"      )
                          (255 215   0 "gold1"           )
                          (238 201   0 "gold2"           )
                          (205 173   0 "gold3"           )
                          (139 117   0 "gold4"           )
                          (255 193  37 "goldenrod1"      )
                          (238 180  34 "goldenrod2"      )
                          (205 155  29 "goldenrod3"      )
                          (139 105  20 "goldenrod4"      )
                          (  0 255   0 "green"           )	; duplicated
                          (  0 255   0 "green1"          )
                          (  0 238   0 "green2"          )
                          (  0 205   0 "green3"          )
                          (  0 139   0 "green4"          )
                          (190 190 190 "grey"            )	; duplicated
                          (  0   0   0 "grey0"           )
                          (  3   3   3 "grey1"           )
                          (  5   5   5 "grey2"           )
                          (  8   8   8 "grey3"           )
                          ( 10  10  10 "grey4"           )
                          ( 13  13  13 "grey5"           )
                          ( 15  15  15 "grey6"           )
                          ( 18  18  18 "grey7"           )
                          ( 20  20  20 "grey8"           )
                          ( 23  23  23 "grey9"           )
                          ( 26  26  26 "grey10"          )
                          ( 28  28  28 "grey11"          )
                          ( 31  31  31 "grey12"          )
                          ( 33  33  33 "grey13"          )
                          ( 36  36  36 "grey14"          )
                          ( 38  38  38 "grey15"          )
                          ( 41  41  41 "grey16"          )
                          ( 43  43  43 "grey17"          )
                          ( 46  46  46 "grey18"          )
                          ( 48  48  48 "grey19"          )
                          ( 51  51  51 "grey20"          )
                          ( 54  54  54 "grey21"          )
                          ( 56  56  56 "grey22"          )
                          ( 59  59  59 "grey23"          )
                          ( 61  61  61 "grey24"          )
                          ( 64  64  64 "grey25"          )
                          ( 66  66  66 "grey26"          )
                          ( 69  69  69 "grey27"          )
                          ( 71  71  71 "grey28"          )
                          ( 74  74  74 "grey29"          )
                          ( 77  77  77 "grey30"          )
                          ( 79  79  79 "grey31"          )
                          ( 82  82  82 "grey32"          )
                          ( 84  84  84 "grey33"          )
                          ( 87  87  87 "grey34"          )
                          ( 89  89  89 "grey35"          )
                          ( 92  92  92 "grey36"          )
                          ( 94  94  94 "grey37"          )
                          ( 97  97  97 "grey38"          )
                          ( 99  99  99 "grey39"          )
                          (102 102 102 "grey40"          )
                          (105 105 105 "grey41"          )
                          (107 107 107 "grey42"          )
                          (110 110 110 "grey43"          )
                          (112 112 112 "grey44"          )
                          (115 115 115 "grey45"          )
                          (117 117 117 "grey46"          )
                          (120 120 120 "grey47"          )
                          (122 122 122 "grey48"          )
                          (125 125 125 "grey49"          )
                          (127 127 127 "grey50"          )
                          (130 130 130 "grey51"          )
                          (133 133 133 "grey52"          )
                          (135 135 135 "grey53"          )
                          (138 138 138 "grey54"          )
                          (140 140 140 "grey55"          )
                          (143 143 143 "grey56"          )
                          (145 145 145 "grey57"          )
                          (148 148 148 "grey58"          )
                          (150 150 150 "grey59"          )
                          (153 153 153 "grey60"          )
                          (156 156 156 "grey61"          )
                          (158 158 158 "grey62"          )
                          (161 161 161 "grey63"          )
                          (163 163 163 "grey64"          )
                          (166 166 166 "grey65"          )
                          (168 168 168 "grey66"          )
                          (171 171 171 "grey67"          )
                          (173 173 173 "grey68"          )
                          (176 176 176 "grey69"          )
                          (179 179 179 "grey70"          )
                          (181 181 181 "grey71"          )
                          (184 184 184 "grey72"          )
                          (186 186 186 "grey73"          )
                          (189 189 189 "grey74"          )
                          (191 191 191 "grey75"          )
                          (194 194 194 "grey76"          )
                          (196 196 196 "grey77"          )
                          (199 199 199 "grey78"          )
                          (201 201 201 "grey79"          )
                          (204 204 204 "grey80"          )
                          (207 207 207 "grey81"          )
                          (209 209 209 "grey82"          )
                          (212 212 212 "grey83"          )
                          (214 214 214 "grey84"          )
                          (217 217 217 "grey85"          )
                          (219 219 219 "grey86"          )
                          (222 222 222 "grey87"          )
                          (224 224 224 "grey88"          )
                          (227 227 227 "grey89"          )
                          (229 229 229 "grey90"          )
                          (232 232 232 "grey91"          )
                          (235 235 235 "grey92"          )
                          (237 237 237 "grey93"          )
                          (240 240 240 "grey94"          )
                          (242 242 242 "grey95"          )
                          (245 245 245 "grey96"          )
                          (247 247 247 "grey97"          )
                          (250 250 250 "grey98"          )
                          (252 252 252 "grey99"          )
                          (255 255 255 "grey100"         )
                          (240 255 240 "honeydew1"       )
                          (224 238 224 "honeydew2"       )
                          (193 205 193 "honeydew3"       )
                          (131 139 131 "honeydew4"       )
                          (255 110 180 "hotpink1"        )
                          (238 106 167 "hotpink2"        )
                          (205  96 144 "hotpink3"        )
                          (139  58  98 "hotpink4"        )
                          (255 106 106 "indianred1"      )
                          (238  99  99 "indianred2"      )
                          (205  85  85 "indianred3"      )
                          (139  58  58 "indianred4"      )
                          (255 255 240 "ivory1"          )
                          (238 238 224 "ivory2"          )
                          (205 205 193 "ivory3"          )
                          (139 139 131 "ivory4"          )
                          (255 246 143 "khaki1"          )
                          (238 230 133 "khaki2"          )
                          (205 198 115 "khaki3"          )
                          (139 134  78 "khaki4"          )
                          (255 240 245 "lavenderblush1"  )
                          (238 224 229 "lavenderblush2"  )
                          (205 193 197 "lavenderblush3"  )
                          (139 131 134 "lavenderblush4"  )
                          (255 250 205 "lemonchiffon1"   )
                          (238 233 191 "lemonchiffon2"   )
                          (205 201 165 "lemonchiffon3"   )
                          (139 137 112 "lemonchiffon4"   )
                          (191 239 255 "lightblue1"      )
                          (178 223 238 "lightblue2"      )
                          (154 192 205 "lightblue3"      )
                          (104 131 139 "lightblue4"      )
                          (224 255 255 "lightcyan1"      )
                          (209 238 238 "lightcyan2"      )
                          (180 205 205 "lightcyan3"      )
                          (122 139 139 "lightcyan4"      )
                          (238 221 130 "lightgoldenrod"  )
                          (255 236 139 "lightgoldenrod1" )
                          (238 220 130 "lightgoldenrod2" )
                          (205 190 112 "lightgoldenrod3" )
                          (139 129  76 "lightgoldenrod4" )
                          (255 174 185 "lightpink1"      )
                          (238 162 173 "lightpink2"      )
                          (205 140 149 "lightpink3"      )
                          (139  95 101 "lightpink4"      )
                          (255 160 122 "lightsalmon1"    )
                          (238 149 114 "lightsalmon2"    )
                          (205 129  98 "lightsalmon3"    )
                          (139  87  66 "lightsalmon4"    )
                          (176 226 255 "lightskyblue1"   )
                          (164 211 238 "lightskyblue2"   )
                          (141 182 205 "lightskyblue3"   )
                          ( 96 123 139 "lightskyblue4"   )
                          (132 112 255 "lightslateblue"  )
                          (202 225 255 "lightsteelblue1" )
                          (188 210 238 "lightsteelblue2" )
                          (162 181 205 "lightsteelblue3" )
                          (110 123 139 "lightsteelblue4" )
                          (255 255 224 "lightyellow1"    )
                          (238 238 209 "lightyellow2"    )
                          (205 205 180 "lightyellow3"    )
                          (139 139 122 "lightyellow4"    )
                          (255   0 255 "magenta1"        )
                          (238   0 238 "magenta2"        )
                          (205   0 205 "magenta3"        )
                          (139   0 139 "magenta4"        )
                          (176  48  96 "maroon"          )	; duplicated
                          (255  52 179 "maroon1"         )
                          (238  48 167 "maroon2"         )
                          (205  41 144 "maroon3"         )
                          (139  28  98 "maroon4"         )
                          (224 102 255 "mediumorchid1"   )
                          (209  95 238 "mediumorchid2"   )
                          (180  82 205 "mediumorchid3"   )
                          (122  55 139 "mediumorchid4"   )
                          (171 130 255 "mediumpurple1"   )
                          (159 121 238 "mediumpurple2"   )
                          (137 104 205 "mediumpurple3"   )
                          ( 93  71 139 "mediumpurple4"   )
                          (255 228 225 "mistyrose1"      )
                          (238 213 210 "mistyrose2"      )
                          (205 183 181 "mistyrose3"      )
                          (139 125 123 "mistyrose4"      )
                          (255 222 173 "navajowhite1"    )
                          (238 207 161 "navajowhite2"    )
                          (205 179 139 "navajowhite3"    )
                          (139 121  94 "navajowhite4"    )
                          (  0   0 128 "navyblue"        )
                          (192 255  62 "olivedrab1"      )
                          (179 238  58 "olivedrab2"      )
                          (154 205  50 "olivedrab3"      )
                          (105 139  34 "olivedrab4"      )
                          (255 165   0 "orange1"         )
                          (238 154   0 "orange2"         )
                          (205 133   0 "orange3"         )
                          (139  90   0 "orange4"         )
                          (255  69   0 "orangered1"      )
                          (238  64   0 "orangered2"      )
                          (205  55   0 "orangered3"      )
                          (139  37   0 "orangered4"      )
                          (255 131 250 "orchid1"         )
                          (238 122 233 "orchid2"         )
                          (205 105 201 "orchid3"         )
                          (139  71 137 "orchid4"         )
                          (154 255 154 "palegreen1"      )
                          (144 238 144 "palegreen2"      )
                          (124 205 124 "palegreen3"      )
                          ( 84 139  84 "palegreen4"      )
                          (187 255 255 "paleturquoise1"  )
                          (174 238 238 "paleturquoise2"  )
                          (150 205 205 "paleturquoise3"  )
                          (102 139 139 "paleturquoise4"  )
                          (255 130 171 "palevioletred1"  )
                          (238 121 159 "palevioletred2"  )
                          (205 104 137 "palevioletred3"  )
                          (139  71  93 "palevioletred4"  )
                          (255 218 185 "peachpuff1"      )
                          (238 203 173 "peachpuff2"      )
                          (205 175 149 "peachpuff3"      )
                          (139 119 101 "peachpuff4"      )
                          (255 181 197 "pink1"           )
                          (238 169 184 "pink2"           )
                          (205 145 158 "pink3"           )
                          (139  99 108 "pink4"           )
                          (255 187 255 "plum1"           )
                          (238 174 238 "plum2"           )
                          (205 150 205 "plum3"           )
                          (139 102 139 "plum4"           )
                          (160  32 240 "purple"          )	; duplicated
                          (155  48 255 "purple1"         )
                          (145  44 238 "purple2"         )
                          (125  38 205 "purple3"         )
                          ( 85  26 139 "purple4"         )
                          (255   0   0 "red1"            )
                          (238   0   0 "red2"            )
                          (205   0   0 "red3"            )
                          (139   0   0 "red4"            )
                          (255 193 193 "rosybrown1"      )
                          (238 180 180 "rosybrown2"      )
                          (205 155 155 "rosybrown3"      )
                          (139 105 105 "rosybrown4"      )
                          ( 72 118 255 "royalblue1"      )
                          ( 67 110 238 "royalblue2"      )
                          ( 58  95 205 "royalblue3"      )
                          ( 39  64 139 "royalblue4"      )
                          (255 140 105 "salmon1"         )
                          (238 130  98 "salmon2"         )
                          (205 112  84 "salmon3"         )
                          (139  76  57 "salmon4"         )
                          ( 84 255 159 "seagreen1"       )
                          ( 78 238 148 "seagreen2"       )
                          ( 67 205 128 "seagreen3"       )
                          ( 46 139  87 "seagreen4"       )
                          (255 245 238 "seashell1"       )
                          (238 229 222 "seashell2"       )
                          (205 197 191 "seashell3"       )
                          (139 134 130 "seashell4"       )
                          (255 130  71 "sienna1"         )
                          (238 121  66 "sienna2"         )
                          (205 104  57 "sienna3"         )
                          (139  71  38 "sienna4"         )
                          (135 206 255 "skyblue1"        )
                          (126 192 238 "skyblue2"        )
                          (108 166 205 "skyblue3"        )
                          ( 74 112 139 "skyblue4"        )
                          (131 111 255 "slateblue1"      )
                          (122 103 238 "slateblue2"      )
                          (105  89 205 "slateblue3"      )
                          ( 71  60 139 "slateblue4"      )
                          (198 226 255 "slategray1"      )
                          (185 211 238 "slategray2"      )
                          (159 182 205 "slategray3"      )
                          (108 123 139 "slategray4"      )
                          (255 250 250 "snow1"           )
                          (238 233 233 "snow2"           )
                          (205 201 201 "snow3"           )
                          (139 137 137 "snow4"           )
                          (  0 255 127 "springgreen1"    )
                          (  0 238 118 "springgreen2"    )
                          (  0 205 102 "springgreen3"    )
                          (  0 139  69 "springgreen4"    )
                          ( 99 184 255 "steelblue1"      )
                          ( 92 172 238 "steelblue2"      )
                          ( 79 148 205 "steelblue3"      )
                          ( 54 100 139 "steelblue4"      )
                          (255 165  79 "tan1"            )
                          (238 154  73 "tan2"            )
                          (205 133  63 "tan3"            )
                          (139  90  43 "tan4"            )
                          (255 225 255 "thistle1"        )
                          (238 210 238 "thistle2"        )
                          (205 181 205 "thistle3"        )
                          (139 123 139 "thistle4"        )
                          (255  99  71 "tomato1"         )
                          (238  92  66 "tomato2"         )
                          (205  79  57 "tomato3"         )
                          (139  54  38 "tomato4"         )
                          (  0 245 255 "turquoise1"      )
                          (  0 229 238 "turquoise2"      )
                          (  0 197 205 "turquoise3"      )
                          (  0 134 139 "turquoise4"      )
                          (208  32 144 "violetred"       )
                          (255  62 150 "violetred1"      )
                          (238  58 140 "violetred2"      )
                          (205  50 120 "violetred3"      )
                          (139  34  82 "violetred4"      )
                          (255 231 186 "wheat1"          )
                          (238 216 174 "wheat2"          )
                          (205 186 150 "wheat3"          )
                          (139 126 102 "wheat4"          )
                          (255 255   0 "yellow1"         )
                          (238 238   0 "yellow2"         )
                          (205 205   0 "yellow3"         )
                          (139 139   0 "yellow4"         ))))))))
-->

```diagram
<!-- expand: EXTERNAL-COLOR-NAME-SAMPLE -->
```
Figure. 色の名前とサンプル - 2

## 更新履歴

　更新履歴です。

## 図表一覧
<!-- embed:figure-list -->

　　

<!-- embed:table-list -->

　　

## 索引
<!-- embed:index -->

--------------------------------------------------------------------------------

<!-- embed:footnotes -->

