<!-- title:kaavio readme -->
<!-- style:./default.css -->

<!-- config:write-comment -->
<!-- config:header-numbering 2 5 -->
<!-- config:entity-numbering-depth 1 -->
<!-- <!-- config:term-link-in-header -->

<!-- filter:kaavio   = bash ./kaavio.sh   %in %out -->
<!-- filter:plantuml = bash ./plantuml.sh %in %out -->

<!-- define: BLANK_PARAGRAPH = '　　' -->
<!-- define: TODO = '@((background:red;color:white;)(ToDo : %1))' -->
<!-- define: B = '@((font-weight:bold;)(%1))' -->
<!-- define: I = '@((font-style:oblique;)(%1))' -->
<!-- define: SYNTAX = "**Syntax:**" -->
<!-- define: ARGS_AND_VALS = "**Arguments and Values:**" -->
<!-- define: DESCRIPTION = "**Description:**" -->
<!-- define: EXAMPLES = "**Examples:**" -->
<!-- define: SEE_ALSO = "**See Also:**" -->
<!-- define: NO_SEE_ALSO = "**See Also:** None." -->
<!-- define: NOTES = "**Notes:**" -->
<!-- define: NO_NOTES = "**Notes:** None." -->
<!-- define: OPTIONAL = '@((color:navy;)(&optional))' -->
<!-- define: KEY = '@((color:navy;)(&key))' -->
<!-- define: REST = '@((color:navy;)(&rest))' -->
<!-- define: BODY = '@((color:navy;)(&body))' -->


# README - kaavio

　この文書は、 **kaavio** のマニュアル文書です。

<!-- anchor: toc-link-target -->
```raw
<h2>Table of contents</h2>
```
<!-- embed:toc-x 2 3 -->
<!-- toc-link: top 'A#toc-link-target' -->

--------------------------------------------------------------------------------

${BLANK_PARAGRAPH}

## kaavio とは

　kaavio{{fn:正式な名称が決まる前は、diagram という名前でした。kaavio は同じ意味のフィンランドの \
言葉だそうです。この名前にした深い理由はなく、単純にネット検索で埋もれてしまわない名前にしたかっただけです。}} 
は、テキストベースの作図ツールです。テキスト形式のデータファイルを入力として、
SVG 形式{{fn:SVG は Scalable Vector Graphics の略です。}}の画像ファイルを生成します。

　kaavio の入力データの記述方法には違和感を感じるかもしれません。これは、kaavio が 
Common Lisp 言語で実装されており、入力データも Common Lisp 上に作成された DSL(domain specific 
language) で記述するためです。しかし、このツールを使ってみたいからといって、知りもしない言語の
お勉強から始めたいとは思わないでしょう。そのため、このマニュアルではサンプルをたくさん提示し、
Common Lisp の詳細には極力立ち入らないようにします{{fn:もともと LISPer だという方で DSL の詳細を知りたい方は、 \
申し訳ありませんがコードを直接参照してください。}}。

<!-- anchor: SVG-ESSENTIALS-2ND -->
<!-- autolink: [SVG本](A#SVG-ESSENTIALS-2ND) -->

　kaavio は、O'Reilly の「[SVG エッセンシャルズ 第二版](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&cad=rja&uact=8&ved=2ahUKEwjxstvtvKL6AhUNUd4KHbdhCmAQFnoECA0QAQ&url=https%3A%2F%2Fwww.oreilly.co.jp%2Fbooks%2F9784873117973%2F&usg=AOvVaw0qe65qVh3tZyCnBnhoaC-V)」
を読んで作られています（この書籍に言及する時は「SVG本」と呼ぶことにします）。SVG 規格にあたることも
時々ありますが、基本的にほとんどの情報はこの書籍から得ています。電子版もありますので興味のある方は
手に取ってみてください。


### インストール

　kaavio プロジェクトを git clone して、ASDF から `src/kaavio.asd` を利用できる
ように設定してください。kaavio をロードすれば、あとは `kaavio:diagram` マクロで
始まる図面データを実行することで SVG 図面を生成することができます。図面データ作成の詳細に
ついては後述します。

### 実行可能バイナリの作成

　kaavio は実行可能バイナリの作成を想定して作られています。kaavio が Common Lisp 
処理系で使えるように設定されている環境であれば、 `misc/` ディレクトリ配下で `make` 
と言うだけで実行可能バイナリを作成できます。SBCL だけがインストールされている環境であれば、
`make bare-build` と言うことでも実行可能バイナリを作成できます。ただし、これらの方法は現状 
SBCL でしか試しておらず、他の処理系では `misc/` 配下のファイルを少し変更する必要があると
思います。

### sandbox mode
<!-- autolink: [$$](#sandbox mode) -->

　`--sandbox` を指定して実行可能バイナリを起動すれば、sandbox mode を利用することが
できます。

```
$ kaavio --sandbox
IN  : ./sandbox.lisp
OUT : ./sandbox.html

```

　sandbox mode では、kaavio は 0.1 杪間隔で入力ファイル（上記の例では `sandbox.lisp` ）
を監視し続け、ファイルが更新されたらそれを入力として SVG 画像を生成して出力ファイル
（上記の例では `sandbox.html` ）に保存します。この出力ファイルは SVG データを埋め
込んだ HTML 形式ファイルで、ブラウザで開くと 2 杪間隔で自身をリロードするように
なっています。kaavio を sandbox mode で起動し、入力ファイルをテキストエディタで、
出力ファイルをブラウザで開いて並べることで、出力を（ほぼ）リアルタイムで確認しながら
kaavio のデータを編集することができます。

　なお、sandbox mode を終了する場合は Ctrl+C を押下してください。

${BLANK_PARAGRAPH}

<!-- collapse:begin -->
　※Common Lisp 処理系上での sandbox mode についてはこちら。

　上記とは別に、Common Lisp 処理系で kaavio をロードした環境で利用できる sandbox mode も
あります。処理系で kaavio をロードした後、出力 HTML ファイル名を指定して sandbox-start 関数
をコールしてください。これによって、diagram マクロの評価で HTML ファイルを生成するように
なります。この状態を元に戻すには、sandbox-stop 関数をコールしてください。Emacs+SLIME の
ような環境を使っていれば、コード補完を利用しつつ kaavio のデータを編集できるでしょう。

```lisp
* (require :kaavio)
* (kaavio:sandbox-start "~/sandbox.html")
* 
```
<!-- collapse:end -->


## 簡単なサンプル

　簡単なサンプルから始めましょう。以下のような入力を kaavio に与えると、

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

```kaavio
<!-- expand: FIRST-SAMPLE -->
```
Figure. 簡単なサンプル


　「入力を kaavio に与える」というのは、具体的には入力データを記述したファイルの名前を
パラメータとして kaavio を起動することを意味します。作成される SVG 画像は標準出力に
書き出されるので、ファイルにリダイレクトしてください。以下のように。

```sh
kaavio ./input.digram > ./output.svg
```

　入力ファイル名が与えられない場合、kaavio は標準入力からデータを読み取ろうとします。そのため、
以下のように書くこともできます。

```sh
cat ./input.digram | kaavio > ./output.svg
```

　余談ですが、SVG 画像には gzip 圧縮した svgz という形式もあります。以下のように出力を gzip に
通してやれば作成できます。

```sh
cat ./input.digram | kaavio | gzip > ./output.svgz
```

　では、先ほどの入力データをもう一度みてみましょう。

```lisp
<!-- expand: FIRST-SAMPLE -->
```

　初めての kaavio データなので、順番に内容をみていきましょう。まずは雰囲気で理解してください。

* `diagram` で「幅 300、高さ 150」の画像を作成
* `grid` で背景にグリッド線を描画
* `rect` で四角形を作成 - 位置は左上から (50, 50)、大きさは幅 80、高さ 60
    * `:fill` で塗り潰しの色を powderblue に指定
    * `:id` でこれに x という ID を設定
* `circle` で円を作成 - 位置は左上から (250, 100)、半径は 40
    * `:fill` で塗り潰しの色を moccasin に指定
    * `:id` でこれに y という ID を設定
* `connect` で、x から y に向かって接続線を描画
    * `:end2` で終端の形状を arrow に設定

${BLANK_PARAGRAPH}

　では、ここで四角形を描画した行に注目してみましょう。

```lisp
(rect  '(50 50) 80 60 :fill :powderblue :id :x)
```

　全体を括る括弧の中にいくつかのデータが書かれていて、最初は rect で始まっています。この
最初の rect が「四角形を描画せよ」という指示で、残りは何処にどのような四角形を描くかの
指示です。続く `'(50 50) 80 60` は位置（座標）、幅、高さの指定です。 `'(50 50)` という表記
は、位置を即値（具体的な値）で記述する時のお約束だと（今は）理解しておいてください。座標の指定
方法には色々ありますが、のちほど順番に説明します。

　その後ろに続く `:fill :powderblue` や `:id :x` といったものは、全て「名前付きパラメータ
{{fn:Lisper の方へ：要するにキーワードパラメータです。}}」です。kaavio では多くの
パラメータが省略可能で、それらの省略可能パラメータを指定する場合はパラメータ名も書いてあげる
必要があります。なお、 `:fill` のようにコロンで始まる名前は「キーワード」と呼ばれるもので、
省略可能パラメータの名前はキーワードで指定します。

　では、この rect の省略可能なパラメータにはどんなものがあるのでしょうか。以下に rect の
全体を示します。

```lisp
(defmacro rect (position width height
                         &key pivot rx ry fill stroke rotate
                              link layer id filter contents) ...)
```

　先頭の `defmacro` はひとまず気にしないでください。rect に続く括弧の中がパラメータの
全体で、 `position width height` が必須パラメータ、 `&key` 以降の全てが省略可能なパラメータ
です。 `:fill` と `:id` はもう使いました。その他のパラメータについてはまた別のところで説明
します。

${BLANK_PARAGRAPH}

　次のサンプルはもう少し複雑です。

<!-- snippet: SECOND-SAMPLE
(diagram (450 150)
  (grid)
  (drop-shadow)
  (with-options (:filter :drop-shadow)
    (textbox (y+ canvas.center 20) "kaavio" :height 40 :fill :cornsilk :id :app)
    (with-options (:stroke '(:color :navy :width 2)
                   :fill   '(:color :skyblue :opacity 0.3))
      (document (x+ app.center -175) 80 60 "input~%file" :id :in)
      (document (x+ app.center  175) 80 60 "svg~%image"  :id :out))
    (with-options (:fill :white)
      (block-arrow1  in.right app.left 15 :margin 10)
      (block-arrow1 app.right out.left 15 :margin 10)
      (balloon (xy+ app.center 110 -60) "Made with LISP." app.topright))))
-->

```kaavio
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
* with-options でデフォルトのフィルタを drop-shadow に設定
    * textbox でテキストボックスを描画 : 場所は画像の中心（canvas.center）から y 軸方向に 20、 \
テキストは "kaavio"、これに app という ID を設定
    * with-options で、デフォルトの線を太さ 2 の `navy` に、デフォルトの塗りつぶしを不透明度 0.3 の `skyblue` にそれぞれ設定
        * document でドキュメントを描画 : 場所は app の中心（app.center）から x 軸方向に -175、 \
幅と高さは 80 60、テキストは "input~%file"、これに in という ID を設定
        * 上記と同じ要領で out という ID のドキュメントを描画
    * with-options で、デフォルトの塗りつぶしを `white` に設定
        * block-arrow1 で in と app の間にブロック矢印を描画
        * 上記と同じ要領で app と out の間にブロック矢印を描画
        * balloon で app の右上付近に吹き出しを描画 : テキストは "Made with LISP."、接続点は app の \
右上端（app.topright）


${BLANK_PARAGRAPH}

　この 2 つめのサンプルには、新しいポイントがいくつかあります。もう少し詳しく説明します。

* `drop-shadow` で宣言し、 `with-options` でデフォルトを設定しているのを「フィルタ」といいます。 \
四角形や円に表示されている影が drop shadow です。
* `with-options` を使って、デフォルトの塗り潰しや線を指定しています。 `:stroke` や `:fill` を \
毎回指定する必要がなくなります。
    * `:stroke` では線の色 `navy` の他に `:width` で線の太さを指定しています。
    * `:fill` では塗り潰しの色 `skyblue` の他に `:opacity` で不透明度を指定しています。これは、0（完全に透明）から 1  \
（完全に不透明）までを指定します。ドキュメントの塗り潰しが少し透けているのがわかると思います。
* `:id` を使って付与した ID を使って `app.center` などと書くことで既出の要素の「中心座標」を指定できます。 \
これは `'(50 50)` といった即値表記の代わりになります。
    * `canvas` は特別な ID で、現在描画中の「キャンバス」を意味します。今の時点では、生成する画像の四角形全体だと \
理解しておいてください。
    * `(y+ canvas.center 20)` といった表記によってある位置から x 軸や y 軸に指定されただけ移動した座標を計算する \
ことができます。
* 複数行のテキストを扱うことができる要素では、 `"input~%file"` のように ~% を使って改行を表します。

${BLANK_PARAGRAPH}

　サンプルは以上です。雰囲気は掴めたと思うので、続いて各種の図形要素について説明します。

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

```kaavio
<!-- expand: RECTANGLE-SAMPLE -->
```
Figure. rect のサンプル


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

```kaavio
<!-- expand: CIRCLE-SAMPLE -->
```
Figure. circle のサンプル

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

```kaavio
<!-- expand: ELLIPSE-SAMPLE -->
```
Figure. ellipse のサンプル

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

```kaavio
<!-- expand: REGULAR-POLYGON-SAMPLE -->
```
Figure. regular-polygon のサンプル

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

```kaavio
<!-- expand: POLYGON-SAMPLE -->
```
Figure. polygon のサンプル

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

```kaavio
<!-- expand: LINE-SAMPLE -->
```
Figure. line のサンプル

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

```kaavio
<!-- expand: ARC-SAMPLE -->
```
Figure. arc のサンプル


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
Figure. arc のサンプル - 2


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

```kaavio
<!-- expand: ARC-DEBUG-SAMPLE -->
```
Figure. arc における補助線のサンプル


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

```kaavio
(diagram (200 100)
  (grid)
  (ellipse canvas.center 80 30 :stroke '(:color :lightgray :width 8))
  (let ((em (make-endmark :type :triangle :size :small)))
    (arc canvas.center 80 30 0 0 90 :stroke :red :end1 em :end2 em)))
```
Figure. arc における終端マークの例

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

```kaavio
<!-- expand: TEXT-SAMPLE -->
```
Figure. text のサンプル

　上記サンプルのソースは以下の通りです。パラメータの詳細については text マクロを参照
してください。

```lisp
<!-- expand: TEXT-SAMPLE -->
```

${BLANK_PARAGRAPH}

　position と align の関係を以下に示します。以下において、赤い点が position で、
align 指定はテキストで示されています。

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
Figure. テキストの position とアライメント指定の関係

${BLANK_PARAGRAPH}

### ひし形
<!-- autolink: [$$](#ひし形) -->

<!-- snippet: DIAMOND-SAMPLE
(diagram (300 100)
  (grid)
  (diamond '(150 50) 150 60 :stroke '(:color :red3 :width 2) :fill :plum3))
-->

　diamond マクロによってひし形を描画できます。

```kaavio
<!-- expand: DIAMOND-SAMPLE -->
```
Figure. diamond のサンプル

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

```kaavio
<!-- expand: PARALLELOGRAM-SAMPLE -->
```
Figure. parallelogram のサンプル

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
Figure. parallelogram における direction と offset



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

```kaavio
<!-- expand: 2D-CURVE-SAMPLE -->
```
Figure. 2d-curve のサンプル


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

```kaavio
<!-- expand: 2D-CURVE-DEBUG-SAMPLE -->
```
Figure. 2d-curve における補助線のサンプル


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

```kaavio
<!-- expand: 3D-CURVE-SAMPLE -->
```
Figure. 3d-curve のサンプル


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

```kaavio
<!-- expand: 3D-CURVE-DEBUG-SAMPLE -->
```
Figure. 3d-curve における補助線の サンプル


<!-- collapse:begin -->
　※上記サンプルのソースはこちら。

```lisp
<!-- expand: 3D-CURVE-DEBUG-SAMPLE -->
```
<!-- collapse:end -->

${BLANK_PARAGRAPH}

## 一般的な図形

　基本的な図形を組み合わせて作成される、複合的な図形を紹介します。以下のサンプルはそれぞれの
説明項目へのリンクになっています。

<!-- define: HASH_CONNECTOR  = '[](#コネクタ)' -->
<!-- define: HASH_PARAGRAPH  = '[](#パラグラフ)' -->
<!-- define: HASH_TEXTBOX    = '[](#テキストボックス)' -->
<!-- define: HASH_DOCUMENT   = '[](#ドキュメント)' -->
<!-- define: HASH_FOLDER     = '[](#フォルダ)' -->
<!-- define: HASH_BALLOON    = '[](#吹き出し)' -->
<!-- define: HASH_PERSON     = '[](#人物)' -->
<!-- define: HASH_MEMO       = '[](#メモ)' -->
<!-- define: HASH_CUBE       = '[](#キューブ)' -->
<!-- define: HASH_CYLINDER   = '[](#円柱)' -->
<!-- define: HASH_EXPLOSION  = '[](#爆発)' -->
<!-- define: HASH_CROSS      = '[](#十字)' -->
<!-- define: HASH_BLOCKARROW = '[](#ブロック矢印)' -->
<!-- define: HASH_BRACE      = '[](#波括弧)' -->
<!-- define: HASH_TABLE      = '[](#テーブル)' -->

```kaavio
(diagram (800 360)
  ;(grid)
  (let ((w 100)
        (h 100)
        (bgclr :white)) ;; (make-fill :color :lightgray :opacity 0.4 )));; 
    (defgroup (w h :connect-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (rect   '(20 20) 20 20 :fill :white :stroke :black :id :r1)
      (circle '(80 60) 10    :fill :white :stroke :black :id :r2)
      (connect :r1 :r2 :stroke :black)
      (text `(,(/ w 2) ,(- h 5)) "コネクタ" :align :center))
    (defgroup (w h :paragraph-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (paragraph (y+ canvas.center -35) "this is~%multi line~%text." :align :center :font 16)
      (text `(,(/ w 2) ,(- h 5)) "パラグラフ" :align :center))
    (defgroup (w h :textbox-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (textbox (y+ canvas.center -10) "this is~%textbox." :rx 5 :ry 5 :align :center :fill :white)
      (text `(,(/ w 2) ,(- h 5)) "テキストボックス" :align :center))
    (defgroup (w h :document-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (document (y+ canvas.center -10) 80 60 "this is~%document."
                                       :align :center :stroke :navy :fill :skyblue)
      (text `(,(/ w 2) ,(- h 5)) "ドキュメント" :align :center))
    (defgroup (w h :folder-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (folder (y+ canvas.center -5) "this is~%folder."
                                   :align :center :height 50 :stroke :darkkhaki :fill :cornsilk)
      (text `(,(/ w 2) ,(- h 5)) "フォルダ" :align :center))
    (defgroup (w h :person-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (person (y+ canvas.center -10) 35 :fill :oldlace :stroke :brown)
      (text `(,(/ w 2) ,(- h 5)) "人物" :align :center))
    (defgroup (w h :balloon-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (balloon (y+ canvas.center -15) "this is~%balloon." '(10 75)
                                                    :fill :honeydew :stroke :forestgreen)
      (text `(,(/ w 2) ,(- h 5)) "吹き出し" :align :center))
    (defgroup (w h :memo-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (memo (y+ canvas.center -15) "this is~%memo." :width 80 :height 60
                       :valign :top :align :left
                       :stroke :red :fill :lavenderblush :fill2 :lightpink)
      (text `(,(/ w 2) ,(- h 5)) "メモ" :align :center))
    (defgroup (w h :cube-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (cube (y+ canvas.center -10) 65 60 "this is~%cube." 
                                       :stroke :black :fill :lightgray :fill2 :darkgray)
      (text `(,(/ w 2) ,(- h 5)) "キューブ" :align :center))
    (defgroup (w h :cylinder-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (cylinder (y+ canvas.center -10) 65 60 "this is~%cylinder." 
                                       :stroke :darkgray :fill :lightgray)
      (text `(,(/ w 2) ,(- h 5)) "円柱" :align :center))
    (defgroup (w h :explosion-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (explosion1 (y+ canvas.center -10) 90 80 "bomb!!" 
                                       :stroke :red :fill :lightpink)
      (text `(,(/ w 2) ,(- h 5)) "爆発" :align :center))
    (defgroup (w h :cross-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (cross (y+ canvas.center -10) (- canvas.width 30) (- canvas.height 30) 20 
                                       :stroke :purple :fill :plum)
      (text `(,(/ w 2) ,(- h 5)) "十字" :align :center))
    (defgroup (w h :blockarrow-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (block-arrow1 '(0 40) '(100 40) 20 :margin 5 :stroke :brown :fill :burlywood)
      (text `(,(/ w 2) ,(- h 5)) "ブロック矢印" :align :center))
    (defgroup (w h :brace-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (brace (y+ canvas.center -20) :upper 80 30 :r 10 :text "this is brace." :stroke :navy)
      (text `(,(/ w 2) ,(- h 5)) "波括弧" :align :center))
    (defgroup (w h :table-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (table (y+ canvas.center -10) '(10 10 10 10) '(20 20 20 20) :fills '(:rc :white :r0 :skyblue) :stroke :navy)
      (text `(,(/ w 2) ,(- h 5)) "テーブル" :align :center))
    (use :connect-grp    '( 70  60) :link "${HASH_CONNECTOR}")
    (use :paragraph-grp  '(200  60) :link "${HASH_PARAGRAPH}")
    (use :textbox-grp    '(330  60) :link "${HASH_TEXTBOX}")
    (use :document-grp   '(460  60) :link "${HASH_DOCUMENT}")
    (use :folder-grp     '(590  60) :link "${HASH_FOLDER}")
    (use :person-grp     '(720  60) :link "${HASH_PERSON}")
    (use :balloon-grp    '( 70 180) :link "${HASH_BALLOON}")
    (use :memo-grp       '(200 180) :link "${HASH_MEMO}")
    (use :cube-grp       '(330 180) :link "${HASH_CUBE}")
    (use :cylinder-grp   '(460 180) :link "${HASH_CYLINDER}")
    (use :explosion-grp  '(590 180) :link "${HASH_EXPLOSION}")
    (use :cross-grp      '(720 180) :link "${HASH_CROSS}")
    (use :blockarrow-grp '( 70 300) :link "${HASH_BLOCKARROW}")
    (use :brace-grp      '(200 300) :link "${HASH_BRACE}")
    (use :table-grp      '(330 300) :link "${HASH_TABLE}")))
```

### コネクタ
<!-- autolink: [$$](#コネクタ) -->

<!-- snippet: CONNECTOR-SAMPLE
(diagram (400 150)
  (grid)
  (with-options (:stroke :navy :fill :skyblue)
    (textbox '(100  40) ":R1" :width 40 :height 40 :id :r1)
    (textbox '(200 110) ":R2" :width 40 :height 40 :id :r2)
    (textbox '(300  75) ":R3" :width 40 :height 40 :id :r3))
  (with-options (:stroke :black)
    (connect :r1 :r2 :style :BL)
    (connect :r1 :r3 :style :RR :label '("label" :offset (-20 -5)))
    (connect :r2 :r3 :style :CC :end2 :arrow)))
-->

　connect マクロによって図形要素どうしを接続することができます。また、終端に矢印などの
マークをつけたり、ラベルを付与することもできます。

```kaavio
<!-- expand: CONNECTOR-SAMPLE -->
```
Figure. コネクタのサンプル

　上記サンプルのソースは以下の通りです。

```lisp
<!-- expand: CONNECTOR-SAMPLE -->
```

${BLANK_PARAGRAPH}

　パラメータの詳細については connect マクロを参照してください。ここでは、 `:style` パラ
メータについて説明します。まず、 `:style` のデフォルト値でもある `:CC` は「Center to 
Center」という意味で、対象図形の中心どうしを結ぶような一本の直線で接続をします。これは 
[$@](F#コネクタのサンプル) における R2 と R3 の接続のイメージです。

　`:CC` 以外の接続方法は、 `T B L R` のいずれか 2 つを使って接続位置を指定する方法になり
ます。これはそれぞれ `Top Bottom Left Right` の頭文字で、たとえば `:BL` であれば
「Bottom to Left」という意味になり、これは from 側の下端と to 側の左端を接続する、という
ことを意味します。これは [$@](F#コネクタのサンプル) における R1 と R2 の接続のイメージ
です。

　さらに細かく接続位置を制御したい場合、それぞれの `T B L R` の後ろに 1 ～ 3 のインデックス
を指定することができます。先程の `:BL` という指定は、実は `:B2L2` の省略記法です。インデックス
と実際の接続位置の関係を以下に示します。

```kaavio
(diagram (300 160)
  (grid)
  (rect canvas.center 120 120 :stroke :gray :fill :white
        :contents
        ((with-options (:stroke :none :fill :red :font '(:size 10 :fill :red))
            (circle '( 30   0) 2) (text '( 30  -5) "T1" :align :center)
            (circle '( 60   0) 2) (text '( 60  -5) "T2" :align :center)
            (circle '( 90   0) 2) (text '( 90  -5) "T3" :align :center)
            (circle '( 30 120) 2) (text '( 30 135) "B1" :align :center)
            (circle '( 60 120) 2) (text '( 60 135) "B2" :align :center)
            (circle '( 90 120) 2) (text '( 90 135) "B3" :align :center)
            (circle '(  0  30) 2) (text '( -5  35) "L1" :align :right)
            (circle '(  0  60) 2) (text '( -5  65) "L2" :align :right)
            (circle '(  0  90) 2) (text '( -5  95) "L3" :align :right)
            (circle '(120  30) 2) (text '(125  35) "R1" :align :left)
            (circle '(120  60) 2) (text '(125  65) "R2" :align :left)
            (circle '(120  90) 2) (text '(125  95) "R3" :align :left)))))
```
Figure. コネクタにおける接続点の指定

　なお、上記は接続対象として図形要素の ID を指定した場合の話です。ID でなく point 値を指定
した場合、指定位置に「大きさが（ほぼ）ゼロの四角形」があるかのように処理されます。

　`:style` パラメータを使って `:CC` 以外の接続を指定すると、コネクタは接続対象の図形要素の
位置関係を調べて適切な折れ線を描画します。たとえば、以下は `:style :LR` で接続しています。

```kaavio
(diagram (300 120)
  (grid)
  (with-options (:stroke :navy :fill :skyblue)
    (rect (xy+ canvas.center -50 -30) 40 40 :id :r1)
    (rect (xy+ canvas.center  50  30) 40 40 :id :r2))
  (connect :r1 :r2 :style :LR))
```
Figure. コネクタによる接続経路決定

　この、位置関係を考慮した接続線の自動決定は、接続対象でない他の図形要素の位置を「考慮しない」
ことに注意してください。以下のように、先程の接続経路上に他の要素があったとしても、それを迂回
するほどには賢くありません。

```kaavio
(diagram (300 120)
  (grid)
  (with-options (:stroke :navy :fill :skyblue)
    (rect (xy+ canvas.center -50 -30) 40 40 :id :r1)
    (rect (xy+ canvas.center  50  30) 40 40 :id :r2))
  (rect canvas.center 30 30 :stroke :red :fill :lightpink)
  (connect :r1 :r2 :style :LR))
```
Figure. コネクタによる接続経路決定は他の要素を迂回しない

　上記のような場合に役に立つ（かもしれない）のが `:spacing` パラメータです。
これは、2 回以上折れ曲がる接続線において、「自由な線分の調整を行う」ものです。
[$@](F#コネクタによる接続経路決定は他の要素を迂回しない) の接続線を見てください。
4 回折れ曲がっていて、5 本の線分でできています。このうち、接続対象に直接触れて
いる線分は（ `:style :LR` 指定のため）動かせませんが、残り 3 つの線分はそれぞれ 
x 軸方向または y 軸方向に移動させられそうなことがわかるでしょう。つまり、以下で
赤くした部分です。

```kaavio
(diagram (300 120)
  (grid)
  (with-options (:stroke :navy :fill :skyblue)
    (rect (xy+ canvas.center -50 -30) 40 40 :id :r1)
    (rect (xy+ canvas.center  50  30) 40 40 :id :r2))
  (connect :r1 :r2 :style :LR :stroke :gray)
  (path '((:move-to ( 50 30))
          (:line-to ( 50 60) (250 60) (250 90))) :stroke '(:color :red :width 3))
  (with-block-arrow-options (:stroke :none :fill '(:color :brown :opacity 0.4))
    (block-arrow2 '( 30 45) '( 70 45) 5)
    (block-arrow2 '(150 40) '(150 80) 5 :length 10 :size 12)
    (block-arrow2 '(230 75) '(270 75) 5)))
```
Figure. コネクタの接続経路における「自由な線分」

　`:spacing` パラメータでは、上記のような「自由な線分」それぞれについて、どれだけズラすかを
指定できます。from 側から順番にどれだけズラすかの値を並べたリストで指定します（ズラす方向が 
x 軸方向なのか y 軸方向なのかは線分毎に自動的に決まります）。ここでは、自由な線分のうち 2 番目
の長い線分を下方向に思い切りズラしてしまいましょう。つまり、 `:spacing '(0 60 0)` と指定します。
すると、以下のようになります。

```kaavio
(diagram (300 140)
  (grid)
  (with-options (:stroke :navy :fill :skyblue)
    (rect (xy+ canvas.center -50 -30) 40 40 :id :r1)
    (rect (xy+ canvas.center  50  30) 40 40 :id :r2))
  (rect canvas.center 30 30 :stroke :red :fill :lightpink)
  (connect :r1 :r2 :style :LR :spacing '(0 60 0)))
```
Figure. spacing パラメータによるコネクタ接続経路の調整

　`:spacing` パラメータは、最初の自由な線分を指定するだけの場合にはリストでなくてもかまい
ません。その場合は、 `:spacing 30` のように数値で指定できます。なお、 `:spacing` パラメータ
は「コネクタによる自動的な経路の決定結果を調整する」ものなので、たとえば接続対象の位置関係
が大きく変化した場合などには、都度再調整が必要になることに注意してください。

### パラグラフ
<!-- autolink: [$$](#パラグラフ) -->

<!-- snippet: PARAGRAPH-SAMPLE
(diagram (200 100)
  (grid)
  (paragraph canvas.center "This is paragraph.~%Multi line OK."
             :align :center :valign :center))
-->

　paragraph マクロによって複数行に渡るテキストを描画できます。

```kaavio
<!-- expand: PARAGRAPH-SAMPLE -->
```
Figure. パラグラフのサンプル

　上記サンプルのソースは以下の通りです。パラメータの詳細については paragraph マクロを参照
してください。

```lisp
<!-- expand: PARAGRAPH-SAMPLE -->
```

${BLANK_PARAGRAPH}

　`position` と `align` および `valign` の関係を以下に示します。以下において、赤い点が 
`position` で、アライメント指定はテキストで示されています。

```kaavio
(diagram (500 180)
  (grid)
  (labels ((impl (pos align valign)
             (let ((txt (format nil "align = :~A~~%valign = :~A" align valign)))
              (paragraph pos txt :align align :valign valign)
              (circle pos 3 :fill :red :stroke :none))))
     (impl (xy+ canvas.center   0 -40) :center :bottom)
     (impl (xy+ canvas.center -90 -40) :right  :bottom)
     (impl (xy+ canvas.center  90 -40) :left   :bottom)
     (impl (xy+ canvas.center   0   0) :center :center)
     (impl (xy+ canvas.center -90   0) :right  :center)
     (impl (xy+ canvas.center  90   0) :left   :center)
     (impl (xy+ canvas.center   0  40) :center :top)
     (impl (xy+ canvas.center -90  40) :right  :top)
     (impl (xy+ canvas.center  90  40) :left   :top)))
```
Figure. パラグラフの position とアライメント指定の関係

${BLANK_PARAGRAPH}

### テキストボックス
<!-- autolink: [$$](#テキストボックス) -->

<!-- snippet: TEXTBOX-SAMPLE
(diagram (200 100)
  (grid)
  (textbox '(100 50) "test text.~%multi line."
                     :rx 10 :ry 10 :stroke :black :fill :white))
-->

　textbox マクロによってテキストボックスを描画できます。テキストボックスは
[$$](#四角形) と [$$](#パラグラフ) を組み合わせたようなものです。テキスト
からサイズを自動決定しますが、明示的にボックスのサイズを指定することも可能です。

```kaavio
<!-- expand: TEXTBOX-SAMPLE -->
```
Figure. テキストボックスのサンプル

　上記サンプルのソースは以下の通りです。パラメータの詳細については textbox マクロを参照
してください。

```lisp
<!-- expand: TEXTBOX-SAMPLE -->
```

${BLANK_PARAGRAPH}

　`align` と `valign` の効果を以下に示します。以下のように、テキストボックスの内部で水平
方向、および垂直方向にそれぞれどう寄せるかが変化します。

```kaavio
(diagram (550 220)
  (grid)
  (labels ((impl (pos align valign)
             (let ((txt (format nil "align = :~A~~%valign = :~A" align valign)))
              (textbox pos txt :align align :valign valign
                       :width 170 :height 60 :fill :white :stroke :black))))
     (impl (xy+ canvas.center    0 -70) :center :bottom)
     (impl (xy+ canvas.center -180 -70) :right  :bottom)
     (impl (xy+ canvas.center  180 -70) :left   :bottom)
     (impl (xy+ canvas.center    0   0) :center :center)
     (impl (xy+ canvas.center -180   0) :right  :center)
     (impl (xy+ canvas.center  180   0) :left   :center)
     (impl (xy+ canvas.center    0  70) :center :top)
     (impl (xy+ canvas.center -180  70) :right  :top)
     (impl (xy+ canvas.center  180  70) :left   :top)))
```
Figure. テキストボックスにおける align と valign

${BLANK_PARAGRAPH}

　図の中でテキストボックスのスタイルを統一する作業を簡単にするために、
with-textbox-options マクロが用意されています。これを以下のように使用すること
で、複数のテキストボックスのスタイルを一箇所で指定することができます。


<!-- snippet: WITH-TEXTBOX-OPTIONS-SAMPLE
(diagram (200 100)
  (grid)
  (with-textbox-options (:rx 5 :ry 5 :stroke :navy :fill :azure)
    (textbox '( 50 50) "first~%textbox")
    (textbox '(150 50) "second~%textbox")))
-->

```lisp
<!-- expand: WITH-TEXTBOX-OPTIONS-SAMPLE -->
```

```kaavio
<!-- expand: WITH-TEXTBOX-OPTIONS-SAMPLE -->
```
Figure. with-textbox-options のサンプル


### ドキュメント
<!-- autolink: [$$](#ドキュメント) -->

<!-- snippet: DOCUMENT-SAMPLE
(diagram (200 100)
  (grid)
  (document '(100 50) 100 70 "document~%name"
                    :stroke :navy :fill :skyblue))
-->

　document マクロによってドキュメントを描画できます。ドキュメントはテキストボックスと良く
似ていますが、サイズは自動計算されないため、幅と高さを指定する必要があります。

```kaavio
<!-- expand: DOCUMENT-SAMPLE -->
```
Figure. ドキュメントのサンプル

　上記サンプルのソースは以下の通りです。パラメータの詳細については document マクロを参照
してください。

```lisp
<!-- expand: DOCUMENT-SAMPLE -->
```

${BLANK_PARAGRAPH}

　図の中でドキュメントのスタイルを統一する作業を簡単にするために、with-document-options マクロが
用意されています。これを以下のように使用することで、複数のドキュメントのスタイルを一箇所で指定
することができます。

<!-- snippet: WITH-DOCUMENT-OPTIONS-SAMPLE
(diagram (240 100)
  (grid)
  (drop-shadow)
  (with-document-options (:stroke :darkgreen :fill :mintcream :filter :drop-shadow)
    (document '( 60 50) 100 70 "first~%document")
    (document '(180 50) 100 70 "second~%document")))
-->

```lisp
<!-- expand: WITH-DOCUMENT-OPTIONS-SAMPLE -->
```

```kaavio
<!-- expand: WITH-DOCUMENT-OPTIONS-SAMPLE -->
```
Figure. with-document-options のサンプル

### フォルダ
<!-- autolink: [$$](#フォルダ) -->

<!-- snippet: FOLDER-SAMPLE
(diagram (200 100)
  (grid)
  (folder '(100 50) "folder.~%multi line."
                    :width 100 :height 60
                    :stroke :darkkhaki :fill :cornsilk))
-->

　folder マクロによってフォルダを描画できます。

```kaavio
<!-- expand: FOLDER-SAMPLE -->
```
Figure. フォルダのサンプル

　上記サンプルのソースは以下の通りです。パラメータの詳細については folder マクロを参照
してください。

```lisp
<!-- expand: FOLDER-SAMPLE -->
```

${BLANK_PARAGRAPH}

　図の中でフォルダのスタイルを統一する作業を簡単にするために、with-folder-options マクロが
用意されています。これを以下のように使用することで、複数のフォルダのスタイルを一箇所で指定
することができます。

<!-- snippet: WITH-FOLDER-OPTIONS-SAMPLE
(diagram (240 100)
  (grid)
  (drop-shadow)
  (with-folder-options (:stroke :maroon :fill :linen :filter :drop-shadow)
    (folder '( 60 50) "first~%folder"  :width 100 :height 70)
    (folder '(180 50) "second~%folder" :width 100 :height 70)))
-->

```lisp
<!-- expand: WITH-FOLDER-OPTIONS-SAMPLE -->
```

```kaavio
<!-- expand: WITH-FOLDER-OPTIONS-SAMPLE -->
```
Figure. with-folder-options のサンプル

### 人物
<!-- autolink: [$$](#人物) -->

<!-- snippet: PERSON-SAMPLE
(diagram (200 120)
  (grid)
  (person '( 50 50) 40 :fill :oldlace :stroke :brown :label :sales)
  (person '(150 50) 40 :fill :oldlace :stroke :brown :label :engineer))
-->

　person マクロを使うと、人物の形を描画することができます。縦横比は一定のため座標とサイズを
指定する必要があり、ラベルを添えることができます。

```kaavio
<!-- expand: PERSON-SAMPLE -->
```
Figure. 人物のサンプル

　上記サンプルのソースは以下の通りです。パラメータの詳細については person マクロを参照
してください。

```lisp
<!-- expand: PERSON-SAMPLE -->
```

${BLANK_PARAGRAPH}

　図の中で人物のスタイルを統一する作業を簡単にするために、with-person-options マクロが
用意されています。これを以下のように使用することで、複数の人物のスタイルを一箇所で指定
することができます。

<!-- snippet: WITH-PERSON-OPTIONS-SAMPLE
(diagram (200 100)
  (grid)
  (drop-shadow)
  (with-person-options (:filter :drop-shadow
                        :stroke '(:color :brown :width 2)
                        :fill   '(:color :wheat :opacity 0.5))
    (person '( 50 50) 40)
    (person '(150 50) 40)))
-->

```lisp
<!-- expand: WITH-PERSON-OPTIONS-SAMPLE -->
```

```kaavio
<!-- expand: WITH-PERSON-OPTIONS-SAMPLE -->
```
Figure. with-person-options のサンプル

### 吹き出し
<!-- autolink: [$$](#吹き出し) -->

<!-- snippet: BALLOON-SAMPLE
(diagram (200 100)
  (grid)
  (rect '(30 70) 30 30 :fill :gray :stroke :black :id :r)
  (balloon '(130 50) "balloon text.~%multi line." r.right
                                   :fill :skyblue :stroke :navy))
-->

　balloon マクロによって吹き出しを描画できます。テキストボックスと良く似ていますが、指定した
位置への引き出し線が描画されます。

```kaavio
<!-- expand: BALLOON-SAMPLE -->
```
Figure. 吹き出しのサンプル

　上記サンプルのソースは以下の通りです。パラメータの詳細については balloon マクロを参照して
ください。

```lisp
<!-- expand: BALLOON-SAMPLE -->
```

${BLANK_PARAGRAPH}

　図の中で吹き出しのスタイルを統一する作業を簡単にするために、with-balloon-options マクロが
用意されています。これを以下のように使用することで、複数の吹き出しのスタイルを一箇所で指定
することができます。

<!-- snippet: WITH-BALLOON-OPTIONS-SAMPLE
(diagram (300 100)
  (grid)
  (drop-shadow)
  (with-balloon-options (:stroke :olive :fill :lemonchiffon :filter :drop-shadow)
    (rect canvas.center 20 20 :stroke :black :fill :white)
    (balloon '( 60 60) "first~%balloon"  $1.left  :width 90)
    (balloon '(240 40) "second~%balloon" $2.right :width 90)))
-->

```lisp
<!-- expand: WITH-BALLOON-OPTIONS-SAMPLE -->
```

```kaavio
<!-- expand: WITH-BALLOON-OPTIONS-SAMPLE -->
```
Figure. with-balloon-options のサンプル

### メモ
<!-- autolink: [$$](#メモ) -->

<!-- snippet: MEMO-SAMPLE
(diagram (200 100)
  (grid)
  (memo '(100 50) "memo text.~%multi line." :width 150 :height 80
                  :crease 30 :align :left :valign :top 
                  :fill :lavenderblush :fill2 :lightpink :stroke :red))
-->

　memo マクロによってメモを描画できます。テキストボックスと良く似ていますが、右下に
折り目が描画されます。

```kaavio
<!-- expand: MEMO-SAMPLE -->
```
Figure. メモのサンプル

　上記サンプルのソースは以下の通りです。パラメータの詳細については memo マクロを参照して
ください。

```lisp
<!-- expand: MEMO-SAMPLE -->
```

${BLANK_PARAGRAPH}

　図の中でメモのスタイルを統一する作業を簡単にするために、with-memo-options マクロが
用意されています。これを以下のように使用することで、複数のメモのスタイルを一箇所で
指定することができます。

<!-- snippet: WITH-MEMO-OPTIONS-SAMPLE
(diagram (240 100)
  (grid)
  (drop-shadow)
  (with-memo-options (:stroke :red :fill :lavenderblush
                      :fill2 :lightpink :filter :drop-shadow)
    (memo '( 60 50) "first~%memo"  :width 80 :height 60)
    (memo '(180 50) "second~%memo" :width 80 :height 60)))
-->

```lisp
<!-- expand: WITH-MEMO-OPTIONS-SAMPLE -->
```

```kaavio
<!-- expand: WITH-MEMO-OPTIONS-SAMPLE -->
```
Figure. with-memo-options のサンプル

### キューブ
<!-- autolink: [$$](#キューブ) -->

<!-- snippet: CUBE-SAMPLE
(diagram (200 100)
  (grid)
  (cube canvas.center 80 60 "cube~%text"
              :fill :lightgray :fill2 :darkgray :stroke :black))
-->

　cube マクロによってキューブを描画できます。テキストボックスとは異なり、サイズは
自動計算されないため、幅と高さを指定する必要があります。

```kaavio
<!-- expand: CUBE-SAMPLE -->
```
Figure. キューブのサンプル

　上記サンプルのソースは以下の通りです。パラメータの詳細については cube マクロを参照して
ください。

```lisp
<!-- expand: CUBE-SAMPLE -->
```

${BLANK_PARAGRAPH}

　図の中でキューブのスタイルを統一する作業を簡単にするために、with-cube-options マクロが
用意されています。これを以下のように使用することで、複数のキューブのスタイルを一箇所で
指定することができます。

<!-- snippet: WITH-CUBE-OPTIONS-SAMPLE
(diagram (240 100)
  (grid)
  (drop-shadow)
  (with-cube-options (:stroke :slateblue4
                      :fill   :lightsteelblue1
                      :fill2  :lightsteelblue2 :filter :drop-shadow)
    (cube '( 60 50) 80 60 "first~%cube" )
    (cube '(180 50) 80 60 "second~%cube")))
-->

```lisp
<!-- expand: WITH-CUBE-OPTIONS-SAMPLE -->
```

```kaavio
<!-- expand: WITH-CUBE-OPTIONS-SAMPLE -->
```
Figure. with-cube-options のサンプル

### 円柱
<!-- autolink: [$$](#円柱) -->

<!-- snippet: CYLINDER-SAMPLE
(diagram (200 100)
  (grid)
  (cylinder canvas.center 80 60 "cylinder~%text"
                                   :fill :honeydew :stroke :forestgreen))
-->

　cylinder マクロによって円柱を描画できます。テキストボックスとは異なり、サイズは
自動計算されないため、幅と高さを指定する必要があります。

```kaavio
<!-- expand: CYLINDER-SAMPLE -->
```
Figure. 円柱のサンプル

　上記サンプルのソースは以下の通りです。パラメータの詳細については cylinder マクロを
参照してください。

```lisp
<!-- expand: CYLINDER-SAMPLE -->
```

${BLANK_PARAGRAPH}

　図の中で円柱のスタイルを統一する作業を簡単にするために、with-cylinder-options マクロが
用意されています。これを以下のように使用することで、複数の円柱のスタイルを一箇所で指定
することができます。

<!-- snippet: WITH-CYLINDER-OPTIONS-SAMPLE
(diagram (240 100)
  (grid)
  (drop-shadow)
  (with-cylinder-options (:stroke :slateblue4
                          :fill   :lightsteelblue1 :filter :drop-shadow)
    (cylinder '( 60 50) 80 60 "first~%cylinder" )
    (cylinder '(180 50) 80 60 "second~%cylinder")))
-->

```lisp
<!-- expand: WITH-CYLINDER-OPTIONS-SAMPLE -->
```

```kaavio
<!-- expand: WITH-CYLINDER-OPTIONS-SAMPLE -->
```
Figure. with-cylinder-options のサンプル

### 爆発
<!-- autolink: [$$](#爆発) -->

<!-- snippet: EXPLOSION-SAMPLE
(diagram (350 150)
  (grid)
  (explosion1 '(100 75) 140 120 "explosion~%type1" :fill :pink :stroke :red)
  (explosion2 '(250 75) 140 120 "explosion~%type2" :fill :pink :stroke :red))
-->

　explosion1 マクロおよび explosion2 マクロによって爆発を描画できます。テキストボックス
とは異なり、サイズは自動計算されないため、幅と高さを指定する必要があります
{{fn:不格好に見えるかもしれませんが、Microsoft Word の図形要素をトレースして作っています。 \
そこそこの再現度のはず。}}。

```kaavio
<!-- expand: EXPLOSION-SAMPLE -->
```
Figure. 爆発のサンプル

　上記サンプルのソースは以下の通りです。パラメータの詳細については explosion1 マクロまたは
explosion2 マクロを参照してください。

```lisp
<!-- expand: EXPLOSION-SAMPLE -->
```

${BLANK_PARAGRAPH}

　図の中で爆発のスタイルを統一する作業を簡単にするために、with-explosion-options マクロが
用意されています。これを以下のように使用することで、複数の爆発のスタイルを一箇所で指定
することができます。

<!-- snippet: WITH-EXPLOSION-OPTIONS-SAMPLE
(diagram (300 100)
  (grid)
  (drop-shadow)
  (with-explosion-options (:stroke :orangered3
                           :fill   :thistle1   :filter :drop-shadow)
    (explosion1 '( 80 50) 140 100 "first~%explosion" )
    (explosion2 '(220 50) 140 100 "second~%explosion")))
-->

```lisp
<!-- expand: WITH-EXPLOSION-OPTIONS-SAMPLE -->
```

```kaavio
<!-- expand: WITH-EXPLOSION-OPTIONS-SAMPLE -->
```
Figure. with-explosion-options のサンプル

### 十字
<!-- autolink: [$$](#十字) -->

<!-- snippet: CROSS-SAMPLE
(diagram (400 100)
  (grid)
  (cross '( 80 50) 80 80 20 :fill :pink :stroke :red)
  (cross '(200 50) 80 80 20 :fill :pink :stroke :red :rotate 45)
  (cross '(320 50) 80 80 10 :fill :pink :stroke :red :intersection '(-10 -15) :size-v 15))
-->

　cross マクロによって十字を描画できます。幅と高さ、太さを指定でき、回転させれば×印にも
なります。また、縦横で太さを変えたり、交差する位置をズラすこともできます。

```kaavio
<!-- expand: CROSS-SAMPLE -->
```
Figure. 十字のサンプル

　上記サンプルのソースは以下の通りです。パラメータの詳細については cross マクロを参照して
ください。

```lisp
<!-- expand: CROSS-SAMPLE -->
```

${BLANK_PARAGRAPH}

　図の中で十字のスタイルを統一する作業を簡単にするために、with-cross-options マクロが
用意されています。これを以下のように使用することで、複数の十字のスタイルを一箇所で
指定することができます。

<!-- snippet: WITH-CROSS-OPTIONS-SAMPLE
(diagram (300 100)
  (grid)
  (drop-shadow)
  (with-cross-options (:stroke :purple
                       :fill   :plum   :filter :drop-shadow)
    (cross '( 80 50) 80 80 20)
    (cross '(220 50) 80 80 20 :rotate 45)))
-->

```lisp
<!-- expand: WITH-CROSS-OPTIONS-SAMPLE -->
```

```kaavio
<!-- expand: WITH-CROSS-OPTIONS-SAMPLE -->
```
Figure. with-cross-options のサンプル

### ブロック矢印
<!-- autolink: [$$](#ブロック矢印) -->

<!-- snippet: BLOCKARROW-SAMPLE
(diagram (300 150)
  (grid)
  (with-options (:fill :skyblue
                 :stroke '(:color :navy :width 2))
    (block-arrow1 '(50  40) '(250  40) 20)
    (block-arrow2 '(50 110) '(250 110) 20)))
-->

　block-arrow1 マクロおよび block-arrow2 マクロにより、指定した２点を結ぶ大きな矢印を描画
できます。block-arrow1 は終端側だけに矢印が描画されますが、block-arrow2 を使えば双方向の
矢印になります。

```kaavio
<!-- expand: BLOCKARROW-SAMPLE -->
```
Figure. ブロック矢印のサンプル

　上記サンプルのソースは以下の通りです。パラメータの詳細については block-arrow1 マクロおよび 
block-arrow2 マクロを参照してください。

```lisp
<!-- expand: BLOCKARROW-SAMPLE -->
```

${BLANK_PARAGRAPH}

　いくつかのパラメータは直感的に判りにくいため、以下で説明します。
`(block-arrow1 pt1 pt2 width :length length :size size :margin margin)` とした
場合、それぞれのパラメータは以下のように使用されます。


```kaavio
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

* `size` が省略された場合、デフォルト値として `width` の２倍が使用されます
* `length` が省略された場合、デフォルト値として `size` と同じ値が使用されます
* `margin` が省略された場合、デフォルト値として 0 が使用されます


${BLANK_PARAGRAPH}

　図の中でブロック矢印のスタイルを統一する作業を簡単にするために、with-block-arrow-options マクロが
用意されています。これを以下のように使用することで、複数のブロック矢印のスタイルを一箇所で指定
することができます。

<!-- snippet: WITH-BLOCK-ARROW-OPTIONS-SAMPLE
(diagram (200 100)
  (grid)
  (drop-shadow)
  (with-block-arrow-options (:fill   :honeydew
                             :stroke :darkgreen :filter :drop-shadow)
    (block-arrow1 '(30 25) '(170 25) 20)
    (block-arrow2 '(30 75) '(170 75) 20)))
-->

```lisp
<!-- expand: WITH-BLOCK-ARROW-OPTIONS-SAMPLE -->
```

```kaavio
<!-- expand: WITH-BLOCK-ARROW-OPTIONS-SAMPLE -->
```
Figure. with-block-arrow-options のサンプル

### 波括弧
<!-- autolink: [$$](#波括弧) -->

<!-- snippet: BRACE-SAMPLE
(diagram (400 220)
   (grid)
   (with-options (:font   '(:fill :navy :size 16)
                  :stroke '(:color :navy :width 2))
       (brace '(200  30) :upper  240  40 :r 10 :point 150 :text "upper brace" )
       (brace '(200 190) :bottom 240  40 :r 10 :point  60 :text "bottom brace")
       (brace '( 30 110) :left    40 120 :r 10 :point  40 :text "left brace"  )
       (brace '(370 110) :right   40 120 :r 10 :point  80 :text "right brace" )))
-->

　brace マクロにより、大きな波括弧を描画することができます。波括弧にはテキストを
添えることができます。

```kaavio
<!-- expand: BRACE-SAMPLE -->
```
Figure. 波括弧のサンプル

　上記サンプルのソースは以下の通りです。パラメータの詳細については brace マクロを参照して
ください。

```lisp
<!-- expand: BRACE-SAMPLE -->
```

${BLANK_PARAGRAPH}

　いくつかのパラメータは直感的に判りにくいため、以下で説明します。
`(brace position direction width height :r r :point point)` とした場合、それぞれの
パラメータは以下のように使用されます。

```kaavio
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

* `r` が省略された場合、デフォルト値として `height` （縦向きの場合は `width` ）の 1/3 が指定されます
* `point` が省略された場合、デフォルト値として `width` （縦向きの場合は `height` ）の 1/2 が指定されます
* `point` が左右端に近過ぎる場合、 `r` が自動調整されます

${BLANK_PARAGRAPH}

　図の中で波括弧のスタイルを統一する作業を簡単にするために、with-brace-options マクロが
用意されています。これを以下のように使用することで、複数の波括弧のスタイルを一箇所で
指定することができます。

<!-- snippet: WITH-BRACE-OPTIONS-SAMPLE
(diagram (400 220)
   (grid)
   (with-brace-options (:font   '(:fill  :brown :size 16)
                        :stroke '(:color :brown :width 2))
       (brace '(200  30) :upper  240  40 :r 10 :point 150 :text "upper brace" )
       (brace '(200 190) :bottom 240  40 :r 10 :point  60 :text "bottom brace")
       (brace '( 30 110) :left    40 120 :r 10 :point  40 :text "left brace"  )
       (brace '(370 110) :right   40 120 :r 10 :point  80 :text "right brace" )))
-->

```lisp
<!-- expand: WITH-BRACE-OPTIONS-SAMPLE -->
```

```kaavio
<!-- expand: WITH-BRACE-OPTIONS-SAMPLE -->
```
Figure. with-brace-options のサンプル

### テーブル
<!-- autolink: [$$](#テーブル) -->

<!-- snippet: TABLE-SAMPLE
(diagram (320 120)
   (table '(160 60) '(30 30 30) '(75 75 75 75)
          :stroke :black :fills '(:rc :white :r0 :skyblue)
          :texts '((:foo :bar :baz :quux)
                   (5 6 42 -123)
                   ("asdf" "qwer" "zxcv" "hjkl"))))
-->

　table マクロを使うことで、表を描画することができます。以下の例では、３行４列の表を
作成しています。

```kaavio
<!-- expand: TABLE-SAMPLE -->
```
Figure. テーブルのサンプル

　上記サンプルのソースは以下の通りです。パラメータの詳細については table マクロを参照して
ください。

```lisp
<!-- expand: TABLE-SAMPLE -->
```

${BLANK_PARAGRAPH}

　表内でのテキストの指定について説明します。まず、 `texts` パラメータそのものを省略した場合、
すべてのセルにおいてテキストは指定されなかったものとして扱われます。指定する場合、典型的には
前述の例のように「リストのリスト」として指定することになります。

```lisp
  :texts '((:foo :bar :baz :quux)
           (5 6 42 -123)
           ("asdf" "qwer" "zxcv" "hjkl"))
```

　この例では 3 行 x 4 列全てのセルにテキストを指定していますが、空のままにしておきたいセルに
は `nil` を指定してください。見ての通り、 `:texts` といっても数値やシンボルも指定することが
できます。ただし、改行を含む複数行のテキストを表示させることはできません。それが必要な場合は、
後述する [$$](#with-table-cell を使ったセル内描画)を利用してください。

　表示させるテキストのフォント情報やアライメントを指定したい場合、個々のデータ自体をリストで
指定する必要があります。

* データはリストの先頭要素として指定します。
* 水平方向のアライメントは、 `:align` に続けて `:left, :center, :right` のいずれかを指定します。 \
これを省略した場合、 `data` が数値であれば右寄せ、文字列であれば左寄せ、キーワードなどのシンボル \
であれば中央揃えになります。
* 垂直方向のアライメントは、 `:valign` に続けて `:top, :center, :bottom` のいずれかを指定します。 \
デフォルトで `:center` 指定になります
* フォントは、 `:font` に続けてフォント情報を指定します。これを省略した場合、その時点でのデフォルト \
フォントが使用されます。


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

```kaavio
<!-- expand: TABLE-ALIGN-SAMPLE -->
```
Figure. テーブルにおけるテキストの align と valign パラメータ

<!-- collapse:begin -->
　※上記サンプルのソースはこちら。

```lisp
<!-- expand: TABLE-ALIGN-SAMPLE -->
```
<!-- collapse:end -->

　データを（リストでなく）直接指定した場合でも、デフォルトの設定として数値ならば右寄せ、
文字列ならば左寄せ、シンボルならば中央揃えという措置が行なわれるため、多くの場合で個別
セルのテキストデータ指定をリストで行なう必要は無いでしょう。キーワードを使うと全て大文字
で表示されてしまいますが、 `:|Foo|` のようにバーティカルバーで括ると文字の大小を維持した
まま表示されます（[$@](F#テーブルにおけるテキストの align と valign パラメータ) のコード
を参照）。

#### with-table-cell を使ったセル内描画

　table の id とセルの行・列番号を指定して with-table-cell マクロを使用することで、該当する
セルをサブキャンバスとした描画ができます。以下の例では、2 x 2 の空のテーブルを作成し、その
うちの２つのセル内部に図形を描画しています。

<!-- snippet: WITH-TABLE-CELL-SAMPLE
(diagram (220 220)
    (table '(110 110) '(100 100) '(100 100)
               :stroke :navy :fills '(:rc :white) :id :tbl)
    (with-table-cell (:tbl 1 0)
      (circle canvas.center 30 :fill :lightcyan :stroke :blue))
    (with-table-cell (:tbl 0 1)
      (rect canvas.center 50 50 :fill :lightpink :stroke :red :rotate 45)))
-->

```kaavio
<!-- expand: WITH-TABLE-CELL-SAMPLE -->
```
Figure. with-table-cell の使用例


　上記のサンプルは以下のコードで生成されています。

```lisp
<!-- expand: WITH-TABLE-CELL-SAMPLE -->
```

　with-table-cell マクロは事実上、テーブルの単一セル領域を指定した with-subcanvas マクロと
して機能します。そのため、 `canvas` を使ってその中心座標や幅、高さ情報にアクセスできます。

${BLANK_PARAGRAPH}

#### with-table-range を使った範囲取得

　with-table-range マクロを使用することで、テーブル内の指定範囲をサブキャンバスとして
取得することができます。範囲は、 `fills` パラメータと同じ形式のキーワードで指定します。
以下の例では、4 x 5 のテーブルを作成し、その一部をマスクするために with-table-range マクロ
を使用しています。

<!-- snippet: WITH-TABLE-RANGE-SAMPLE
(diagram (300 100)
  (grid)
  (table (x+ canvas.center -40) '(20 20 20 20) '(40 40 40 40 40)
         :stroke :black :fills '(:rc :white :r0 :skyblue) :id :tbl)
  (with-table-range (:tbl :r2-3c2-4)
    (rect canvas.center canvas.width canvas.height
          :stroke :none :fill '(:color :lightgray :opacity 0.8))
    (brace (x+ canvas.right 10) :left 10 canvas.height :r 3 :text "masked")))
-->

```kaavio
<!-- expand: WITH-TABLE-RANGE-SAMPLE -->
```
Figure. with-table-range の使用例


　上記のサンプルは以下のコードで生成されています。

```lisp
<!-- expand: WITH-TABLE-RANGE-SAMPLE -->
```

　with-table-range マクロは事実上、テーブルの指定領域を指定した with-subcanvas マクロ
として機能します。そのため、 `canvas` を使ってその中心座標や幅、高さ情報にアクセス
できます。

#### テーブルの罫線を細かく制御するには

　基本的に、テーブルの罫線は `:stroke` パラメータで一種類しか指定できません。外枠、縦線、横線
などを細かく制御したい場合、少しコードを書いてあげる必要があります。以下に例を示します。

<!-- snippet: TABLE-LINES-SAMPLE
(diagram (400 100)
  (grid)
  (table canvas.cc '(20 20 20) '(50 80 40 20 30 40 100) :id :tbl
         :stroke :none
         :fills '(:rc :white :r0 :skyblue :r1-2c3-5 :lightpink))
  (mapcar (lambda (rng)
            (with-table-range (:tbl rng)
              (line (list canvas.tl canvas.tr) :stroke 1)))
          '(:r1 :r2))
  (mapcar (lambda (rng)
            (with-table-range (:tbl rng)
              (line (list canvas.tl canvas.bl)
                    :stroke '(:width 0.5 :dasharray (2 1)))))
          '(:c1 :c2 :c3 :c4 :c5 :c6))
  (with-table-range (:tbl :rc)
    (rect canvas.cc canvas.width canvas.height :stroke 2)))
-->

```kaavio
<!-- expand: TABLE-LINES-SAMPLE -->
```
Figure. テーブルの罫線を描き分けるサンプル

　上記サンプルのコードは以下の通りです。table マクロでは `:stroke :none` によって罫線
なしにしておき、その後 with-table-range マクロを複数回使って罫線を個別に描画しています。


```lisp
<!-- expand: TABLE-LINES-SAMPLE -->
```


${BLANK_PARAGRAPH}

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

```kaavio
<!-- expand: PATH-SAMPLE -->
```
Figure. パスのサンプル

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

Table. path の data で使用できるディレクティブ
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

```kaavio
(diagram (400 100)
  (grid)
  (path '((:move-to (200 10) (230 50) (170 50) (200 10)))
          :stroke :black :fill :rosybrown)
  (with-options (:font "Courier New")
    (textbox '(170 80) "(:move-to (200 10) (230 50)
          (170 50) (200 10))" :no-frame t :align :left )))
```
Figure. :move-to ディレクティブのサンプル

　なお、:move-to で指定する点は :absolute および :relative の影響を受けます。

#### :line-to ディレクティブ
<!-- autolink: [:line-to](#:line-to ディレクティブ) -->

　`(:line-to pt)` という記述により、現在の点から直線を描きながら指定した点 pt に移動します。
`(:line-to pt1 pt2 pt3 ...)` のように複数の点を記述することができます。これは現在の点から
順番に直線を描きながら移動します。

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
Figure. :line-to ディレクティブのサンプル

　なお、:line-to で指定する点は :absolute および :relative の影響を受けます。

#### :h-line-to ディレクティブ
<!-- autolink: [:h-line-to](#:h-line-to ディレクティブ) -->

　`(:h-line-to x)` という記述により、現在の点から指定した x 座標まで水平線を描きながら移動します。

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
Figure. :h-line-to ディレクティブのサンプル

　なお、:h-line-to で指定する点は :absolute および :relative の影響を受けます。

#### :v-line-to ディレクティブ
<!-- autolink: [:v-line-to](#:v-line-to ディレクティブ) -->

　`(:v-line-to y)` という記述により、現在の点から指定した y 座標まで垂直線を描きながら移動します。

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
Figure. :v-line-to ディレクティブのサンプル

　なお、 :v-line-to で指定する点は :absolute および :relative の影響を受けます。

#### :arc-to ディレクティブ
<!-- autolink: [:arc-to](#:arc-to ディレクティブ) -->

　:arc-to ディレクティブにより、現在の点から楕円弧を描きながら指定した点 pt に移動します。

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
Figure. :arc-to ディレクティブのサンプル

　:arc-to は円弧と考え方はほぼ同じですが、指定はやや複雑です。まず、パラメータ構成は以下の
通りです。

```lisp
(:arc-to rx ry x-axis-rotation large-arc-flag sweep-flag pt)
```

　`rx, ry, x-axis-rotation` は円弧での指定と同じもので、ベースとなる楕円の x 半径、y 半径、
および x 軸に対する楕円の回転角です。:arc-to では、この 3 つの値で決まる楕円の一部を使って、
「現在の点から点 `pt` に至る曲線」を描画します。

　以下のように、そのような楕円を使って 2 点を通る円弧は 4 種類描けることがわかります。

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
Figure. :arc-to における rx, ry, x-axis-rotation

　この 4 種類の円弧のうち、どれを使うのかを指定するのが `large-arc-flag` と `sweep-flag` です。
いずれも 0 または 1 を指定するもので、 `large-arc-flag` は「1 ならば大きい方の円弧、0 ならば
小さい方の円弧」を使うという意味で、 `sweep-flag` は「1 ならば時計回り、0 ならば反時計回り」
です。このイメージを以下に示します。

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
Figure. :arc-to における large-arc-flag, sweep-flag


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

```kaavio
<!-- expand: 2D-CURVE-TO-SAMPLE-1 -->
```
Figure. :2d-curve-to ディレクティブのサンプル

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

```kaavio
<!-- expand: 2D-CURVE-TO-SAMPLE-2 -->
```
Figure. :2d-curve-to ディレクティブのサンプル - 2


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

```kaavio
<!-- expand: 3D-CURVE-TO-SAMPLE-1 -->
```
Figure. :3d-curve-to ディレクティブのサンプル

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

```kaavio
<!-- expand: 3D-CURVE-TO-SAMPLE-2 -->
```
Figure. :3d-curve-to ディレクティブのサンプル -2

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

```kaavio
<!-- expand: 3D-CURVE-TO-SAMPLE-3 -->
```
Figure. :3d-curve-to ディレクティブのサンプル - 3


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
Figure. :absolute ディレクティブのサンプル


#### :relative ディレクティブ
<!-- autolink: [:relative](#:relative ディレクティブ) -->

　後続のディレクティブを、現在の点に対する相対座標として処理します。以下の例では、三角形の描画を 
:relative 指定によって描画しています。同じ図形を :absolute で描画する [$@](F#:absolute ディレクティブのサンプル) 
と比較してみてください。

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
Figure. :relative ディレクティブのサンプル

#### :close-path ディレクティブ
<!-- autolink: [:close-path](#:close-path ディレクティブ) -->

　現在の点から現在のサブパス（:move-to によって開始された一連の描画）の開始点まで直線を描き
ながら移動します。以下の例では、１回のパスで 2 つの三角形を描画していますが、それぞれの三角形の
最後の直線を :close-path で描画しています。

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
Figure. :close-path ディレクティブのサンプル

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

```kaavio
<!-- expand: RAW-SVG-SAMPLE -->
```
Figure. raw-svg のサンプル

${BLANK_PARAGRAPH}

## 座標と位置

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

```kaavio
<!-- expand: GEOMETRY-SAMPLE-1 -->
```
Figure. kaavio における座標系

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
Figure. 図形要素の座標参照 - 1

${BLANK_PARAGRAPH}

　さらに、コネクタの `:style` パラメータ指定で使う `L1` や `T3` といった記法も使用できます。

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
Figure. 図形要素の座標参照 - 2

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

```kaavio
(diagram (200 100)
  (grid)
  (rect '(70 50) 50 50 :id :rct)
  (diamond rct.cr  60 40 :pivot :CL)
  (circle rct.cr 3 :stroke :none :fill :red))
```
Figure. pivot パラメータの利用例


　直線や円弧、コネクタ、およびブロック矢印では、 `center` および線の端点として `end1, end2` が
利用できます。以下のように、この場合の `center` は線の総延長のちょうど半分にあたる位置になります
（円弧の場合はベースとなる楕円の中心です）。

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
Figure. 図形要素の座標参照 - 3

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

## サブキャンバス
<!-- autolink: [$$](#サブキャンバス) -->
<!-- autolink: [キャンバス](#サブキャンバス) -->

　[$@ 章](#座標と位置)では、座標指定のための ID 名として `canvas` を指定すると作成中の図全体の
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

```kaavio
<!-- expand: SUBCANVAS-SAMPLE-1 -->
```
Figure. サブキャンバスのサンプル

　サブキャンバスは入力データの一部分で独自の座標系を一時的に作成するもので、それ以外の効果は
ありません。たとえば、描画順序を制御するレイヤーとは無関係ですし、サブキャンバスの矩形で
描画内容をクリッピングすることもしません。つまり、サブキャンバスの機能自体には実質的に原点を
ズラす効果しかありません
{{fn:サブキャンバスが自動的なクリッピングをしないのは、kaavio が作図指向で絵画的な効果を重視していない \
という理由があります。}}。
クリッピングを行ないたい場合は [$@ 章](#クリッピング)を参照してください。

　with-subcanvas マクロで明示的にサブキャンバスを作成するのでなく、作成した図形要素の内部を
サブキャンバスとすることもできます。これには、 `:contents` パラメータを使用します。たとえば、
先程の例と同じ作図をするには以下のように書きます。この場合、先程とは違って rect の中に 
circle が置かれることになります（rect が動けば circle も動く）。

```lisp
(diagram (300 150)
  (grid)
  (circle '(50 50) 20 :stroke :brown :fill :thistle)
  (rect '(200 90) 100 100 :stroke :gray :fill :none
     :contents
     ((circle '(50 50) 20 :stroke :navy :fill :skyblue))))
```
Figure. contents パラメータを使ったサブキャンバス


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

```lisp
(diagram (300 150)
  (grid)
  (circle '(50 50) 20 :stroke :brown :fill :thistle)
  (rect '(200 90) 100 100 :stroke :gray :fill :none :id :rct)
  (with-subcanvas-of (:rct)
     (circle '(50 50) 20 :stroke :navy :fill :skyblue)))
```
Figure. with-subcanvas-of を使ったサブキャンバス

${BLANK_PARAGRAPH}


　最後に with-current-canvas マクロを紹介しておきます。with-subcanvas マクロと 
with-subcanvas-of マクロは新しいサブキャンバスを確立するものでしたが、
with-current-canvas マクロは「現在のキャンバスへのアクセスを簡単にする」ものです。
キャンバスを使っていると、 `canvas.center, canvas.width, canvas.height` などを頻繁に
使うことになりますが、これらに短い名前でアクセスできるようにします。たとえば、
[本節冒頭の例](#サブキャンバス)は、以下のように書き換えることができます
（１回ずつしか使ってないのでメリットがわかりにくいですが）。

```lisp
(diagram (300 150)
  (grid)
  (circle '(50 50) 20 :stroke :brown :fill :thistle)
  (with-subcanvas ('(150 40) 100 100)
    (with-current-canvas ((cc center) (w width) (h height))
      (rect cc w h :stroke :gray :fill :none)
      (circle '(50 50) 20 :stroke :navy :fill :skyblue))))
```
Figure. with-current-canvas の使用


${BLANK_PARAGRAPH}

## クリッピング
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

```kaavio
<!-- expand: DEFS-CLIPPING-SAMPLE-2 -->
```
Figure. with-clipping-current-canvas マクロの例


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

```kaavio
<!-- expand: DEFS-CLIPPING-SAMPLE-3 -->
```
Figure. with-clipping-use マクロの例

　より複雑なパスでクリッピングを行なうことも可能です。以下では、文字を使ってクリッピング
をています。

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

```kaavio
<!-- expand: DEFS-CLIPPING-SAMPLE-4 -->
```
Figure. 文字を使ったクリッピングの例

## 定義と再使用

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

```kaavio
<!-- expand: DEFGROUP-USE-SAMPLE -->
```
Figure. defgroup と use のサンプル

${BLANK_PARAGRAPH}

　注意してほしいのは、defgroup マクロで作成する定義に指定する ID と、図形要素を描画する
時に指定する ID は別モノだということです。上記の例で言えば、 `:frame` は定義の ID なので、
use マクロの最初のパラメータには使えますがコネクタの接続対象としては指定できません。逆も
同様で、たとえば rect を描いた後にその ID を指定して use することはできません。

* ${{TODO}{defgroup の中で use できない（っぽい：要確認）ことに言及する必要がある。}}

${BLANK_PARAGRAPH}

## パターンとグラデーション

　「フィル」では通常塗り潰しを指定しますが、定義した図形を敷き詰める「パターン」や、複雑な色の
変化を見せる「グラデーション」も利用できます。また、パターンとグラデーションはフィルだけでなく
ストロークで使用することも可能です。

### パターン
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

```kaavio
<!-- expand: PATTERN-1ST-SAMPLE -->
```
Figure. 単純なパターンのサンプル

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

```kaavio
<!-- expand: PATTERN-2ND-SAMPLE -->
```
Figure. パターンのサンプル - 2

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

### グラデーション
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

```kaavio
<!-- expand: GRADIENT-1ST-SAMPLE -->
```
Figure. 単純なグラデーションのサンプル

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

```kaavio
<!-- expand: GRADIENT-2ND-SAMPLE -->
```
Figure. グラデーションのサンプル - 2

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

## テーマ
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
      (cross        (xy+ cc -190  100)  70 70 20)
      (block-arrow1 (xy+ cc -100  100) (xy+ cc -20 100) 20))))
-->

```lisp
<!-- expand: THEME-SAMPLE-1 -->
```

${BLANK_PARAGRAPH}

描画結果は以下のようなものになります。これは 3 行目の with-theme で default テーマを
指定したことによる効果です。

```kaavio
<!-- expand: THEME-SAMPLE-1 -->
```
Figure. デフォルトテーマの使用例

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

### 利用できるテーマ

　現在、以下のテーマが利用できます。

* default : [$$](#一般的な図形)の図形要素を設定しています。サンプルは [$@](F#デフォルトテーマの使用例) を \
参照してください。


### 新しいテーマの作成

　register-theme を使えば、新しいテーマを作成することができます。以下は、default テーマを
定義している register-theme の使用例です。

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
Figure. register-theme によるテーマの作成

${BLANK_PARAGRAPH}


　register-theme に続けて、括弧内にテーマ名をキーワードで指定します。続けて、各図形要素に
指定するスタイルを指定します。このとき、

* それぞれの指定は `(textbox :stroke :black :fill :white)` のようにリストで指定します
    * その先頭要素は、 with-xxx-options マクロの xxx 部分を指定してください
    * 後続要素は、with-xxx-options のパラメータ部分をそのまま記述してください
* with-options に相当する指定は、 `(t  :font '(:family "sans-serif"))` のように `t` で始まるリストで指定してください

### テーマのカスタマイズ

　register-theme において、「ベースとする既存のテーマ」を指定することで、差分だけを指定した
カスタムテーマを作成することができます。以下の例では、default テーマをベースとして my-theme と
いうテーマを作成しています。

```lisp
(register-theme (:my-theme :default)
  (cylinder :stroke :maroon :fill :beige)
  (cross :stroke :purple :fill :lavender))
```
Figure. register-theme でベーステーマを指定する例

${BLANK_PARAGRAPH}


　上記の my-theme を使用して [$@](F#デフォルトテーマの使用例) と同じ図面を描画した結果を
以下に示します。カスタマイズした内容が反映されていることがわかります。

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
      (cross        (xy+ cc -190  100)  70 70 20)
      (block-arrow1 (xy+ cc -100  100) (xy+ cc -20 100) 20))))
```
Figure. テーマのカスタマイズ例

${BLANK_PARAGRAPH}

## パラメータの詳細
### 色の指定
<!-- autolink: [$$](#色の指定) -->
<!-- autolink: [色名の指定](#色の指定) -->

　kaavio は SVG 形式で図形を生成するため、色の指定は SVG の規格に準拠します。

* `#rrggbb` 表記による、6 桁の16進指定。rr、gg、bb は順に赤、緑、青の成分で、 00〜ff の範囲で \
指定します。
* `#rgb` 表記による、3 桁の16進指定。r、g、b は順に赤、緑、青の成分で、 0〜f の範囲で指定します。 \
これは `#rrggbb` の簡略表記で、たとえば #136 は #113366 に相当します。
* rgb 関数による指定。これは `(rgb r g b)` の要領で使用します。r、g、b は順に赤、緑、青の \
成分で、それぞれ 0〜255 の整数または 0.0〜1.0 の小数点数で指定します。0.0〜1.0 の指定の場合、 \
それに 255 をかけた値が指定されます。
* 色名での指定。 `:black` など先頭にコロンをつけたキーワードの形式で指定します。使用できる色の名前と \
サンプルは [$@ 節](#色の名前) を参照してください。

${BLANK_PARAGRAPH}

### ストローク
<!-- autolink: [$$](#ストローク) -->

　ストロークとは、図形を描画する際の「線の引き方」を指定する情報です。

　このマニュアルのほとんどの部分では、 `:stroke :red` のように、 `:stroke` に続けて色名だけを
指定しています。もう少し複雑な場合、 `:stroke '(:color :red :width 3)` といった要領で色名と
太さを指定している個所もあります。実はこれらは全て簡易的な指定方法で、ストロークにはもっと多くの
情報が含まれています。以下に説明します。

* `color` は線の色を指定します。色の指定方法については [$@ 節](#色の指定)を参照してください。
* `width` は線の太さです。数値で指定します。
* `opacity` は線の不透明度です。0.0 ～ 1.0 の数値で指定します。0.0 は完全な透明、1.0 は完全な不透明です。
* `linecap` は線の両端の形状です。詳細は後述します。
* `linejoin, miterlimit` は線が折れ曲ってできる角の形状に関する指定です。詳細は後述します。
* `dasharray, dashoffset` は点線や破線を描画する場合の指定です。詳細は後述します。

${BLANK_PARAGRAPH}

　それぞれについて細かい説明を始める前に、 `:stroke :red` といった記述がどのように扱われるのか
を説明する必要があるでしょう。 `:stroke` によるこれらの指定は、実は全て make-stroke 関数に
渡されます。make-stroke 関数は、渡されたのが単一の値の場合には、数値なら太さ、キーワードや
文字列なら色名などと解釈します。そして `'(:color :red :width 3)` などの（複数要素からなる）
リストの場合、名前付きパラメータの羅列として解釈します。

　make-stroke 関数はその結果として「ストローク情報オブジェクト」を返しますが、そのストローク情報
オブジェクトを make-stroke 関数自身に渡した場合、そのまま返すようになっています。そのため、自分で
明示的に make-stroke 関数を使ってストローク情報オブジェクトを作成し、Common Lisp 変数に格納して
複数の図形要素で使用する、ということも可能です。以下のように{{fn:これはあまり使わない方が良いテクニックかもしれません。 \
図形要素別の「デフォルト設定」とは無関係に動作するので、慣れないと混乱するかもしれないからです。}}。
この関数の詳細は [$@ 節](#function make-stroke) を参照してください。

```lisp
(let ((st (make-stroke :color :blue :width 4 :opacity 0.3)))
  (rect   ... :stroke st)
  (circle ... :stroke st))
```

${BLANK_PARAGRAPH}

#### ストロークのデフォルト設定

　さて、実際のところ、作図をする上でそれぞれの要素の線がバラバラな色や太さで描かれることはないでしょう。
わかりやすい図面というのは、同じ種類の図形要素は同じ種類の線で描画されているなど、統制が取れているものです。
このことは、多くの作図においては「まったく同じストロークの指定を繰り返す場合が多い」ことを意味します。
前述の方法でストローク情報オブジェクトを作成して使い回すことも可能ですが、もっとよい方法があります。
それは「デフォルトストロークの変更」です。もともと、ストロークはデフォルトで `:color :black :width 1` と
されていますが、with-options マクロを使えばこれを変更することができます{{fn:Lisper の方へ。with-options マクロは  \
`*default-stroke*` や `*default-fill*` といったスペシャル変数を束縛するものです。非 Lisper のユーザーへの説明を \
わかりやすくする上でこの方法をとっています。}}。
以下の例では４種類の四角形を描いていますが、B, C の四角形ではデフォルトストロークを変更しています。

<!-- snippet: WITH-OPTIONS-STROKE-SAMPLE
(diagram (250 100)
  (grid)
  (rect '(50 50) 40 40)
  (with-options (:stroke '(:color :navy :width 8 :linejoin :round))
    (rect '(100 50) 40 40)
    (rect '(150 50) 40 40 :stroke :brown))
  (rect '(200 50) 40 40 :stroke :brown)
  (text '( 50 90) "A" :align :center)
  (text '(100 90) "B" :align :center)
  (text '(150 90) "C" :align :center)
  (text '(200 90) "D" :align :center))
-->

```lisp
<!-- expand: WITH-OPTIONS-STROKE-SAMPLE -->
```

```kaavio
<!-- expand: WITH-OPTIONS-STROKE-SAMPLE -->
```
Figure. with-options によるデフォルトストロークの変更

${BLANK_PARAGRAPH}

　C の四角形では「色しか指定していない」のに線が太くなっていることに注意してください。
これは、「明示的に指定されていないものはデフォルトの設定が使用される」からです。
with-options マクロによってデフォルト設定が変更されており、B の四角形では（ `:stroke` を
省略することによって）全てがデフォルト設定で描画されました。C の四角形では、 
`:stroke :brown` によってデフォルト設定をベースとして色だけを変更している、というわけです。

　with-options マクロでは全体のデフォルト設定を変更しますが、図形要素によっては個別にデフォルト設定
を持っています。たとえば、テキストボックスであれば with-textbox-options マクロでデフォルト設定を
変更することができます。

${BLANK_PARAGRAPH}

#### linecap

　`linecap` について説明します。これは `:butt :round :square` から指定するもので、
以下のように線の端の形状が変わります。 `:butt` は指定した開始点／終了点で線が切れます
が、 `:round :square` では開始点／終了点を少しはみ出すことに注意してください。

```kaavio
(diagram (200 100)
  (grid)
  (with-options (:font 10
                 :stroke '(:color :lightgray :width 16))
    (line '(( 50 25) (150  25)) :stroke '(:linecap   :butt))
    (text '(100 30) ":butt" :align :center)
    (line '(( 50 50) (150  50)) :stroke '(:linecap  :round))
    (text '(100 55) ":round" :align :center)
    (line '(( 50 75) (150  75)) :stroke '(:linecap :square))
    (text '(100 80) ":square" :align :center))
  (with-options (:stroke '(:color :red :width 1 :dasharray (3 3)))
    (line '(( 50  0) ( 50 100)))
    (line '((150  0) (150 100)))))
```
Figure. linecap のサンプル

${BLANK_PARAGRAPH}

#### linejoin と miterlimit

　`linejoin` は、線が折れ曲る部分の形状を `:miter :round :bevel` から指定するもので、
以下のように角の形状が変わります。

```kaavio
(diagram (300 100)
  (grid)
  (with-options (:stroke '(:color :gray :width 20))
    (labels ((impl (x linejoin tag)
                (with-subcanvas ((list x 0) 80 80)
                   (line '(( 10 65) ( 40 35) ( 70 65))
                         :stroke `(:linejoin ,linejoin))
                   (text '(40 90) tag :align :center))))
      (impl  10 :miter ":miter")
      (impl 110 :round ":round")
      (impl 210 :bevel ":bevel"))))
```
Figure. linejoin のサンプル

${BLANK_PARAGRAPH}

<!-- collapse:close -->
__miterlimit についての説明は暫定です（作者自身 SVG 規格における miterlimit の意味が良くわかっていない）。__

　`miterlimit` は、 `linejoin` が `:miter` の場合における、結合される線の太さに対する結合部の長さの
比率を数値で指定します。デフォルト値は 4 です。

```kaavio
(diagram (600 100)
  (grid)
  (with-options (:stroke '(:color :gray :width 16))
    (labels ((impl (x limit)
                (with-subcanvas ((list x 0) 80 80)
                   (line '(( 25 65) ( 40 35) ( 55 65))
                         :stroke `(:linejoin :miter :miterlimit ,limit))
                   (text '(40 90) (format nil "~A" limit) :align :center))))
      (impl  10 0)
      (impl 110 2)
      (impl 210 4)
      (impl 310 6)
      (impl 410 8)
      (impl 510 10))))
```
Figure. miterlimit のサンプル

<!-- collapse:end -->

${BLANK_PARAGRAPH}

#### dasharray と dashoffset

　`dasharray` と `dashoffset` は点線や破線を描画する際に指定します。 `dasharray` は繰り返される線の幅と
間隔の幅を数値でリストにしたものを渡します。通常は `dasharray` で指定された点線・破線を最初から描画します
が、 `dashoffset` を指定すると開始するオフセットを指定できます。以下に例を示します。

```kaavio
(diagram (400 120)
  (grid)
  (with-options (:font 10 :stroke '(:color :black :width 4))
    (text '(100 20) "dasharray" :align :center)
    (labels ((impl (y arr)
                (text  `(65 ,(+ y 5)) (format nil "~A" arr) :align :right)
                (line `((70 ,y) (160 ,y)) :stroke `(:dasharray ,arr))))
      (impl  40 '(2 2))
      (impl  60 '(5 5))
      (impl  80 '(7 3))
      (impl 100 '(10 5 3 5)))
    (text '(300 20) "dashoffset" :align :center)
    (labels ((impl (y offset)
                (text  `(265 ,(+ y 5)) (format nil "~A" offset) :align :right)
                (line `((270 ,y) (380 ,y)) :stroke `(:dasharray (20 10) :dashoffset ,offset))))
      (impl  40  0)
      (impl  60  5)
      (impl  80 10)
      (impl 100 15))))
```
Figure. dasharray, dashoffset のサンプル

${BLANK_PARAGRAPH}

#### ストロークにおけるパターンとグラデーションの指定

　ストロークでできるのは、単色で線や点線をひくことだけではありません。パターンやグラデーション
を定義して、それを使うこともできます。たとえば以下では、赤色から青色に変化するグラデーションを
作成し、四角形のストロークで使用しています。

<!-- snippet: STROKE-URL-SAMPLE
(diagram (140 70)
  (grid)
  (defgradient (:linear :gradient1)
    (0.00 :red)
    (1.00 :blue))
  (rect canvas.center 100 50 :stroke '(:url :gradient1 :width 4) :fill :white))
-->

```kaavio
<!-- expand: STROKE-URL-SAMPLE -->
```
Figure. グラデーションを使ったストロークのサンプル

${BLANK_PARAGRAPH}

　上記のサンプルを生成するコードは以下になります。パターンとグラデーションの詳細について
は [$@ 章](#パターンとグラデーション)を参照してください。定義されたグラデーションを指定する
ために、 `:stroke '(:url :gradient1 :width 4)` という記述をしています。

```lisp
<!-- expand: STROKE-URL-SAMPLE -->
```

　[$@ 節](#ストローク)では説明を省略しましたが、make-stroke 関数は `url` というパラメータで
パターンやグラデーションの ID を指定することができます。これを指定した場合、 `:color` パラメータは
無視されます。

${BLANK_PARAGRAPH}

### フィル
<!-- autolink: [$$](#フィル) -->
<!-- autolink: [塗り潰し](#フィル) -->

　フィルとは、図形を描画する際の「塗り潰し方」を指定する情報です。

　このマニュアルのほとんどの部分では、 `:fill :white` のように、 `:fill` に続けて色名だけを
指定しています。もう少し複雑な場合、 `:fill '(:color :skyblue :opacity 0.3)` といった要領で
色名と不透明度を指定している個所もあります。実はこれらは全て簡易的な指定方法で、フィルにはもっと
多くの情報が含まれています。以下に説明します。

* `color` は塗り潰しの色を指定します。色の指定方法については [$@ 節](#色の指定)を参照してください。
* `opacity` は塗り潰しの不透明度です。0.0 ～ 1.0 の数値で指定します。0.0 は完全な透明、1.0 は完全な不透明です。
* `rule` は複雑な図形における塗り潰しの規則を指定するものです。詳細は後述します。

${BLANK_PARAGRAPH}

　それぞれについて細かい説明を始める前に、 `:fill :white` といった記述がどのように扱われるのか
を説明する必要があるでしょう。 `:fill` によるこれらの指定は、実は全て make-fill 関数に
渡されます。make-fill 関数は、渡されたのが単一の値の場合には、キーワードや文字列なら色名と解釈
します。そして `'(:color :red :width 3)` などの（複数要素からなる）リストの場合、名前付き
パラメータの羅列として解釈します。

　make-fill 関数はその結果として「フィル情報オブジェクト」を返しますが、そのフィル情報オブジェクト
を make-fill 関数自身に渡した場合、そのまま返すようになっています。そのため、自分で明示的に 
make-fill 関数を使ってフィル情報オブジェクトを作成し、Common Lisp 変数に格納して複数の図形要素
で使用する、ということも可能です。以下のように{{fn:これはあまり使わない方が良いテクニックかもしれません。 \
図形要素別の「デフォルト設定」とは無関係に動作するので、慣れないと混乱するかもしれないからです。}}。
この関数の詳細は [$@ 節](#function make-fill)を参照してください。

```lisp
(let ((fl (make-fill :color :blue :opacity 0.3)))
  (rect   ... :fill fl)
  (circle ... :fill fl))
```

${BLANK_PARAGRAPH}

#### フィルのデフォルト設定

　ストロークの説明では「デフォルト設定がある」という話をしましたが、塗り潰しにおいても
デフォルト設定があります。フィルはデフォルトで `:color :none` とされているため、図形を
描画すると塗り潰し無し（つまり背景が透けて見える）になります。with-options マクロを使え
ばこれを変更することができます。以下の例では４種類の四角形を描いていますが、B, C の四角形
ではデフォルトのフィルを変更しています。

<!-- snippet: WITH-OPTIONS-FILL-SAMPLE
(diagram (250 100)
  (grid)
  (rect '(50 50) 40 40 :fill :white)
  (with-options (:fill '(:color :red :opacity 0.2))
    (rect '(100 50) 40 40)
    (rect '(150 50) 40 40 :fill :blue))
  (rect '(200 50) 40 40 :fill :lightgray)
  (text '( 50 90) "A" :align :center)
  (text '(100 90) "B" :align :center)
  (text '(150 90) "C" :align :center)
  (text '(200 90) "D" :align :center))
-->

```lisp
<!-- expand: WITH-OPTIONS-FILL-SAMPLE -->
```

```kaavio
<!-- expand: WITH-OPTIONS-FILL-SAMPLE -->
```
Figure. with-options によるデフォルトフィルの変更

${BLANK_PARAGRAPH}

　C の四角形では「色しか指定していない」のに半透明になっていることに注意してください。
これは、「明示的に指定されていないものはデフォルトの設定が使用される」からです。
with-options マクロによってデフォルト設定が変更されており、B の四角形では（ `:fill` を
省略することによって）全てがデフォルト設定で描画されました。C の四角形では、 `:fill :blue` に
よってデフォルト設定をベースとして色だけを変更している、というわけです。

　with-options マクロでは全体のデフォルト設定を変更しますが、図形要素によっては個別にデフォルト設定
を持っています。たとえば、テキストボックスであれば with-textbox-options マクロでデフォルト設定を
変更することができます。

${BLANK_PARAGRAPH}

#### fill における rule パラメータ

　`rule` パラメータについて説明します。これは `:nonezero :evenodd` から指定するもので、
以下のように複雑な図形の塗り潰し方が変わります。

<!-- snippet: FILL-RULE-SAMPLE
(diagram (400 120)
  (let ((points '((50 10) (20 90) (90 40) (10 40) (80 90))))
    (with-subcanvas ('(75 0) 100 100)
      (polygon points :fill '(:color :skyblue :rule :nonzero))
      (text '(50 110) ":nonzero" :align :center))
    (with-subcanvas ('(225 0) 100 100)
      (polygon points :fill '(:color :skyblue :rule :evenodd))
      (text '(50 110) ":evenodd" :align :center))))
-->

```kaavio
<!-- expand: FILL-RULE-SAMPLE -->
```
Figure. fill における rule のサンプル

<!-- collapse:begin -->
[$@](F#fill における rule のサンプル) のソースはこちら

```lisp
<!-- expand: FILL-RULE-SAMPLE -->
```
<!-- collapse:end -->

${BLANK_PARAGRAPH}

　これについては、ひとまずのところあまり使用頻度が高いとは思われないため、SVG本の説明を
引用するに留めます。

> __塗りつぶしのルールの説明__
> 
> 　完全を期すために、fill-rule がどのように機能するかを説明しますが、悩む必要はありません −− 詳細を
> 知らなくてもルールを利用することはできます。nonzero ルールは、問題になっている点から無限遠まで線を
> 引くことで、その点が多角形の内側か外側かを判断します。その線が多角形の線と何回交差するかを数え、多角形
> の線が右から左に向かって描かれている場合は1を足し、左から右に向かって描かれている場合は1を引きます。
> 合計がゼロであれば、その点は多角形の外側にあります。合計がゼロ以外（nonzero）であれば、その点は多角形
> の内側にあります。
> 
> 　evenodd ルールも、問題になっている点から無限遠まで線を引きますが、その線が多角形の線と何回交差する
> かを単純に数えます。交差した回数が奇数であれば、その点は内側にあります。偶数であれば、外側にあります。

${BLANK_PARAGRAPH}

#### フィルにおけるパターンとグラデーションの指定

　フィルでできるのは、単色で塗り潰すことだけではありません。パターンやグラデーションを
定義して、それを使うこともできます。たとえば以下では、細い斜め線のパターンを作成し、
四角形の中に敷き詰めています。

<!-- snippet: FILL-URL-SAMPLE
(diagram (140 70)
  (defpattern (:tile :width 5 :height 5 :units :userSpaceOnUse)
    (line '((5 0) (0 5)) :stroke '(:color :blue :width 0.3)))
  (rect canvas.center 100 50 :stroke :black :fill '(:url :tile)))
-->

```kaavio
<!-- expand: FILL-URL-SAMPLE -->
```
Figure. パターンを使った塗り潰しのサンプル

${BLANK_PARAGRAPH}

　上記のサンプルを生成するコードは以下になります。パターンとグラデーションの詳細について
は [$@ 章](#パターンとグラデーション)を参照してください。定義されたパターンを指定する
ために、 `:fill '(:url :tile)` という記述をしています。

```lisp
<!-- expand: FILL-URL-SAMPLE -->
```

　[$@ 節](#フィル)では説明を省略しましたが、make-fill 関数は `url` というパラメータで
パターンやグラデーションのID を指定することができます。これを指定した場合、 `:color` など
他のパラメータは無視されます。

${BLANK_PARAGRAPH}

### フォント
<!-- autolink: [$$](#フォント) -->

　kaavio では様々な図形要素にテキストを付与できます。おおむね Web におけるフォント指定と同じ
ですが、醜いハックも含まれています。

　通常の使用では、 `:font 24` のようにサイズだけを指定したりします。もう少し複雑な場合、 
`:font '(:family "monospace" :size 16)` といった要領でフォントの種類とサイズを指定することも
あります。実はこれらは全て簡易的な指定方法で、フォントにはもっと多くの情報が含まれています。
以下に説明します。

* `family` はフォントの名称を文字列で指定します。
* `size` はフォントのサイズを指定します（厳密には違うらしいのですが、詳細は後述します）。
* `fill` はフォントの塗り潰しを指定します。
* `stroke` はフォントの輪郭線を指定します。フォントの場合、通常は塗り潰しのみで輪郭線は指定しません。
* `style` はスタイルの指定です。 `:normal :italic :oblique` のいずれかから選択します。
* `decoration` は装飾の指定です。 `:none :underline :overline :line-through` のいずれかから選択します。
* `weight` は文字の太さの指定です。 `:normal :bold :bolder :lighter` のいずれか、または  \
100 200 300 400 500 600 700 800 900 のいずれかです。
* `filter` はテキストに適用するフィルタの指定です。
* `line-spacing` は、テキストが複数行になる場合の行間を指定します。
* `width-spice` は、「テキストが実際に描画される幅」を計算するための目安となる係数を指定します。詳細は後述します。

${BLANK_PARAGRAPH}

　それぞれについて細かい説明を始める前に、 `:font 24` といった記述がどのように扱われるのか
を説明する必要があるでしょう。 `:font` によるこれらの指定は、実は全て make-font 関数に
渡されます。make-font 関数は、渡されたのが単一の値の場合には、数値ならサイズ、キーワードなら色名、
文字列ならフォントファミリ名などと解釈します。そして `'(:family "sans serif" :size 10)` などの
（複数要素からなる）リストの場合、名前付きパラメータの羅列として解釈します。

　make-font 関数はその結果として「フォント情報オブジェクト」を返しますが、そのフォント情報
オブジェクトを make-font 関数自身に渡した場合、そのまま返すようになっています。そのため、
自分で明示的に make-font 関数を使ってフォント情報オブジェクトを作成し、Common Lisp 変数に
格納して複数の図形要素で使用する、ということも可能です。以下のように。この関数の
詳細は [$@ 節](#function make-font)を参照してください。

```lisp
(let ((fnt (make-font :family "Courier New, monospace" :size 12)))
  (text   ... :font fnt)
  (paragraph ... :font fnt))
```

${BLANK_PARAGRAPH}

#### フォントのデフォルト設定

　実際のところ、作図をする上でそれぞれのテキストがバラバラなフォントで描かれることはない
でしょう。わかりやすい図面というのは、同じ種類の図形要素は同じ種類の線で描画されているなど、
統制が取れているものです。このことは、複数のテキストを含む作図においては「まったく同じ
フォントの指定を繰り返す場合が多い」ことを意味します。前述の方法でフォント情報オブジェクト
を作成して使い回すことも可能ですが、もっとよい方法があります。それは「デフォルトフォントの
変更」です。もともと、フォントはデフォルトで `:size 12 :fill :black` とされています
{{fn:正確には `:size 12 :fill :black :width-spice 0.65 :line-spacing 2` です。}}が、
with-options マクロを使えばこれを変更することができます。以下の例では４種類のテキストを
描いていますが、B, C のテキストはデフォルトフォントを変更しています。

<!-- snippet: WITH-OPTIONS-FONT-SAMPLE
(diagram (250 100)
  (grid)
  (text '(30 70) "A" :font 36)
  (with-options (:font '(:size 36 :fill :navy :style :italic :weight :bold))
    (text '(80 70) "B")
    (text '(130 70) "C" :font '(:fill :brown)))
  (text '(190 70) "D" :font '(:size 36 :fill :brown)))
-->

```lisp
<!-- expand: WITH-OPTIONS-FONT-SAMPLE -->
```

```kaavio
<!-- expand: WITH-OPTIONS-FONT-SAMPLE -->
```
Figure. with-options によるデフォルトフォントの変更

${BLANK_PARAGRAPH}

　C のテキストでは「色しか指定していない」のに文字装飾が変更されていることに注意して
ください。これは、「明示的に指定されていないものはデフォルトの設定が使用される」から
です。with-options マクロによってデフォルト設定が変更されており、B のテキストでは
（ `:font` を省略することによって）全てがデフォルト設定で描画されました。C の四角形
では、 `:font :brown` によってデフォルト設定をベースとして色だけを変更している、という
わけです。

　with-options マクロでは全体のデフォルト設定を変更しますが、図形要素によっては個別に
デフォルト設定を持っています。たとえば、テキストボックスであれば with-textbox-options マクロで
デフォルト設定を変更することができます。

${BLANK_PARAGRAPH}

#### フォントにおける family パラメータ

　family パラメータについては、SVG本の説明を引用しておきます（手抜きでごめんなさい）。

> __font-family__
>
> この値に指定するのは、フォントファミリー名または総称ファミリー名をカンマで区切って並べたリストです。
> これはフォールバック値のリストです。つまり、SVGビューアーは、自身が認識する最初のファミリー名を使用します。
> 総称ファミリー名は、リスト内の最後に指定しなければなりません。SVG ビューアーに義務づけられているのは、
> 総称ファミリー名を認識することと、それらを表示できるフォントを備えていることです。総称ファミリー名は、
> serif、 sans-serif、 monospace、 fantasy、 cursive です。 serif （セリフ）フォントには、
> ストロークの端に「セリフ」と呼ばれる小さな飾り −−「うろこ」や「ひげ」などとも呼ばれます −− があります。
> sans-serif （サンセリフ）フォントには、セリフがありません。‥‥（中略）‥‥serif フォントも 
> sans-serif フォントも、プロポーショナルフォント（可変幅フォント）です。つまり、大文字の「M」の幅と
> 「I」の幅は異なります。 monospace （モノスペース）フォントは、タイプライターの文字のように、
> すべてのグリフが同じ幅を持つフォントです（等幅フォント、固定幅フォントなどと呼ばれます）。これは、
> セリフがあるフォントでも、ないフォントでもかまいません。 fantasy フォントと cursive フォントは、
> ブラウザーやSVGビューアーによって実装が大きく異なる可能性があります。

${BLANK_PARAGRAPH}

#### フォントにおける stroke と fill

　前述の通り、フォントでは通常 stroke を指定しません。しかし、逆に fill を無し（あるいは背景色同等）に
して stroke を指定することで縁取られたテキストを描画することもできます。以下に例を示します。

```kaavio
(diagram (400 190)
  (grid)
  (with-options (:stroke 2)
    (labels ((frmt (lst)
               (string-downcase (format nil ":~A :~A" (first lst) (second lst))))
             (impl (y &rest lst)
               (text `(10 ,y) "Abcdefgh" :align :left :font lst)
               (with-options (:font '(:size 12 :weight :normal :stroke :none :fill :black))
                 (text `(270 ,(- y 20)) (frmt lst))
                 (text `(270 ,y)        (frmt (cddr lst))))))
      (with-options (:font '(:family "sans-serif" :size 40 :weight :bold))
        (impl  50 :stroke :black :fill :white)
        (impl 110 :stroke :none  :fill :black)
        (impl 170 :stroke :black :fill :black)))))
```
Figure. font における stroke と fill

${BLANK_PARAGRAPH}

#### size と line-spacing

　フォントの size 情報については、まず SVG本の説明を引用しておきましょう。

> __font-size__
> 
> この値には、テキストが複数行になる場合の、ベースラインからベースラインまでのグリフの距離を
> 指定します（SVGでは、複数行の `<text>` の内容は自分自身で配置しなければならないので、この
> 概念は少々空論的です）。

　上記のようなわけで、kaavio ではフォント情報の size は単純に「フォントの縦方向のサイズである」
という立場をとっています。正確には、「文字の上端からベースラインまでの距離」です。そして、
paragraph などで複数行を描画する場合の「ベースラインから次行の上端までの距離」を `line-spacing` で
指定します。以下は、 `:font '(:size 50 :line-spacing 30)` で描画した場合のサンプルです。

```kaavio
(diagram (400 170)
  (grid)
  (with-options (:font '(:size 50 :line-spacing 30))
    (paragraph '(130 20) "Abcdefg~%Hijklmn"))
  (with-options (:stroke '(:color :red :width 1 :dasharray (2 2)))
    (line '((130   0) (130 160)))
    (line '((  0  20) (400  20)))
    (line '((  0  70) (400  70)))
    (line '((  0 100) (400 100)))
    (line '((  0 150) (400 150))))
  (with-options (:font '(:fill :blue))
    (let ((em (make-endmark :stroke :blue :type :arrow :size :small)))
      (line '((110 20) (110  70)) :stroke :blue :end1 em :end2 em)
      (text '(100 50) "size" :align :right)
      (line '((110 70) (110 100)) :stroke :blue :end1 em :end2 em)
      (text '(100 90) "line-spacing" :align :right))))
```
Figure. font における size と line-spacing

${BLANK_PARAGRAPH}

　見ての通り、文字によってはベースラインよりも下に描画されることがありますから、 `size` に
よって `line-spacing` も調整する必要があります。

${BLANK_PARAGRAPH}

#### width-spice

　`width-spice` は、kaavio におけるもっとも醜いハックです。簡単に言えば、「フォントサイズと
テキスト長にかける係数を指定可能にし、テキストの描画幅の計算に使う」というものです。というのも、
「テキストがそのフォント設定で実際に描画される場合、どれだけの幅をとることになるかわからない」ためです。
たとえば、テキストボックスでは明示的に幅を指定しない場合はテキストから幅を自動計算しようとしますが、
width-spice の値によって結果は以下のように変わります。

<!-- snippet: WIDTH-SPICE-SAMPLE
(diagram (450 200)
  (grid)
  (labels ((impl (x y spice)
             (let ((txt (format nil "width-spice ~A" spice)))
               (textbox `(,x ,y) txt :font `(:width-spice ,spice)))))
    (with-options (:stroke :gray :fill :lightgray
                   :font '(:fill :red :size 12 :family "Courier New, monospace"))
      (text '(120 20) "Courier New" :align :center :font '(:fill :black))
      (impl 120  50 0.4)
      (impl 120  90 0.6)
      (impl 120 130 0.8)
      (impl 120 170 1.0))
    (with-options (:stroke :gray :fill :lightgray
                   :font '(:fill :red :size 12 :family "serif"))
      (text '(330 20) "serif font" :align :center :font '(:fill :black))
      (impl 330  50 0.4)
      (impl 330  90 0.6)
      (impl 330 130 0.8)
      (impl 330 170 1.0))))
-->

```kaavio
<!-- expand: WIDTH-SPICE-SAMPLE -->
```
Figure. width-spice のサンプル

<!-- collapse:begin -->
[$@](F#width-spice のサンプル) のソースはこちら

```lisp
<!-- expand: WIDTH-SPICE-SAMPLE -->
```
<!-- collapse:end -->

${BLANK_PARAGRAPH}

　上記の結果を見る限りでは、Courier New フォントでは 0.8 程度、serif フォントでは 0.7 弱
が spice 値としてよさそうに思えます。しかし、プロポーショナルフォントでは（文字数が同じでも）
文字列内で使われている文字の種類によって変わってしまいますし、そもそも制御できない外部環境に
よって描画は変わりえるのです。上記サンプルの左側は `:family` 指定で `Courier New, monospace` と
していますから、Courier New フォントがない環境で表示すれば monospace が使用されるでしょう。
それによって、テキストの実際の描画幅は大きく変化してしまいます。

　width-spice は kaavio の開発初期の段階で導入されたものですが、現在ではあまり良くない
アイデアとみなされています。テキストボックスなどでは幅を明示的に指定するのが現実的であり、
その他テキストの描画幅においてもなんらかの仮定に依存するようなデザインをしないことが賢明でしょう
{{fn:SVG 規格には `textLength` 属性というものがあって、指定されたスペースにテキストを収めることができるそうです。 \
これを今後導入するかもしれません。しかし、重箱の隅っこ的な機能は SVG レンダリングエンジン（多くの場合はブラウザのこと \
ですが）がサポートしていない可能性があるので、今後の検討次第です。}}。

${BLANK_PARAGRAPH}

### IDと参照

　たいていの場合、図中の要素は互いに関係しています。コネクタなどで接続されることも
ありますし、それ以外の位置関係に意味があるかもしれません。kaavio では、
個々の図形要素に ID を付与し、その ID を指定することで図形要素どうしを接続
したり位置関係を指定したりできます。

　このマニュアルの[冒頭に出てきたサンプル](#簡単なサンプル)は、以下のような
ものでした。

```lisp
<!-- expand: FIRST-SAMPLE -->
```

　ここでは、rect と circle に `:x` や `:y` という ID を付与しており、
connect での指定に利用しています。これが ID とその参照の基本的なかたちです。
もうひとつ、ID は座標の指定でも利用できます。上のサンプルを少し変えて、circle 
を rect の相対位置で指定してみましょう。生成される図はまったく同じです。

```lisp
(diagram (300 150)
  (grid)
  (rect   '( 50  50) 80 60         :fill :powderblue :id :x)
  (circle (xy+ x.center 200 50) 40 :fill :moccasin   :id :y)
  (connect :x :y :end2 :arrow))
```

　このように、ID（この例では `x` ）とその図形要素の属性（この例では `center` ）
を `.` で繋ぐことで既出の図形要素の位置などを参照することができます。使用できる
属性は「[](#座標と位置)」にて紹介しています。

　続いて、ID を使わない方法について説明します。これは 「$N 記法」と呼ばれるもので、
「N 個手前で登場した図形要素を参照」するものです。先ほどの circle の例は以下
のように書くことができます。

```lisp
(diagram (300 150)
  (grid)
  (rect   '( 50  50) 80 60          :fill :powderblue :id :x)
  (circle (xy+ $1.center 200 50) 40 :fill :moccasin   :id :y)
  (connect :x :y :end2 :arrow))
```

　この `$1` は、circle から見て「ひとつ前の図形要素」、つまり rect の `x` を参照して
います。このような $N 記法は、$1 から $9 までが利用可能です。つまり、9 つ前まで
の要素を ID を使わずに参照できるわけです。

　この `$1.center` といった記法は、ID 指定を省略した図形要素でも使用することが
できます。しかし、connect はどうでしょう。connect では図形要素の ID そのもの
をキーワードで指定する必要があります。

　まず、connect での ID 指定の話からしましょう。$N 記法を使うと、以下の
ように書くことができます。

```lisp
(diagram (300 150)
  (grid)
  (rect   '( 50  50) 80 60          :fill :powderblue :id :x)
  (circle (xy+ $1.center 200 50) 40 :fill :moccasin   :id :y)
  (connect $2.id $1.id :end2 :arrow))
```

　connect からみれば rect は 2 つ前なので `$2` 、circle は 1 つ前なので `$1` で参照し、
`$1.id` とすることでその ID を参照しています。結局、これは `:x` や `:y` を指定している
のと同じことになります。

　この説明からすると奇妙に思われるかもしれませんが、上記の状態になっていれば rect / circle から 
ID 指定は除去することができます。つまり、以下のように書けるということです。

```lisp
(diagram (300 150)
  (grid)
  (rect   '( 50  50) 80 60          :fill :powderblue)
  (circle (xy+ $1.center 200 50) 40 :fill :moccasin)
  (connect $2.id $1.id :end2 :arrow))
```

　このコードでは、connect における `$2.id` といった記述は何を返すのでしょうか？　
実は、kaavio では ID 指定を省略された図形要素には、kaavio が独自の ID を
付与するようになっています{{fn:Lisper の方へ：要するに `gensym` を使っています。}}。
この自動付与される ID はコード上には現われないため直接指定することはできませんが、
connect での指定で $N.id とする場合には使用できます。

　最後に、ID に . を続けることで参照できる属性の一覧を示しておきます。座標参照の正確な
位置については [$@](F#図形要素の座標参照 - 1) を参照してください。

<!-- stack:push tr style="font-size: 14;" -->

Table. ID 指定で参照できる属性の一覧
| 属性           | 説明                               |
|:===============|:-----------------------------------|
| `id`           | 図形要素の ID を参照します。       |
| `width`        | 幅と高さを持つ図形要素の幅を参照します。         |
| `height`       | 幅と高さを持つ図形要素の高さを参照します。       |
| `topleft`      | 幅と高さを持つ図形要素の左上の座標を参照します。 |
| `top`          | 幅と高さを持つ図形要素の上端の座標を参照します。 |
| `topright`     | 幅と高さを持つ図形要素の右上の座標を参照します。 |
| `left`         | 幅と高さを持つ図形要素の右端の座標を参照します。 |
| `center`       | 幅と高さを持つ図形要素の中心の座標を参照します。<br> \
                 または直線やコネクタの中央点の座標を参照します。 |
| `right`        | 幅と高さを持つ図形要素の左端の座標を参照します。 |
| `bottomleft`   | 幅と高さを持つ図形要素の左下の座標を参照します。 |
| `bottom`       | 幅と高さを持つ図形要素の下端の座標を参照します。 |
| `bottomright`  | 幅と高さを持つ図形要素の右下の座標を参照します。 |
| `end1`         | 直線やコネクタの端点（始点）の座標を参照します。 |
| `end2`         | 直線やコネクタの端点（終点）の座標を参照します。 |

<!-- stack:pop tr -->

${BLANK_PARAGRAPH}

### 回転
<!-- autolink: [$$](#回転) -->

　お望みなら、図形要素を回転させることができます。ただし、この機能は kaavio の他の機能とは
あまり整合しないので注意が必要です。ひとまずのところ、 `:rotate` パラメータに角度を与えること
で回転させることができます。以下のように。

<!-- snippet: ROTATE-SAMPLE
(diagram (300 200)
  (grid)
  (rect canvas.center 150 100 :fill :lightgray :stroke :black :rotate 30))
-->

```lisp
<!-- expand: ROTATE-SAMPLE -->
```

　このコードは以下の図を生成します。

```kaavio
<!-- expand: ROTATE-SAMPLE -->
```
Figure. 回転のサンプル

　見た目は問題なく回転できていますが、この四角形にコネクタを接続しようとすると何がどう
「整合しない」のかがわかります。以下のように、接続点は回転には追従しないのです。

```kaavio
(diagram (300 200)
  (grid)
  (drop-shadow)
  (let ((w 150)
        (h 100))
    (rect canvas.center w h :id :frame :fill :none
                        :stroke '(:color :gray :dasharray (3 3)))
    (with-options (:stroke :none :fill :red)
      (dotimes (x 3)
        (circle (x+ frame.topleft    (* (1+ x) (/ w 4))) 3)
        (circle (x+ frame.bottomleft (* (1+ x) (/ w 4))) 3)
        (circle (y+ frame.topleft    (* (1+ x) (/ h 4))) 3)
        (circle (y+ frame.topright   (* (1+ x) (/ h 4))) 3)))
    (rect canvas.center w h :stroke :black :rotate 30 :id :target
          :fill '(:color :gray :opacity 0.3) :filter :drop-shadow)
    (with-options (:stroke '(:color :brown :width 2)  :fill :white)
      (circle '( 20 20) 10 :id :c1)
      (circle '(280 20) 10 :id :c2)
      (connect :c1 :target :style :CC  :end2 :arrow)
      (connect :c2 :target :style :LT3 :end2 :arrow))))
```
Figure. 回転しても図形要素の属性は変化しない

　接続点だけではありません。[$@](T#ID 指定で参照できる属性の一覧) で紹介した属性についても、
すべて回転前の状態のままとなります。これら全てを回転に追従させることも開発途上で検討されました
が、すっきりとした仕様に落とし込むことができなかったため、回転の機能は「図形要素の見た目だけを
回転させるもの」とされました。

${BLANK_PARAGRAPH}

### フィルタ
<!-- autolink: [$$](#フィルタ) -->

　フィルタは、SVG 規格における `<filter>` 要素の機能を利用するものの総称で、おおまかに言って
図形要素に対してなんらかのグラフィカルな効果を及ぼす機能です。現在の kaavio における
フィルタ機能は極めて限定されたもので、包括的なサポートを行うかは未定です。

　現在、drop-shadow マクロと glow-shadow マクロの２種類が利用できます。例を以下に示します。
通常、冒頭で `(drop-shadow)` などの記述により使用を宣言し、図形要素の `:filter` パラメータ
で指定します。

<!-- snippet: FILTER-SAMPLE
(diagram (360 180)
  (grid)
  (drop-shadow)
  (glow-shadow)
  (rect (y+ canvas.center -30) 100 70
        :stroke :black :fill :lightgray :filter :drop-shadow)
  (text (y+ canvas.center  70) "sample text"
        :align :center :font (make-font :size 36 :filter :glow-shadow)))
-->

```lisp
<!-- expand: FILTER-SAMPLE -->
```

　上記のコードは以下の画像を生成します。四角形の右下にできている影が drop-shadow で、テキスト
の周囲に広がるような影が glow-shadow です。

```kaavio
<!-- expand: FILTER-SAMPLE -->
```
Figure. フィルタのサンプル



${BLANK_PARAGRAPH}

　これらのシャドウは通常図面内で１種類しか使用しないため、ID もデフォルト値を使用可能になっています。
複数のシャドウを導入する場合には、ID を明示的に指定して区別することができます。以下の例では、
`color-matrix` を指定して異なる色のシャドウを導入しています。

<!-- snippet: FILTER-SAMPLE-2
(diagram (400 200)
  (grid)
  (drop-shadow :id :shadow1 :color-matrix '(0 0 0 0.5 0
                                            0 0 0 0   0
                                            0 0 0 0   0
                                            0 0 0 0.6 0))
  (drop-shadow :id :shadow2 :color-matrix '(0 0 0 0   0
                                            0 0 0 0   0
                                            0 0 0 0.5 0
                                            0 0 0 0.6 0))
  (glow-shadow :id :shadow3 :color-matrix '(0 0 0 0   0
                                            0 0 0 0.5 0
                                            0 0 0 0   0
                                            0 0 0 0.9 0))
  (rect '(100  70) 100 70 :fill :lightpink :stroke :red  :filter :shadow1)
  (rect '(300  70) 100 70 :fill :lightcyan :stroke :navy :filter :shadow2)
  (text '(200 170) "sample text" :align :center
                   :font (make-font :size 36 :fill :green :filter :shadow3)))
-->

```kaavio
<!-- expand: FILTER-SAMPLE-2 -->
```
Figure. フィルタのサンプル - 2

<!-- collapse:close -->
上記サンプルのコードはこちら。

```lisp
<!-- expand: FILTER-SAMPLE-2 -->
```
<!-- collapse:end -->

${BLANK_PARAGRAPH}

　シャドウを使用する場合、通常は図面内の多くの図形要素に同じフィルタを適用します。そのため、
`with-options` でデフォルトのフィルタを指定可能になっています。[$@](F#簡単なサンプル-2) の
コードを参照してください。

${BLANK_PARAGRAPH}

### レイヤー
<!-- autolink: [$$](#レイヤー) -->

　レイヤーは図形要素の表示順序を制御するための仕組みです。まずは以下の例をご覧ください。
表示位置が重なる四角形を３つ描画しています。

<!-- snippet: LAYER-SAMPLE-1
(diagram (160 160)
  (grid)
  (with-options (:stroke :black)
    (rect '(50 50)              60 60 :fill :lightcyan :id :R1)
    (rect (xy+ R1.center 30 30) 60 60 :fill :lightpink :id :R2)
    (rect (xy+ R2.center 30 30) 60 60 :fill :palegreen :id :R3)))
-->

```lisp
<!-- expand:LAYER-SAMPLE-1 -->
```

　R2 は R1 に、R3 は R2 に、それぞれ座標指定において依存しているので、この順序で書く必要が
あり、結果として表示は以下のようになります。これが kaavio における描画の基本的なルール、
「コード上で書いた順に描画される」です。

```kaavio
<!-- expand:LAYER-SAMPLE-1 -->
```

　この描画順序を制御するのがレイヤーという機能です。レイヤーを使用するには、コードの冒頭で 
layer 関数を使用してその名前とともに使用を宣言し、図形要素のパラメータ `:layer` で所属する
レイヤーを指定します。以下の例では、3 つのレイヤーを導入し、それぞれの四角形を別のレイヤーに
所属させています。

<!-- snippet: LAYER-SAMPLE-2
(diagram (160 160)
  (layer :L1)
  (layer :L2)
  (layer :L3)
  (grid)
  (with-options (:stroke :black)
    (rect '(50 50)              60 60 :fill :lightcyan :id :R1 :layer :L3)
    (rect (xy+ R1.center 30 30) 60 60 :fill :lightpink :id :R2 :layer :L2)
    (rect (xy+ R2.center 30 30) 60 60 :fill :palegreen :id :R3 :layer :L1)))
-->

```lisp
<!-- expand:LAYER-SAMPLE-2 -->
```

　複数のレイヤーが存在する場合、その導入順で描画が行なわれます。上記の例では逆順になるように
レイヤーを指定しているので、以下のような描画になります。

```kaavio
<!-- expand:LAYER-SAMPLE-2 -->
```
Figure. レイヤーを使用した表示順序の制御

　さらに、layer 関数にはオプションの `display` 引数があります。これは省略時のデフォルト値は 
`:inline` ですが、 `:none` を指定することで「そのレイヤー全体を非表示にする」ことができます。
先程の例に対してレイヤー L2 を非表示にする例を以下に示します。

<!-- snippet: LAYER-SAMPLE-3
(diagram (160 160)
  (layer :L1)
  (layer :L2 :none)
  (layer :L3)
  (grid)
  (with-options (:stroke :black)
    (rect '(50 50)              60 60 :fill :lightcyan :id :R1 :layer :L3)
    (rect (xy+ R1.center 30 30) 60 60 :fill :lightpink :id :R2 :layer :L2)
    (rect (xy+ R2.center 30 30) 60 60 :fill :palegreen :id :R3 :layer :L1)))
-->

```lisp
<!-- expand:LAYER-SAMPLE-3 -->
```

```kaavio
<!-- expand:LAYER-SAMPLE-3 -->
```
Figure. レイヤーを非表示にする例

${BLANK_PARAGRAPH}

　レイヤー機能について以下にまとめます。

* レイヤーはその宣言順で描画される
* レイヤー指定されない図形要素は「暗黙の背景レイヤー」として最初に（つまり一番下に）描画される
* 同じレイヤーに所属する図形要素はコード上での登場順で描画される
* layer 関数でオプションの `display` パラメータに `:none` を指定するとレイヤーをまるごと非表示にできる \
{{fn:ブラウザなどの SVG ビューワー上で動的にレイヤーの表示／非表示を切り替えることは可能ですが、 \
kaavio としては現状サポートしていません。}}

${BLANK_PARAGRAPH}

### リンク
<!-- autolink: [$$](#リンク) -->

　HTML で任意の文言にリンクを設定できるように、SVG では図形要素にリンクを設定することができます。
kaavio では、図形要素の `:link` パラメータで実現します。以下の例では、図中の四角形に
[目次](A#toc-link-target)へのリンクを設定しています。

<!-- define: HASH_TOC = '[](A#toc-link-target)' -->

<!-- snippet: LINK-SAMPLE-1
(diagram (200 100)
  (grid)
  (with-options (:stroke :navy :fill :lightcyan)
    (rect canvas.center  60 60 :link "${HASH_TOC}")))
-->

```lisp
<!-- expand:LINK-SAMPLE-1 -->
```

```kaavio
<!-- expand:LINK-SAMPLE-1 -->
```
Figure. 図形要素へのリンクの設定例

　この例では、 `:link` に続けて文字列でリンク先を指定しています（このマニュアルでは SVG 図面
を HTML に直接埋め込んでいるので、これは HTML 文書内へのアンカーを指定したリンクです）。
あるいは、 `:link '(:url "${HASH_TOC}" :target :blank)` といった指定をする場合もあります。
実はこれらは全て簡易的な指定方法で、リンクにはもう少し多くの情報が含まれています。以下に
説明します。

* `:url` はリンク先の URL を文字列で指定します。
* `:target` はターゲット指定です。 `:blank` などを指定します（[$@ 節](#function make-link)参照）。

${BLANK_PARAGRAPH}

　それぞれについて細かい説明を始める前に、 `:link "URL"` といった記述がどのように扱われるのか
を説明する必要があるでしょう。 `:link` によるこれらの指定は、実は全て make-link 関数に
渡されます。make-link 関数は、渡されたのが単一の値の場合には、それを URL と解釈します。
そして `'(:url "URL" :target :blank)` などの（複数要素からなる）リストの場合、名前付き
パラメータの羅列として解釈します。

　make-link 関数はその結果として「リンク情報オブジェクト」を返しますが、そのリンク情報
オブジェクトを make-link 関数自身に渡した場合、そのまま返すようになっています。そのため、
自分で明示的に make-link 関数を使ってリンク情報オブジェクトを作成し、Common Lisp 変数に
格納して複数の図形要素で使用する、ということも可能です。この関数の詳細は [$@ 節](#function make-link)
を参照してください。

${BLANK_PARAGRAPH}

　複数の図形要素をまとめてリンク設定したい場合はどうすれば良いでしょうか。現状では、
defgroup マクロによってグループ化し、use において `:link` パラメータを使うことになります。
以下の例は、[$@ 章](#基本的な図形)冒頭の一覧から抜粋したものです。

<!-- snippet: LINK-SAMPLE-2
(diagram (100 120)
  ;(grid)
  (let ((w  80)
        (h 100))
    (defgroup (w h :rect-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill :white)
      (rect `(,(/ w 2) ,(/ w 2)) 50 50 :fill :skyblue :stroke :blue)
      (text `(,(/ w 2) ,(- h 5)) "四角形" :align :center))
    (use :rect-grp canvas.center :link "${HASH_RECT}")))
-->

```lisp
<!-- expand:LINK-SAMPLE-2 -->
```

```kaavio
<!-- expand:LINK-SAMPLE-2 -->
```
Figure. グループ化と use におけるリンク設定の例

${BLANK_PARAGRAPH}

### 終端マーク
<!-- autolink: [$$](#終端マーク) -->

　終端マークとは、直線やコネクタ、および円弧の端点に描画するマークを指定する情報です。コネクタを使った
例を以下に示します。

<!-- snippet: ENDMARK-SAMPLE
(diagram (300 210)
  (grid)
  (with-options (:stroke :gray :fill :lightgray)
    (circle '( 50  40) 20 :id :C1)
    (circle '(250  40) 20 :id :C2)
    (circle '(150 170) 20 :id :C3)
    (with-options (:stroke :blue)
      (connect :C1 :C2                 :end2 :arrow)
      (connect :C2 :C3 :end1 :triangle :end2 :diamond)
      (connect :C3 :C1 :end1 :circle   :end2 :rect))
    (with-options (:font '(:fill :blue))
      (text '(225  30) ":arrow"    :align :right)
      (text '(230  90) ":triangle" :align :left)
      (text '(185 140) ":diamond"  :align :left)
      (text '(125 150) ":circle"   :align :right)
      (text '( 55  80) ":rect"     :align :right))))
-->

```kaavio
<!-- expand: ENDMARK-SAMPLE -->
```
Figure. 終端マークの例

<!-- collapse:close -->
※上記サンプルのソースはこちら。

```lisp
<!-- expand: ENDMARK-SAMPLE -->
```
<!-- collapse:end -->

　上記のサンプルでは、 `:end2 :arrow` のように、 `:end1` または `:end2` に続けて終端マークの
種類だけを指定しています。もう少し複雑な場合、 `:end1 '(:type :arrow :size :small)` といった
要領で種類とサイズを指定することもできます。実はこれらは全て簡易的な指定方法で、終端マークには
もっと多くの情報が含まれています。以下に説明します。

* `type` は終端マークの形状を指定します。 `:arrow :triangle :diamond :circle :rect` のいずれか、またはカスタム描画 \
関数を指定します。詳細は make-endmark 関数を参照してください。
* `size` は終端マークの大きさです。 `:small :medium :large :xlarge` のいずれか、または数値を指定します。
* `stroke` は終端マークの線を描画するストローク指定です。通常は（指定を省略することで）終端マークが適用される直線や \
コネクタのストロークと同じものを指定します。
* `fill` は終端マークの内部の塗り潰し指定です。省略すると、ストロークと同じ色で塗り潰されます。

${BLANK_PARAGRAPH}

　それぞれについて細かい説明を始める前に、 `:end1 :arrow` といった記述がどのように扱われるのか
を説明する必要があるでしょう。 `:end1 / :end2` によるこれらの指定は、実は全て make-endmark 関数
に渡されます。make-endmark 関数は、渡されたのが単一の値の場合には、キーワードなら種類、数値なら
サイズなどと解釈します。そして `'(:type :arrow :size :small)` などの（複数要素からなる）リスト
の場合、名前付きパラメータの羅列として解釈します。

　make-endmark 関数はその結果として「終端マーク情報オブジェクト」を返しますが、その終端マーク
情報オブジェクトを make-endmark 関数自身に渡した場合、そのまま返すようになっています。そのため、
自分で明示的に make-endmark 関数を使って終端マーク情報オブジェクトを作成し、Common Lisp 変数に
格納して複数の図形要素で使用する、ということも可能です。以下のように。この関数の詳細は 
[$@ 節](#function make-endmark)を参照してください。

```lisp
(let ((em (make-endmark :type :triangle :size :small)))
  (connect  ... :end2 em)
  (connect  ... :end2 em))
```

${BLANK_PARAGRAPH}

#### 終端マークのデフォルト設定

　実際のところ、作図をする上でそれぞれのコネクタの終端がバラバラな形状やサイズで描かれる
ことはないでしょう。わかりやすい図面というのは、同じ種類の図形要素は同じ種類の線で描画され
ているなど、統制が取れているものです。このことは、多くの作図においては「まったく同じ終端
マークの指定を繰り返す場合が多い」ことを意味します。前述の方法で終端マーク情報オブジェクト
を作成して使い回すことも可能ですが、もっとよい方法があります。それは with-endmark-options マクロ
を使った「終端マークのデフォルト設定値の変更」です。以下の例では３つのコネクタを描いています
が、終端マークの種類は with-endmark-options マクロで指定しています。

<!-- with-endmark-options の :end[12] のみ指定する例 -->
<!-- snippet: WITH-ENDMARK-OPTIONS-SAMPLE-1
(diagram (300 150)
  (grid)
  (with-options (:stroke :black :fill :white)
    (circle '( 50  40) 20 :id :C1)
    (circle '(250  40) 20 :id :C2)
    (circle '(150 110) 20 :id :C3)
    (with-endmark-options (:end1 nil :end2 :triangle)
      (connect :C1 :C2 :stroke :red)
      (connect :C2 :C3 :stroke :green)
      (connect :C3 :C1 :stroke :blue))))
-->

```lisp
<!-- expand: WITH-ENDMARK-OPTIONS-SAMPLE-1 -->
```

```kaavio
<!-- expand: WITH-ENDMARK-OPTIONS-SAMPLE-1 -->
```
Figure. with-endmark-options によるデフォルト終端マークの変更 - 1

${BLANK_PARAGRAPH}

　上記の場合、３つのコネクタ全てで `:end1 nil :end2 :triangle` と指定したのと同じことに
なります。結果として、それぞれの終端マークはそれぞれのコネクタのストロークで描画され、
塗り潰しもストロークと同じ色になりました。

　種類は個別に指定して、塗り潰しやサイズのデフォルト値を変更したい場合は以下のように
なります。この場合、それぞれのコネクタの `:end2` で指定指定された `:triangle` などの
キーワードから終端マーク情報オブジェクトを作成する際、with-endmark-options マクロの 
`:size :small :fill :white` が適用されるイメージになります。

<!-- with-endmark-options の :end[12] に「具体的な endmark」を指定する例 -->
<!-- snippet: WITH-ENDMARK-OPTIONS-SAMPLE-2
(diagram (300 150)
  (grid)
  (with-options (:stroke :black :fill :white)
    (circle '( 50  40) 20 :id :C1)
    (circle '(250  40) 20 :id :C2)
    (circle '(150 110) 20 :id :C3)
    (with-endmark-options (:size :small :fill :white)
      (connect :C1 :C2 :stroke :red   :end2 :triangle)
      (connect :C2 :C3 :stroke :green :end2 :diamond)
      (connect :C3 :C1 :stroke :blue  :end2 :circle))))
-->

```lisp
<!-- expand: WITH-ENDMARK-OPTIONS-SAMPLE-2 -->
```

```kaavio
<!-- expand: WITH-ENDMARK-OPTIONS-SAMPLE-2 -->
```
Figure. with-endmark-options によるデフォルト終端マークの変更 - 2

${BLANK_PARAGRAPH}

### ラベル
<!-- autolink: [$$](#ラベル) -->

　ラベルとは、直線やコネクタ、および一部の図形要素に付加できる簡易的なテキスト片のことです。
明示的に位置を指定して text マクロを使用するよりは楽にテキストを付加することができます。
人物とコネクタを使った例を以下に示します。

<!-- snippet: LABEL-SAMPLE-1
(diagram (400 150)
  (grid)
  (with-options (:stroke :navy :fill :lightcyan)
    (person '( 50  50) 30 :label   "actor1")
    (person '(350 100) 30 :label '("actor2" :position :above)))
  (with-options (:stroke :black)
    (connect $2.id $1.id
             :stroke '(:dasharray (5 2))
             :label '("connection" :offset (-40 -15)))))
-->

```kaavio
<!-- expand: LABEL-SAMPLE-1 -->
```
Figure. ラベルの例

<!-- collapse:close -->
※上記サンプルのソースはこちら。

```lisp
<!-- expand: LABEL-SAMPLE-1 -->
```
<!-- collapse:end -->

　上記のサンプルでは、 `:label "actor1"` のように、 `:label` に続けてラベルのテキストだけを
指定しています。もう少し複雑な場合、 `:label '("actor2" :position :above)` や 
`:label '("connection" :offset (-40 -15))` といった要領でテキストと表示位置を指定することも
できます。実はこれらは全て簡易的な指定方法で、ラベルにはもっと多くの情報が含まれています。
パラメータ先頭で常に指定するテキスト以外の名前付きパラメータを以下に説明します。

* `position` はラベルの表示位置を指定します。 `:above :below :left :right` のいずれかを指定します。
* `offset` はラベルの表示位置を調整する値を `(x y)` の要領で指定します。
* `font` はラベルの描画に使用されるフォントを指定します。詳細は [$@ 節](#フォント) を参照してください。

${BLANK_PARAGRAPH}

　それぞれについて細かい説明を始める前に、 `:label "actor1"` といった記述がどのように
扱われるのかを説明する必要があるでしょう。 `:label` によるこれらの指定は、実は全て 
make-label 関数に渡されます。make-label 関数は、渡されたのが単一の値の場合には、それを
ラベルテキストと解釈します。そして `'("text" :position :above :offset (-5 10))` などの
（複数要素からなる）リストの場合、パラメータの羅列として解釈します。

　make-label 関数はその結果として「ラベル情報オブジェクト」を返しますが、そのラベル情報
オブジェクトを make-label 関数自身に渡した場合、そのまま返すようになっています。そのため、
自分で明示的に make-label 関数を使ってラベル情報オブジェクトを作成し、Common Lisp 変数に
格納して複数の図形要素で使用する、ということも可能です。以下のように。この関数の詳細は 
[$@ 節](#function make-label)を参照してください。

```lisp
(let ((txt (make-label "label" :position :above)))
  (connect  ... :label txt)
  (connect  ... :label txt))
```
    
#### position パラメータによるラベル位置の指定

　`:position` パラメータを使うことで、図形要素の上下左右のどこにラベルを配置するかを指定でき
ます。[先ほどの例](F#ラベルの例)では、人物に対して `:label "actor1"` とすることで図形要素の
下にラベルが表示され、 `:label '("actor2" :position :above)` によって上にラベルが表示され
ました。このように、 `:position` パラメータはラベルの表示位置を明示的に指定する場合に使用
します。省略した場合の表示位置はラベルをサポートする図形要素によって異なります。

　`:position` パラメータは、人物などの「幅と高さを持つ」図形要素でのみ有効であることに注意
してください。直線やコネクタのような図形要素では `:position` パラメータは単純に無視され、
ラベルの配置は経路の中央付近に自動的に決定されます。どちらの場合でも、結果としてラベルが表示
される位置に不満を感じるかもしれません。そのような場合は、 `:offset` パラメータで微調整を
することができます。これについては次節を参照してください。

#### offset パラメータによるラベル位置の微調整

　ラベルが配置される場所を微調整したい場合、 `:offset` パラメータを使うことができます。
これは x 軸および y 軸方向にどれだけ移動させるかを指定する値を `(x y)` 形式、つまり
座標指定と同じ方法で指定するものです。[先ほどの例](F#ラベルの例)では、コネクタに対して 
`:label '("connection" :offset (-40 -15))` とすることでラベルの位置調整を行なっています。

　`:offset` パラメータは「 `:position` パラメータ（または自動決定結果）による配置場所に
対して調整をする」ものなので、周辺の状況が変化した場合などには、都度変更が必要になること
に注意してください。

#### ラベルのデフォルト設定

　実際のところ、作図をする上でそれぞれのラベルの位置やフォントがバラバラに指定されること
はないでしょう。わかりやすい図面というのは、同じ種類の図形要素は同じ種類の線で描画されて
いるなど、統制が取れているものです。このことは、多くの作図においては「まったく同じラベル
の指定を繰り返す場合が多い」ことを意味します。前述の方法でラベル情報オブジェクトを作成して
使い回すことも可能ですが、もっとよい方法があります。それは with-label-options マクロを
使った「ラベルのデフォルト設定値の変更」です。以下の例では全部で６つのラベルを描いています
が、そのフォントや位置は with-label-options マクロで指定しています。

<!-- snippet: WITH-LABEL-OPTIONS-SAMPLE
(diagram (300 150)
  (grid)
  (with-options (:stroke :black :fill :white)
    (with-label-options (:font 10)
      (with-label-options (:position :left :offset '(-5 0))
        (person '( 70  40) 20 :label "actor1" :id :P1)
        (person '( 70 110) 20 :label "actor2" :id :P2))
      (with-label-options (:position :right :offset '(5 0))
        (person '(230  40) 20 :label "actor3" :id :P3)
        (person '(230 110) 20 :label "actor4" :id :P4))
      (with-label-options (:offset '(0 -5))
        (connect :P1 :P3 :end2 :arrow :label "P1 - P3")
        (connect :P2 :P4 :end2 :arrow :label "P2 - P4")))))
-->

```lisp
<!-- expand: WITH-LABEL-OPTIONS-SAMPLE -->
```

```kaavio
<!-- expand: WITH-LABEL-OPTIONS-SAMPLE -->
```
Figure. with-label-options によるラベル設定の変更

${BLANK_PARAGRAPH}

　ラベルのテキストは通常それぞれで異なるため、with-label-options マクロで指定することはできません。
指定可能なのは `:position :offset :font` だけです{{fn:`:position` もラベル毎に異なる場合が多いですが、 \
きれいに整列した作図だと同じになる場合もあるので with-label-options マクロでサポートしています。}}。

${BLANK_PARAGRAPH}

### 補助線
<!-- autolink: [$$](#補助線) -->

　円弧やベジェ曲線など、一部の要素はパラメータの指定が難しく感じられるかもしれません。
これは、指定する位置（制御点など）と実際に描画されるもの（曲線など）が異なるからです。
このような図形要素では、補助線の表示がサポートされています。これは作図を支援するための
機能で、 `:debug` パラメータに色名を指定することで調整に役立つ線を表示してくれます。
二次ベジェ曲線での例を以下に示します。

```kaavio
<!-- expand: 2D-CURVE-DEBUG-SAMPLE -->
```
Figure. 二次ベジェ曲線での補助線の例

　なお、色名の指定が面倒であれば `:debug t` とすることもできます。この場合、デフォルト
の色が使用されます。

## UML

　kaavio は、UML を（一部）サポートしています。

### アクティビティ図
<!-- autolink: [$$](#アクティビティ図) -->

　UML アクティビティ図の例を以下に示します。

<!-- snippet: UML-ACTIVITY-DIAGRAM-SAMPLE
(diagram (800 270)
  (grid)
  (with-theme (:uml-activity-default)
    (uml-activity-start '(30 60) :id :start)
    (uml-action   (x+ start.cc  90) "step1" :rake t :id :step1)
    (uml-note (xy+ step1.cc -30 80) 130 60
              "action with~%rake icon."
              :targets :step1 :keyword "memo")
    (uml-decision (x+ step1.cc 120)
                  :text "check~%status" :width 80 :height 40 :id :check)
    (uml-signal   (y+ check.center 100) :send
                  "signal~%error" :direction :left :id :signal)
    (uml-action (xy+ check.center 290 40) "step2"
                :id :step2 :width 400 :height 160 :contents t)
    (labels ((flow-chain (from &rest rest)
               (when rest
                 (uml-flow from (car rest))
                 (apply #'flow-chain (car rest) (cdr rest)))))
      (with-subcanvas-of (:step2)
        (uml-activity-start (x+ canvas.cl 20) :id :start2)
        (uml-fork           (x+ start2.cc 40) :h :id :fork)
        (uml-action (xy+ $1.cc 80 -30)   "sub1" :id :sub1)
        (uml-action (x+  $1.cc 120)      "sub2" :id :sub2)
        (uml-action (xy+ fork.cc 140 30) "sub3" :id :sub3)
        (uml-join           (xy+ sub2.cc 80 30) :h :id :join)
        (uml-activity-final (x+  join.cc 40) :id :final2)
        (flow-chain :start2 :fork :sub1 :sub2 :join :final2)
        (flow-chain :fork :sub3 :join))
      (uml-merge (y+ step2.bc 60) :id :merge)
      (uml-activity-final (x+ merge.center 180) :id :final)
      (flow-chain :start  :step1 :check)
      (uml-flow   :check  :signal
                  :spec '(:guard "NG" :offset ( 5 -15) :font 9))
      (uml-flow   :check  :step2 :style :RL1
                  :spec '(:guard "OK" :offset (-9   0) :font 9))
      (uml-flow   :signal :merge :style :BL)
      (flow-chain :step2  :merge :final))))
-->

```kaavio
<!-- expand: UML-ACTIVITY-DIAGRAM-SAMPLE -->
```
Figure. UML アクティビティ図の例

<!-- collapse:close -->
※上記サンプルのソースはこちら。

```lisp
<!-- expand: UML-ACTIVITY-DIAGRAM-SAMPLE -->
```
<!-- collapse:end -->

${BLANK_PARAGRAPH}

　UML アクティビティ図で使用する構成要素は以下を参照してください。

* [$$](#uml-action)
* [$$](#uml-activity-final)
* [$$](#uml-activity-start)
* [$$](#uml-connector)
* [$$](#uml-decision)
* [$$](#uml-expansion-region)
* [$$](#uml-flow)
* [$$](#uml-flow-final)
* [$$](#uml-fork)
* [$$](#uml-frame)
* [$$](#uml-join)
* [$$](#uml-merge)
* [$$](#uml-note)
* [$$](#uml-partition)
* [$$](#uml-pin)
* [$$](#uml-signal)
* [$$](#uml-time-event)

### クラス図
<!-- autolink: [$$](#クラス図) -->

　UML クラス図の例を以下に示します。

<!-- snippet: UML-CLASS-DIAGRAM-SAMPLE
(diagram (800 270)
  (grid)
  (with-theme (:uml-class-default)
    (uml-package (y+ canvas.tc 100) "GUI"
      :width 140 :height 110 :stroke :brown :fill :beige :id :gui
      :contents
      ((uml-class canvas.cc "Application" :id :app)))
    (uml-package (xy+ canvas.tc 200 100) "sys"
      :width 140 :height 110 :stroke :brown :fill :beige :id :sys
      :contents
      ((uml-class canvas.cc "Thread" :id :thread)))
    (uml-class (y+ app.cc    120) "TheApp" :id :theapp
      :width 120 :active t
      :operations
      ((:public "Run" :type :int)))
    (uml-note (xy+ theapp.cc -180 -80) 130 60
                "TheApp is~%active class."
                :fill :ivory :stroke :olive
                :targets :theapp :keyword "memo")
    (uml-class (y+ thread.cc 120) "Worker" :keyword '("thread" :font 9) :id :worker)
    (uml-generalization :theapp :app)
    (uml-generalization :worker :thread)
    (uml-aggregation :theapp :worker :arrow t)
    (uml-interface (x+ theapp.cc -170) "LogReceiver" :fill :lightgray :id :logrcv)
    (uml-class (x+ logrcv.cc -150) "Logger" :id :logger)
    (uml-interface-request :theapp :logrcv)
    (uml-association :logger :logrcv)))
-->

```kaavio
<!-- expand: UML-CLASS-DIAGRAM-SAMPLE -->
```
Figure. UML クラス図の例

<!-- collapse:close -->
※上記サンプルのソースはこちら。

```lisp
<!-- expand: UML-CLASS-DIAGRAM-SAMPLE -->
```
<!-- collapse:end -->

${BLANK_PARAGRAPH}

　UML クラス図で使用する構成要素は以下を参照してください。

* [$$](#uml-aggregation)
* [$$](#uml-association)
* [$$](#uml-class)
* [$$](#uml-composition)
* [$$](#uml-dependency)
* [$$](#uml-frame)
* [$$](#uml-generalization)
* [$$](#uml-interface)
* [$$](#uml-interface-request)
* [$$](#uml-stereotype-info)
* [$$](#uml-multiplicity-info)
* [$$](#uml-note)
* [$$](#uml-package)
* [$$](#uml-realization)
* [$$](#uml-role-info)

### コンポーネント図
<!-- autolink: [$$](#コンポーネント図) -->

　UML コンポーネント図の例を以下に示します。これは書籍「UML モデリングのエッセンス第３版」の図を
再現したものです。

<!-- snippet: UML-COMPONENT-DIAGRAM-SAMPLE
(diagram (700 400)
  (grid)
  (with-theme (:uml-component-default)
    (with-options (:font 10)
      (uml-component (xy+ canvas.tl 80 50) ": till" :id :till)
      (uml-interface (xy+ $1.cc 0 70)
                     '("sales message" :position :right :offset (5 0)) :id :sales-msg1)
      (uml-component (xy+ $1.cc 0 60) "~%: message~%queue" :id :msg-que)
      (uml-interface-request :till :sales-msg1)
      (uml-association :msg-que :sales-msg1)
      (uml-interface (xy+ msg-que.tr 50 (/ msg-que.height 4))
                     '("sales~%message" :position :below :offset (0 5)) :id :sales-msg2)
      (uml-interface-request :msg-que :sales-msg2 :style :R1L2)
      (uml-component (xy+ sales-msg2.cc 270 0) ": Sales Server"
                     :width 420 :height 150 :contents t :id :sales-svr)
      (uml-port sales-svr.CL :id :port1)
      (uml-association :port1 :sales-msg2)
      (with-subcanvas-of (:sales-svr)
        (uml-component (xy+ port1.cc 100 0)
                       "~%: transaction~%processor" :height 70 :id :tran-proc)
        (uml-interface (xy+ $1.cc  120 0) "" :id :tmp-interface)
        (uml-component (xy+ $1.cc  120 0)
                       "~%: accounting~%driver" :height 70 :id :account-drvr)
        (uml-interface-request :tran-proc :tmp-interface)
        (uml-association :account-drvr :tmp-interface)
        (uml-dependency :port1 :tran-proc))
      (uml-port (x+ sales-svr.B3 25) :id :port2)
      (uml-dependency :account-drvr :port2)
      (uml-interface (xy+ port2.cc 0 45)
                     '("receivables" :position :left :offset (-5 0)) :id :receivables)
      (uml-interface-request :port2 :receivables)
      (uml-component (xy+ receivables.cc 0 65)
                     "~%: accounting~%system" :height 70 :id :account-sys)
      (uml-association :account-sys :receivables))))
-->

```kaavio
<!-- expand: UML-COMPONENT-DIAGRAM-SAMPLE -->
```
Figure. UML コンポーネント図の例

<!-- collapse:close -->
※上記サンプルのソースはこちら。

```lisp
<!-- expand: UML-COMPONENT-DIAGRAM-SAMPLE -->
```
<!-- collapse:end -->

${BLANK_PARAGRAPH}

　UML コンポーネント図で使用する構成要素は以下を参照してください。

* [$$](#uml-component)
* [$$](#uml-dependency)
* [$$](#uml-frame)
* [$$](#uml-stereotype-info)
* [$$](#uml-note)
* [$$](#uml-package)
* [$$](#uml-port)
* [$$](#uml-interface)
* [$$](#uml-interface-request)
* [$$](#uml-association)
* [$$](#uml-realization)

### パッケージ図
<!-- autolink: [$$](#パッケージ図) -->

　UML パッケージ図の例を以下に示します。

<!-- snippet: UML-PACKAGE-DIAGRAM-SAMPLE
(diagram (700 300)
  (grid)
  (with-theme (:uml-package-default)
	(with-options (:font 10)
	  (uml-package (xy+ canvas.tl 100  60) "Application")
	  (uml-package (xy+ $1.cc     260   0) "Database~%Gateway" :id :db-gateway)
	  (uml-dependency $2.id $1.id)
	  (uml-package (xy+ $2.cc       0 160) "SQL Server~%Gateway")
	  (uml-realization $1.id :db-gateway)
	  (uml-package (xy+ $2.cc    -200   0) "Oracle~%Gateway")
	  (uml-realization $1.id :db-gateway :style :TB1)
	  (uml-package (xy+ $4.cc     200   0) "Tset Stub~%Gateway")
	  (uml-realization $1.id :db-gateway :style :TB3))))
-->

```kaavio
<!-- expand: UML-PACKAGE-DIAGRAM-SAMPLE -->
```
Figure. UML パッケージ図の例

<!-- collapse:close -->
※上記サンプルのソースはこちら。

```lisp
<!-- expand: UML-PACKAGE-DIAGRAM-SAMPLE -->
```
<!-- collapse:end -->

${BLANK_PARAGRAPH}

　UML パッケージ図で使用する構成要素は以下を参照してください。

* [$$](#uml-stereotype-info)
* [$$](#uml-dependency)
* [$$](#uml-frame)
* [$$](#uml-note)
* [$$](#uml-package)
* [$$](#uml-realization)

### 状態マシン図
<!-- autolink: [$$](#状態マシン図) -->

　UML 状態マシン図の例を以下に示します。

<!-- snippet: UML-STATEMACHINE-DIAGRAM-SAMPLE
(diagram (600 300)
  (grid)
  (with-theme (:uml-statemachine-default)
    (with-options (:font '(:width-spice 0.8))
      (uml-state (xy+ canvas.tc   0  40) "Show~%Connections" :id :s1 :width 100)
      (uml-state (xy+ $1.cc       0 160) "Enter Connection Details"
                 :id :s2 :width 560 :height 140 :contents t)
      (with-subcanvas-of (:s2)
        (uml-state-begin (x+ canvas.cl 30) :id :s3)
        (uml-state (xy+ $1.cc  90 0) "Enter phone~%number"    :height 40 :id :s4)
        (uml-state (xy+ $1.cc 180 0) "Choose shared~%or solo" :height 40 :id :s5)
        (uml-state (xy+ $1.cc 180 0) "Enter name"             :height 40 :id :s6)
        (uml-transition :s3 :s4)
        (uml-transition :s4 :s5 :style :R1L1 :spec :next)
        (uml-transition :s5 :s4 :style :L3R3 :spec :back)
        (uml-transition :s5 :s6 :style :R1L1 :spec :next)
        (uml-transition :s6 :s5 :style :L3R3 :spec :back))
      (uml-transition :s1 (x+ s2.tc -25) :style :B1T
                      :spec '(:trigger "new"    :offset (-30 -20)))
      (uml-transition (x+ s2.tc  25) :s1 :style :TB3
                      :spec '(:trigger "cancel" :offset ( 50  20)))
      (uml-transition :s6 :s1 :style :TR
                      :spec '(:trigger :save :offset (35 50))))))
-->

```kaavio
<!-- expand: UML-STATEMACHINE-DIAGRAM-SAMPLE -->
```
Figure. UML 状態マシン図の例

<!-- collapse:close -->
※上記サンプルのソースはこちら。

```lisp
<!-- expand: UML-STATEMACHINE-DIAGRAM-SAMPLE -->
```
<!-- collapse:end -->

${BLANK_PARAGRAPH}

　UML 状態マシン図で使用する構成要素は以下を参照してください。

* [$$](#uml-frame)
* [$$](#uml-note)
* [$$](#uml-state)
* [$$](#uml-state-begin)
* [$$](#uml-state-end)
* [$$](#uml-state-history)
* [$$](#uml-transition)
* [$$](#uml-transition-spec)

### ユースケース図
<!-- autolink: [$$](#ユースケース図) -->

　UML ユースケース図の例を以下に示します。

<!-- snippet: UML-USECASE-DIAGRAM-SAMPLE
(diagram (600 200)
  (grid)
  (with-theme (:uml-usecase-default)
	(with-options (:font 10)
	  (uml-actor   (xy+ canvas.tl 140  60) "user")
	  (uml-usecase (xy+ $1.cc     200   0) "make diagram." :id :case1)
	  (uml-association $2.id $1.id)
	  (uml-usecase (xy+ $2.cc      80  80) "make UML~%diagram." :id :case2)
	  (uml-generalization $1.id $3.id))))
-->

```kaavio
<!-- expand: UML-USECASE-DIAGRAM-SAMPLE -->
```
Figure. UML ユースケース図の例

<!-- collapse:close -->
※上記サンプルのソースはこちら。

```lisp
<!-- expand: UML-USECASE-DIAGRAM-SAMPLE -->
```
<!-- collapse:end -->

${BLANK_PARAGRAPH}

　UML ユースケース図で使用する構成要素は以下を参照してください。


* [$$](#uml-actor)
* [$$](#uml-association)
* [$$](#uml-frame)
* [$$](#uml-generalization)
* [$$](#uml-note)
* [$$](#uml-usecase)

### 配置図
<!-- autolink: [$$](#配置図) -->

　UML 配置図の例を以下に示します。

<!-- snippet: UML-DEPLOYMENT-DIAGRAM-SAMPLE
(diagram (600 300)
  (grid)
  (with-theme (:uml-deployment-default)
    (uml-node (xy+ canvas.tl  80  70) "BrowserClient" :width 120 :id :n1)
    (uml-node (xy+ canvas.tl 220  70) "Rich Client"   :width 120 :id :n2)
    (uml-node (xy+ canvas.tl 150 220) "Web Server"    :width 120 :id :n3)
    (uml-association :n3 :n1 :style :T1B :name '("http/internet" :offset (-30  0)))
    (uml-association :n3 :n2 :style :T3B :name '("http/LAN"      :offset ( 30 23)))
    (uml-node (xy+ n3.cc 280 -65) "Application Server" :width 160 :height 260 :contents t :id :n4)
    (uml-association :n3 :n4 :style :RL3 :name '("Java RMI/LAN" :offset (0 5)))
    (with-subcanvas-of (:n4)
      (uml-artifact (y+ canvas.tc  40) "JoveGL.exe" :height 50)
      (uml-node     (y+ $1.cc      70) "EJB Container" :width 130 :height 50 :id :n5)
      (uml-node     (y+ $1.cc      80) "Oracle DBMS"   :width 130 :height 50 :id :n6)
      (uml-association :n6 :n5 :name '("JDBC" :offset (-10 -5))))))
-->

```kaavio
<!-- expand: UML-DEPLOYMENT-DIAGRAM-SAMPLE -->
```
Figure. UML 配置図の例

<!-- collapse:close -->
※上記サンプルのソースはこちら。

```lisp
<!-- expand: UML-DEPLOYMENT-DIAGRAM-SAMPLE -->
```
<!-- collapse:end -->

${BLANK_PARAGRAPH}

　UML 配置図で使用する構成要素は以下を参照してください。

* [$$](#uml-artifact)
* [$$](#uml-association)
* [$$](#uml-dependency)
* [$$](#uml-frame)
* [$$](#uml-node)
* [$$](#uml-note)

### UML のダイアグラム要素

#### uml-action
<!-- autolink: [$$](#uml-action) -->

　uml-action は UML のアクティビティ図におけるアクションを表記するための図形要素です。
ほぼテキストボックスと同じように使えますが、リンク明示のためのレーキアイコンなどが用意
されています。

<!-- snippet: UML-ACTION-SAMPLE
(diagram (400 220)
  (grid)
  (with-theme (:uml-activity-default)
    (uml-activity-start '(50 40) :id :start)
    (uml-action (x+ start.cc 100) "step1" :id :step1)
    (uml-action (x+ step1.cc 150) "step2" :rake t :id :step2)
    (uml-action (xy+ canvas.cc -60 30) "step3" :id :step3 :width 250 :height 110
        :contents
        ((uml-action (x+ canvas.cc -70) "sub-step1")
         (uml-action (x+ canvas.cc  70) "sub-step2")
         (uml-flow $2.id $1.id)))
    (uml-activity-final (xy+ step3.cc 220 50) :id :final)
    (uml-flow :start :step1)
    (uml-flow :step1 :step2)
    (uml-flow :step2 :step3 :style :BR1)
    (uml-flow :step3 :final)))
-->

```kaavio
<!-- expand: UML-ACTION-SAMPLE -->
```
Figure. uml-action のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-ACTION-SAMPLE -->
```

　`:contents` パラメータと with-subcanvas-of マクロは通常は同じように使えますが、
uml-action では違いが存在します。 `:contents` で内部を描画する場合、uml-action は
テキストを上端付近に描画しますが、 `:contents` を使わずに with-subcanvas-of マクロを
使用した場合、テキストは中央に描画されます。この問題を解決するため、uml-action で
は `:contents t` と指定することでテキストを上端付近に描画させることができます。

　詳細は以下を参照してください。

* uml-action マクロ
* with-uml-action-options マクロ

#### uml-activity-final
<!-- autolink: [$$](#uml-activity-final) -->

　uml-activity-final は UML のアクティビティ図における終了状態を表記するための図形要素です。

<!-- snippet: UML-ACTIVITY-FINAL-SAMPLE
(diagram (200 80)
  (grid)
  (with-theme (:uml-activity-default)
    (uml-action (x+ canvas.cc -40) "action" :id :step)
    (uml-activity-final (x+ $1.cc 100) :id :final)
    (uml-flow :step :final)))
-->

```kaavio
<!-- expand: UML-ACTIVITY-FINAL-SAMPLE -->
```
Figure. uml-activity-final のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-ACTIVITY-FINAL-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-activity-final マクロ
* with-uml-activity-final-options マクロ

#### uml-activity-start
<!-- autolink: [$$](#uml-activity-start) -->

　uml-activity-start は UML のアクティビティ図における開始状態を表記するための図形要素です。

<!-- snippet: UML-ACTIVITY-START-SAMPLE
(diagram (200 80)
  (grid)
  (with-theme (:uml-activity-default)
    (uml-activity-start (x+ canvas.cc -60) :id :start)
    (uml-action (x+ $1.cc 100) "action" :id :step)
    (uml-flow :start :step)))
-->

```kaavio
<!-- expand: UML-ACTIVITY-START-SAMPLE -->
```
Figure. uml-activity-start のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-ACTIVITY-START-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-activity-start マクロ
* with-uml-activity-start-options マクロ

#### uml-actor
<!-- autolink: [$$](#uml-actor) -->

　uml-actor は UML のユースケース図におけるアクターを表記するための図形要素です。

<!-- このダイアグラムは uml-usecase のところでも使われてることに注意。 -->
<!-- snippet: UML-ACTOR-USECASE-SAMPLE
(diagram (240 80)
  (grid)
  (with-theme (:uml-usecase-default)
    (uml-actor   (x+ canvas.cl  30) "actor"   :id :actor)
    (uml-usecase (x+ canvas.cr -60) "usecase" :id :usecase :height 50)
    (uml-association :actor :usecase)))
-->

```kaavio
<!-- expand: UML-ACTOR-USECASE-SAMPLE -->
```
Figure. uml-actor のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-ACTOR-USECASE-SAMPLE -->
```

　第二パラメータで指定するアクターの名称は、ラベル形式での指定になります。

　詳細は以下を参照してください。

* uml-actor マクロ
* with-uml-actor-options マクロ

#### uml-aggregation
<!-- autolink: [$$](#uml-aggregation) -->

　uml-aggregation は UML のクラス図などにおける集約関連を表記するための図形要素です。

<!-- snippet: UML-AGGREGATION-SAMPLE
(diagram (400 80)
  (grid)
  (with-theme (:uml-class-default)
    (uml-class   (x+ canvas.cl  50) "foo" :id :foo)
    (uml-class   (x+ canvas.cr -50) "bar" :id :bar)
    (uml-aggregation :foo :bar
                     :arrow      t
                     :stereotype '("stereotype" :font 9)
                     :name       '("name" :offset (0 5))
                     :role1      "role1"
                     :role2      "role2"
                     :mult1      1
                     :mult2      '(0 . :*))))
-->

```kaavio
<!-- expand: UML-AGGREGATION-SAMPLE -->
```
Figure. uml-aggregation のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-AGGREGATION-SAMPLE -->
```

　必須パラメータは接続元／接続先のみで、それ以外の名前付きパラメータは基本的に 
uml-association と同じです。ただし、矢印は関連先に表示するか否かの指定になる
ため、（uml-association の `:arrows` と異なり）真偽値を指定する `:arrow` パラ
メータとなります。詳細は以下を参照してください。

* uml-association
* uml-aggregation マクロ
* with-uml-aggregation-options マクロ

#### uml-association
<!-- autolink: [$$](#uml-association) -->

　uml-association は UML のクラス図などにおける関連を表記するための図形要素です。

<!-- snippet: UML-ASSOCIATION-SAMPLE
(diagram (400 80)
  (grid)
  (with-theme (:uml-class-default)
    (uml-class   (x+ canvas.cl  50) "foo" :id :foo)
    (uml-class   (x+ canvas.cr -50) "bar" :id :bar)
    (uml-association :foo :bar
                     :arrows     1
                     :stereotype '("stereotype" :font 9)
                     :name       '("name" :offset (0 5))
                     :role1      "role1"
                     :role2      "role2"
                     :mult1      1
                     :mult2      '(0 . :*))))
-->

```kaavio
<!-- expand: UML-ASSOCIATION-SAMPLE -->
```
Figure. uml-association のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-ASSOCIATION-SAMPLE -->
```

　uml-association はコネクタとほぼ同じように使えますが、終端に表示する情報などを
指定可能になっています。矢印に関しては `:arrows` パラメータで 0,1,2 のいずれかを
指定します。これは、矢印無しか、接続先のみか、両端かを意味します。また、ロール情
報や多重度情報を `:role1 :role2 :mult1 :mult2` といった名前付きパラメータで指定
します。詳細は以下を参照してください。

* uml-role-info
* uml-multiplicity-info
* uml-association マクロ
* with-uml-association-options マクロ

#### uml-artifact
<!-- autolink: [$$](#uml-artifact) -->

　uml-artifact は UML の配置図における生成物を表記するための図形要素です。

<!-- snippet: UML-ARTIFACT-SAMPLE
(diagram (240 120)
  (grid)
  (drop-shadow)
  (with-theme (:uml-deployment-default)
    (uml-artifact canvas.cc "sample.exe"
                  :width 120 :height 50 :filter :drop-shadow)))
-->

```kaavio
<!-- expand: UML-ARTIFACT-SAMPLE -->
```
Figure. uml-artifact のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-ARTIFACT-SAMPLE -->
```

　上記では指定されていませんが、右上に表示されるアイコンの位置などの調整には `:icon`
パラメータを指定します（[$@ 節](#uml-icon-setting)参照）。詳細は以下を参照してください。

* uml-icon-setting
* uml-artifact マクロ
* with-uml-artifact-options マクロ

#### uml-class
<!-- autolink: [$$](#uml-class) -->

　uml-class は UML のクラス図におけるクラスを表記するための図形要素です。 クラス
名だけの単純なボックスでも、属性・操作・責務などの記述を伴ったボックスでも描画で
きます。

<!-- snippet: UML-CLASS-SAMPLE
(diagram (500 180)
  (grid)
  (with-theme (:uml-class-default)
    (uml-class (xy+ canvas.tl  60 40) "foo")
    (uml-class (x+  $1.cc     120) "abstract~%class" :abstract t)
    (uml-class (y+  $2.cc     100)   "active~%class" :active t)
    (uml-class (y+  $2.cc     100) "template~%class"
               :width 90 :height 40 :template " T ")
    (uml-class (xy+ $4.cc 60   50) "bar" :width 90
                                   :keyword '("keyword" :font 9))
    (uml-class (x+  canvas.cc 120) "class with~%additional boxes"
               :width 160
               :attributes
               ((:private "m_status" :type :uint32_t))
               :operations
               ((:public "Run" :type :int)
                (:public "Abort"))
               :responsibilities
               "blah blah blah~%blah blah blah~%blah blah blah")))
-->

```kaavio
<!-- expand: UML-CLASS-SAMPLE -->
```
Figure. uml-class のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-CLASS-SAMPLE -->
```

　必須のパラメータは座標とクラス名だけです。それ以外は名前付きパラメータで、抽象
クラスやクラステンプレートなどの指定を行ないます。属性や操作の指定は `:attributes` と 
`:operations` でクラス属性情報またはクラス操作情報のリストを指定します。これらに
ついては、uml-class-attribute および uml-class-operation を参照してください。

　詳細は以下を参照してください。

* uml-class-attribute
* uml-class-operation-param
* uml-class-operation
* uml-class マクロ
* with-uml-class-options マクロ

#### uml-class-attribute
<!-- autolink: [$$](#uml-class-attribute) -->

　uml-class-attribute は UML のクラス表記における属性欄を記述するための、可視性、
名前、型、多重度などからなる情報です。以下に例を示します。

<!-- snippet: UML-CLASS-ATTRIBUTE-SAMPLE
(diagram (300 150)
  (grid)
  (with-theme (:uml-class-default)
    (uml-class canvas.cc "foo"
               :width 160
               :attributes
               (:<<section1>>
                (:private "m_lastMsg" :type :string)
                :etc
                :<<section2>>
                (:private "m_status" :type :StatusT)
                (:private "m_data"   :type :uint32_t :multiplicity 10)
                :etc))))
-->

```kaavio
<!-- expand: UML-CLASS-ATTRIBUTE-SAMPLE -->
```
Figure. uml-class-attribute のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-CLASS-ATTRIBUTE-SAMPLE -->
```

　uml-class マクロの `:attributes` パラメータには、属性情報のボックスに記載する
情報をリストで指定します。このリストの要素は以下のいずれかです。

* キーワードを示すキーワードシンボル
    * これは文字列 `<<` で始まり、文字列 `>>` で終わる名前を持ちます
* 属性情報を表現するリスト
    * これは make-uml-class-attribute 関数に渡されます
* 省略を示すキーワードシンボル `:etc`
    * これは「ここに記載されているものが全てではない」ことを表現する `...` になります

　詳細は以下を参照してください。

* make-uml-class-attribute 関数

#### uml-class-operation
<!-- autolink: [$$](#uml-class-operation) -->

　uml-class-operation は UML のクラス表記における操作欄を記述するための、可視性、
名前、パラメータ、型などからなる情報です。以下に例を示します。

<!-- snippet: UML-CLASS-OPERATION-SAMPLE
(diagram (300 170)
  (grid)
  (with-theme (:uml-class-default)
    (uml-class canvas.cc "foo"
               :width 250
               :operations
               (:<<constructor>>
                (:public "foo")
                (:public "foo" :parameters ("foo&"))
                :<<accessors>>
                (:public "GetData" :type :uint32_t :property :frozen)
                :<<operations>>
                (:public "DoSomething" :parameters (:uint32_t) :type :boolean)
                :etc))))
-->

```kaavio
<!-- expand: UML-CLASS-OPERATION-SAMPLE -->
```
Figure. uml-class-operation のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-CLASS-OPERATION-SAMPLE -->
```

　uml-class マクロの `:operations` パラメータには、操作情報のボックスに記載する
情報をリストで指定します。このリストの要素は以下のいずれかです。

* キーワードを示すキーワードシンボル
    * これは文字列 `<<` で始まり、文字列 `>>` で終わる名前を持ちます
* 操作情報を表現するリスト
    * これは make-uml-class-operation 関数に渡されます
* 省略を示すキーワードシンボル `:etc`
    * これは「ここに記載されているものが全てではない」ことを表現する `...` になります

　詳細は以下を参照してください。

* uml-class-operation-param
* make-uml-class-operation 関数

#### uml-class-operation-param
<!-- autolink: [$$](#uml-class-operation-param) -->

　uml-class-operation-param は UML のクラス操作情報におけるパラメータを記述するための、
名前、型、デフォルト値などからなる情報です。以下に例を示します。

<!-- snippet: UML-CLASS-OPERATION-PARAM-SAMPLE
(diagram (400 100)
  (grid)
  (with-theme (:uml-class-default)
    (uml-class canvas.cc "foo"
               :width 320
               :operations
               ((:public "Sample" :type :bool
                 :parameters
                 ((:prm1 :type :string)
                  (:prm2 :type :int :default 0)
                  :etc))))))
-->

```kaavio
<!-- expand: UML-CLASS-OPERATION-PARAM-SAMPLE -->
```
Figure. uml-class-operation-param のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-CLASS-OPERATION-PARAM-SAMPLE -->
```

　make-uml-class-operation 関数の `:parameters` パラメータには、操作情報のパラメータ
部分に記載する情報をリストで指定します。このリストの要素は以下のいずれかです。

* パラメータ情報を表現するリスト
    * これは make-uml-class-operation-param 関数に渡されます
* 省略を示すキーワードシンボル `:etc`
    * これは「ここに記載されているものが全てではない」ことを表現する `...` になります

　詳細は以下を参照してください。

* uml-class-operation
* make-uml-class-operation-param 関数

#### uml-component
<!-- autolink: [$$](#uml-component) -->

　uml-component は UML のコンポーネント図などにおけるコンポーネントを表記するための
図形要素です。

<!-- snippet: UML-COMPONENT-SAMPLE
(diagram (240 120)
  (grid)
  (drop-shadow)
  (with-theme (:uml-component-default)
    (uml-component canvas.cc "somewhat.dll"
                   :width 120 :height 60 :filter :drop-shadow)))
-->

```kaavio
<!-- expand: UML-COMPONENT-SAMPLE -->
```
Figure. uml-component のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-COMPONENT-SAMPLE -->
```

　上記では指定されていませんが、右上に表示されるアイコンの位置などの調整には `:icon`
パラメータを指定します（[$@ 節](#uml-icon-setting)参照）。詳細は以下を参照してください。

* uml-icon-setting
* uml-component マクロ
* with-uml-component-options マクロ

#### uml-composition
<!-- autolink: [$$](#uml-composition) -->

　uml-composition は UML のクラス図などにおけるコンポジション関連を表記するための図形要素です。

<!-- snippet: UML-COMPOSITION-SAMPLE
(diagram (400 80)
  (grid)
  (with-theme (:uml-class-default)
    (uml-class   (x+ canvas.cl  50) "foo" :id :foo)
    (uml-class   (x+ canvas.cr -50) "bar" :id :bar)
    (uml-composition :foo :bar
                     :arrow      t
                     :stereotype '("stereotype" :font 9)
                     :name       '("name" :offset (0 5))
                     :role1      "role1"
                     :role2      "role2"
                     :mult1      1
                     :mult2      '(0 . :*))))
-->

```kaavio
<!-- expand: UML-COMPOSITION-SAMPLE -->
```
Figure. uml-composition のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-COMPOSITION-SAMPLE -->
```

　必須パラメータは接続元／接続先のみで、それ以外の名前付きパラメータは基本的に 
uml-association と同じです。ただし、矢印は関連先に表示するか否かの指定になる
ため、（uml-association の `:arrows` と異なり）真偽値を指定する `:arrow` パラ
メータとなります。詳細は以下を参照してください。

* uml-association
* uml-composition マクロ
* with-uml-composition-options マクロ

#### uml-connector
<!-- autolink: [$$](#uml-connector) -->

　uml-connector は UML のアクティビティ図における[コネクタ](#uml-connector)を表記
するための図形要素です。あまり使用されませんが、接続線がどうしても交差してしまう
場合には便利です。

<!-- snippet: UML-CONNECTOR-SAMPLE
(diagram (300 120)
  (grid)
  (with-theme (:uml-activity-default)
    (uml-action (xy+ canvas.center -80 -30) "action1" :id :step1)
    (uml-action (xy+ canvas.center  80  30) "action2" :id :step2)
    (uml-connector (x+ step1.center  130)
                   (x+ step2.center -130) :warp-a :name :A)
    (uml-flow :step1  :warp-a)
    (uml-flow :warp-a :step2)))
-->

```kaavio
<!-- expand: UML-CONNECTOR-SAMPLE -->
```
Figure. uml-connector のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-CONNECTOR-SAMPLE -->
```

　uml-connector はかなり特殊な図形要素で、２点を指定します。指定された２ケ所に同じ円形の
マーカーが描画され、uml-flow などの接続線で `from` と `to` のどちらに指定されたかで
接続先が変わります。詳細は以下を参照してください。

* uml-connector マクロ
* with-uml-connector-options マクロ

#### uml-decision
<!-- autolink: [$$](#uml-decision) -->

　uml-decision は UML のアクティビティ図における判断を表記するための図形要素です。
以下の例では左端の check status というテキストを伴った要素が uml-decision です。
テキストを指定しなければ uml-merge と同じようにひし形が描画されます。

<!-- snippet: UML-DECISION-MERGE-SAMPLE
(diagram (400 150)
  (grid)
  (drop-shadow)
  (with-theme (:uml-activity-default)
    (with-options (:filter :drop-shadow)
      (uml-decision (x+ canvas.cc -120)
                    :text "check~%status" :width 80 :height 40 :id :check)
      (uml-action   (xy+ canvas.cc 40 -40) "action1" :id :act1)
      (uml-action   (xy+ canvas.cc 40  40) "action2" :id :act2)
      (uml-merge    (x+ canvas.cc  150) :id :merge))
    (uml-flow :check :act1 :style :TL :spec "OK")
    (uml-flow :check :act2 :style :BL :spec "NG")
    (uml-flow :act1 :merge :style :RT)
    (uml-flow :act2 :merge :style :RB)))
-->

```kaavio
<!-- expand: UML-DECISION-MERGE-SAMPLE -->
```
Figure. uml-decision のサンプル

　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-DECISION-MERGE-SAMPLE -->
```

　uml-decision はテキストの有無で形状が変化します。そして、どちらの場合でも width と 
height の指定、または with-uml-decision-merge-options マクロで指定されるそれらの
デフォルト値のみでサイズを決定します。つまり、テキスト内容から（テキストボックスのように）
サイズを自動決定したりはしません。このことは、width / height のデフォルト値としてテキ
ストありの場合と無しの場合のどちらを想定した値を設定すべきか、という問題を引き起こします。
ひとまずのところ、デフォルトとしてはテキスト無しのサイズを指定しておき、テキストを指定
する場合は width / height を明示的に指定することをお勧めします。

　詳細は以下を参照してください。

* uml-merge マクロ
* uml-decision マクロ
* with-uml-decision-merge-options マクロ

#### uml-dependency
<!-- autolink: [$$](#uml-dependency) -->

　uml-dependency は UML の各種ダイアグラムにおける依存関係を表記するための図形要素です。

<!-- snippet: UML-DEPENDENCY-SAMPLE
(diagram (400 80)
  (grid)
  (with-theme (:uml-class-default)
    (uml-class   (x+ canvas.cl  50) "foo" :id :foo)
    (uml-class   (x+ canvas.cr -50) "bar" :id :bar)
    (uml-dependency :foo :bar
                    :stereotype '("stereotype" :font 9)
                    :name       '("name" :offset (0 5)))))
-->

```kaavio
<!-- expand: UML-DEPENDENCY-SAMPLE -->
```
Figure. uml-dependency のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-DEPENDENCY-SAMPLE -->
```

　必須パラメータは接続元／接続先のみで、それ以外は名前付きパラメータです。コネク
タとほぼ同じように使えます。詳細は以下を参照してください。

* uml-dependency マクロ
* with-uml-dependency-options マクロ

#### uml-expansion-region
<!-- autolink: [$$](#uml-expansion-region) -->

　uml-expansion-region は UML のアクティビティ図における拡張領域を表記するための
図形要素です。

<!-- snippet: UML-EXPANSION-REGION-SAMPLE
(diagram (300 200)
  (grid)
  (drop-shadow)
  (with-theme (:uml-activity-default)
    (with-options (:filter :drop-shadow)
      (uml-expansion-region canvas.center 260 160 :keyword '(:concurrent :font 10)))))
-->

```kaavio
<!-- expand: UML-EXPANSION-REGION-SAMPLE -->
```
Figure. uml-expansion-region のサンプル

　上記の作図は以下のコードで行なっています。 `stereotype` または `keyword` パラメータ
を指定した場合、デフォルトでは左上に表示されます。この位置は `offset` パラメータで調整
することができます。

* ${{TODO}{上記の調整方法について改善タスクが起票されている。}}

```lisp
<!-- expand: UML-EXPANSION-REGION-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-expansion-region マクロ
* with-uml-expansion-region-options マクロ

#### uml-flow-final
<!-- autolink: [$$](#uml-flow-final) -->

　uml-flow-final は UML のアクティビティ図におけるフロー終了を表記するための図形要素です。

<!-- snippet: UML-FLOW-FINAL-SAMPLE
(diagram (200 80)
  (grid)
  (with-theme (:uml-activity-default)
    (uml-action (x+ canvas.center -40) "action" :id :step)
    (uml-flow-final (x+ $1.center 100) :id :final)
    (uml-flow :step :final)))
-->

```kaavio
<!-- expand: UML-FLOW-FINAL-SAMPLE -->
```
Figure. uml-flow-final のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-FLOW-FINAL-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-flow-final マクロ
* with-uml-flow-final-options マクロ

#### uml-flow
<!-- autolink: [$$](#uml-flow) -->

　uml-flow は UML のアクティビティ図におけるアクション間の遷移を表記するための
図形要素です。

<!-- snippet: UML-FLOW-SAMPLE
(diagram (400 100)
  (grid)
  (drop-shadow)
  (with-theme (:uml-activity-default)
    (with-uml-action-options (:filter :drop-shadow)
      (uml-action '( 60 50) "step1" :id :step1)
      (uml-action '(340 50) "step2" :id :step2)
      (uml-flow :step1 :step2 :spec '(:guard :idle :action "act()")))))
-->

```kaavio
<!-- expand: UML-FLOW-SAMPLE -->
```
Figure. uml-flow のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-FLOW-SAMPLE -->
```

　必須パラメータは接続元／接続先の指定のみで、それ以外は名前付きパラメータです。
そのうち、 `:stereotype :keyword :spec` 以外はほぼコネクタと同じように使えます。
`:spec` は遷移のガード条件やアクションを指定するためのものです。これらについて
の詳細は以下を参照してください。

* uml-flow-spec
* uml-flow マクロ
* with-uml-flow-options マクロ

#### uml-flow-spec
<!-- autolink: [$$](#uml-flow-spec) -->

　uml-flow-spec は UML のアクティビティ図におけるフロー仕様を記述するための、
ガード条件、アクションなどからなる情報です。以下に例を示します。

<!-- snippet: UML-FLOW-SPEC-SAMPLE
(diagram (400 190)
  (grid)
  (drop-shadow)
  (with-theme (:uml-activity-default)
    (with-uml-action-options (:filter :drop-shadow)
      (uml-action '( 60  40) "action1" :id :act1)
      (uml-action '(340  40) "action2" :id :act2)
      (uml-flow :act1 :act2 :spec :on-idle)
      (uml-action '( 60 100) "action3" :id :act3)
      (uml-action '(340 150) "action4" :id :act4)
      (uml-flow :act3 :act4
                :style :RL
                :spec '(:guard  "guard"
                        :action "action()"
                        :offset (-90 -35)
                        :font   (:fill :brown :size 9))))))
-->

```kaavio
<!-- expand: UML-FLOW-SPEC-SAMPLE -->
```
Figure. uml-flow-spec のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-FLOW-SPEC-SAMPLE -->
```

　アクティビティ図におけるフロー仕様は、uml-flow マクロに渡す `:spec` パラメータで
指定します。このパラメータは make-uml-flow-spec 関数に渡されます。単一のキーワード
シンボルや文字列を与えた場合、ガード条件として扱われます。リストを渡せばガード条件
やアクション、表示位置の調整やフォント指定を行なうことができます。

　詳細は以下を参照してください。

* uml-flow
* make-uml-flow-spec 関数

#### uml-fork
<!-- autolink: [$$](#uml-fork) -->

　uml-fork は UML のアクティビティ図におけるフォークを表記するための図形要素です。

<!-- snippet: UML-FORK-SAMPLE
(diagram (340 150)
  (grid)
  (drop-shadow)
  (with-theme (:uml-activity-default)
    (with-options (:filter :drop-shadow)
      (uml-action (x+ canvas.cc -100)"step1" :id :step1)
      (uml-fork   canvas.cc :h :id :fork)
      (uml-action (xy+ canvas.cc 100 -40) "step2" :id :step2)
      (uml-action (xy+ canvas.cc 100  40) "step3" :id :step3))
    (uml-flow :step1  :fork)
    (uml-flow fork.R1 :step2)
    (uml-flow fork.R3 :step3)))
-->

```kaavio
<!-- expand: UML-FORK-SAMPLE -->
```
Figure. uml-fork のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-FORK-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-fork マクロ
* with-uml-fork-join-options マクロ

#### uml-frame
<!-- autolink: [$$](#uml-frame) -->

　uml-frame は UML において汎用的に使用される区分要素です。ダイアグラム全体を包む場合も
ありますし、別図面へのリンクを提示して詳細を省略したりします。

<!-- snippet: UML-FRAME-SAMPLE
(diagram (300 150)
  (grid)
  (drop-shadow)
  (with-theme (:uml-activity-default)
    (uml-frame canvas.center 260 110 "this is frame." :filter :drop-shadow)
    (with-subcanvas-of ($1.id)
      (uml-action (y+ canvas.center 10) "action" :id :act1)
      (uml-activity-start (x+ $1.center -100) :id :start)
      (uml-activity-final (x+ $2.center  100) :id :final)))
  (connector :start :act1 :end2 :arrow)
  (connector :act1 :final :end2 :arrow))
-->

```kaavio
<!-- expand: UML-FRAME-SAMPLE -->
```
Figure. uml-frame のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-FRAME-SAMPLE -->
```

<!-- snippet: UML-FRAME-SAMPLE-2
(diagram (700 220)
  (grid)
  (with-theme (:uml-component-default)
    ;; frame1
    (uml-frame (x+ canvas.cc -180) 300 180 "frame1" :id :f1 :fragments 90)
    (with-uml-frame-fragment (:f1 0)
      (text (y+ canvas.tc 15) "[when foo]" :align :center))
    (with-uml-frame-fragment (:f1 1)
      (text (y+ canvas.tc 15) "[unless foo]" :align :center))
    ;; frame2
    (uml-frame (x+ canvas.cc 180) 300 180 "frame2" :id :f2 :fragments '(60 60))
    (with-uml-frame-fragment (:f2 0)
      (text (y+ canvas.tc 15) "[condition1]" :align :center))
    (with-uml-frame-fragment (:f2 1)
      (text (y+ canvas.tc 15) "[condition2]" :align :center))
    (with-uml-frame-fragment (:f2 2)
      (text (y+ canvas.tc 15) "[condition3]" :align :center))))
-->

　`:fragments` パラメータを使用することで、フレームを複数の領域に分割することができます。
以下に例を示します。分割された個々の領域には、with-uml-frame-fragment マクロを使って
アクセスできます。

```kaavio
<!-- expand: UML-FRAME-SAMPLE-2 -->
```
Figure. fragments パラメータを使った uml-frame のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-FRAME-SAMPLE-2 -->
```


　詳細は以下を参照してください。

* uml-frame マクロ
* with-uml-frame-fragment マクロ
* with-uml-frame-options マクロ

#### uml-generalization
<!-- autolink: [$$](#uml-generalization) -->

　uml-generalization は UML のクラス図などにおける汎化関係を表記するための図形要素です。

<!-- snippet: UML-GENERALIZATION-SAMPLE
(diagram (400 80)
  (grid)
  (with-theme (:uml-class-default)
    (uml-class   (x+ canvas.cl  50) "foo" :id :foo)
    (uml-class   (x+ canvas.cr -50) "bar" :id :bar)
    (uml-generalization :foo :bar
                        :stereotype '("stereotype" :font 9)
                        :name       '("name" :offset (0 5)))))
-->

```kaavio
<!-- expand: UML-GENERALIZATION-SAMPLE -->
```
Figure. uml-generalization のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-GENERALIZATION-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-generalization マクロ
* with-uml-generalization-options マクロ

#### uml-icon-setting
<!-- autolink: [$$](#uml-icon-setting) -->

　uml-icon-setting は UML の描画要素において使用されるアイコンの設定をするための要素です。
以下に例を示します。

<!-- snippet: UML-ICON-SETTING-SAMPLE
(diagram (400 110)
  (grid)
  (with-theme (:uml-component-default)
    (uml-component (x+ canvas.CL  100) "component" :width 120)
    (uml-component (x+ canvas.CR -100) "component" :width 120
                   :icon '(:fill :lightcyan :stroke :navy
                           :size 16 :pivot :TL :offset (17 13)))))
-->

```kaavio
<!-- expand: UML-ICON-SETTING-SAMPLE -->
```
Figure. uml-icon-setting のサンプル

　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-ICON-SETTING-SAMPLE -->
```

　左の uml-component はデフォルトのアイコン設定で描画されていますが、右の uml-component 
は `:icon` パラメータによってアイコン設定を指定されています。指定できるのは塗り潰しと
ストローク、サイズ、位置です。詳細は以下を参照してください。

* make-uml-icon-setting 関数

#### uml-interface
<!-- autolink: [$$](#uml-interface) -->

　uml-interface は UML のクラス図などにおけるインターフェースを表記するための図形要素です。

<!-- snippet: UML-INTERFACE-SAMPLE
(diagram (360 100)
  (grid)
  (with-theme (:uml-class-default)
    (uml-class     (x+ canvas.CL  100) "class")
    (uml-interface (x+ canvas.CR -100) "interface")
    (uml-association $2.id $1.id)))
-->

```kaavio
<!-- expand: UML-INTERFACE-SAMPLE -->
```
Figure. uml-interface のサンプル

　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-INTERFACE-SAMPLE -->
```

詳細は以下を参照してください。

* uml-interface マクロ

#### uml-interface-request
<!-- autolink: [$$](#uml-interface-request) -->

　uml-interface-request は UML のクラス図などにおけるインターフェースのソケットを表記する
ための図形要素です。

<!-- snippet: UML-INTERFACE-REQUEST-SAMPLE
(diagram (360 150)
  (grid)
  (with-theme (:uml-class-default)
    (uml-class     (xy+ canvas.TL 60 40) "foo" :id :foo)
    (uml-class     (x+  foo.cc 240)      "bar" :id :bar)
    (uml-interface (x+  bar.cc -110)     "baz" :id :baz)
    (uml-association       :bar :baz)
    (uml-interface-request :foo :baz)
    (uml-interface-request :foo (xy+ foo.cc 120 60)
                           :style :BL :name '("quux" :offset (0 25)))))
-->

```kaavio
<!-- expand: UML-INTERFACE-REQUEST-SAMPLE -->
```
Figure. uml-interface-request のサンプル

　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-INTERFACE-REQUEST-SAMPLE -->
```

　uml-interface-request には 2 つの使い方があります。ひとつは要求されるインターフェース
がuml-interface で描画されている場合にそれを接続先として指定するもので、上記の例ではクラス 
`foo` からインターフェース `baz` の要求が描かれています。

　もうひとつは接続先として座標を指定することでソケットだけを描画するもので、上記の例では
クラス `foo` からインターフェース `quux` を要求するソケットが描画されています。この場合、
インターフェース名は `:name` パラメータで指定することになります。いずれの場合も、
uml-interface-request は実質的にコネクタなので、 `:style` や `:spacing` パラメータ
で接続線の折れ曲がり方を調整することができます。

　詳細は以下を参照してください。

* uml-interface-request マクロ

#### uml-join
<!-- autolink: [$$](#uml-join) -->

　uml-join は UML のアクティビティ図におけるフォークを表記するための図形要素です。見た目は
[$$](#uml-fork) と同じですが、ジョイン仕様を指定することができます。

<!-- snippet: UML-JOIN-SAMPLE
(diagram (340 150)
  (grid)
  (drop-shadow)
  (with-theme (:uml-activity-default)
    (with-options (:filter :drop-shadow)
      (uml-action (xy+ canvas.cc -100 -40) "step1" :id :step1)
      (uml-action (xy+ canvas.cc -100  40) "step2" :id :step2)
      (uml-join canvas.cc :h :id :join
                :spec '("{joinSpec=blah blah}" :offset (30 8) :font 10))
      (uml-action (x+ canvas.cc 100) "step3" :id :step3))
    (uml-flow :step1 join.L1)
    (uml-flow :step2 join.L3)
    (uml-flow :join  :step3)))
-->

```kaavio
<!-- expand: UML-JOIN-SAMPLE -->
```
Figure. uml-join のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-JOIN-SAMPLE -->
```

　ジョイン仕様は `:spec` パラメータでラベル形式で指定します。詳細は以下を参照してください。

* uml-join マクロ
* with-uml-fork-join-options マクロ

#### uml-merge
<!-- autolink: [$$](#uml-merge) -->

　uml-merge は UML のアクティビティ図において判断による分岐の終了を表記するための
図形要素です。以下の例では右端のひし形の要素が uml-merge です。uml-decision とは
異なり、テキストを指定することはできません。

```kaavio
<!-- expand: UML-DECISION-MERGE-SAMPLE -->
```
Figure. uml-merge のサンプル

　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-DECISION-MERGE-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-decision マクロ
* uml-merge マクロ
* with-uml-decision-merge-options マクロ

#### uml-multiplicity-info
<!-- autolink: [$$](#uml-multiplicity-info) -->

* ${{TODO}{uml-class では uml-multiplicity-info の offset が利用されていない疑いあり。}}

　uml-multiplicity-info は UML のクラスや関連において使用される多重度の記述を
するための要素です。以下に例を示します。

<!-- snippet: UML-MULTIPLICITY-INFO-SAMPLE
(diagram (300 140)
  (grid)
  (drop-shadow)
  (with-theme (:uml-class-default)
    (with-uml-class-options (:width 80 :height 40 :filter :drop-shadow)
      (uml-class (xy+ canvas.tl  60  40) "app" :multiplicity 1)
      (uml-class (xy+ canvas.bl  60 -40) "foo")
      (uml-class (xy+ canvas.br -60 -40) "bar")
      (uml-association $2.id $1.id :arrows 1 :mult2 '(:min 2 :max :* :font 9)))))
-->

```kaavio
<!-- expand: UML-MULTIPLICITY-INFO-SAMPLE -->
```
Figure. uml-multiplicity-info のサンプル

　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-MULTIPLICITY-INFO-SAMPLE -->
```

　uml-class では、多重度は `:multiplicity` パラメータで指定します。関連などでは
両端に多重度の指定がありうるため、 `:mult1 :mult2` で指定を行ないます。いずれに
おいても、指定されたパラメータは make-uml-multiplicity 関数に渡されます。詳細は
以下を参照してください。

* make-uml-multiplicity 関数

#### uml-node
<!-- autolink: [$$](#uml-node) -->

　uml-node は UML の配置図などにおけるノードを表記するための図形要素です。

<!-- snippet: UML-NODE-SAMPLE
(diagram (300 160)
  (grid)
  (drop-shadow)
  (with-theme (:uml-deployment-default)
    (uml-node (x+ canvas.cl  60) "client")
    (uml-node (x+ canvas.cr -90) "server" :width 120 :height 120
      :contents
      ((paragraph (xy+ canvas.tl 5 5)
                  "* Web server~%* Postgre SQL"
                  :align :left :valign :top)))))
-->

```kaavio
<!-- expand: UML-NODE-SAMPLE -->
```
Figure. uml-node のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-NODE-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-node マクロ
* with-uml-node-options マクロ

#### uml-note
<!-- autolink: [$$](#uml-note) -->

　uml-note は UML において汎用的にノートを付加させる図形要素です。ほぼメモと同じように
使えますが、targets パラメータによって複数の図形要素に接続することが可能です。

<!-- snippet: UML-NOTE-SAMPLE
(diagram (400 170)
  (grid)
  (with-theme (:uml-class-default)
    (uml-class '( 80 45) "Foo" :id :foo)
    (uml-class '(300 25) "Bar" :id :bar)
    (uml-note (xy+ bar.center -10 80) 120 60
              "test note."
              :targets '(:foo :bar) :keyword "memo")
    (uml-note (xy+ foo.center 20 80) 160 60
              "this is spec memo.~%multi line is OK."
              :targets :foo :keyword "spec")))
-->

```kaavio
<!-- expand: UML-NOTE-SAMPLE -->
```
Figure. uml-note のサンプル


　上記の作図は以下のコードで行なっています。targets パラメータは、単一の要素だけを指定する
場合はリストである必要はありません。また、図面中で uml-note のスタイルを統一するために 
with-uml-note-options マクロを使用しています。

```lisp
<!-- expand: UML-NOTE-SAMPLE -->
```

　version 0.022 より、targets パラメータに指定する接続先として、図形要素の ID だけでなく
座標の直接指定も可能になりました。図形要素の ID を指定した場合はコネクタで `:style :CC` を
指定したイメージで接続されますが、座標の直接指定では任意の位置に接続線をひくことが可能になり
ます。

　詳細は以下を参照してください。

* uml-note マクロ
* with-uml-note-options マクロ

#### uml-package
<!-- autolink: [$$](#uml-package) -->

　uml-package は UML のパッケージ図などにおけるパッケージを表記するための図形要素です。

<!-- snippet: UML-PACKAGE-SAMPLE
(diagram (300 160)
  (grid)
  (drop-shadow)
  (with-theme (:uml-package-default)
    (uml-package (xy+ canvas.cl  60 10) "app")
    (uml-package (xy+ canvas.cr -90 10) "lib" :width 120 :height 100
      :contents
      ((paragraph (xy+ canvas.tl 5 10)
                  "* pattern match~%* multi threads"
                  :align :left :valign :top)))))
-->

```kaavio
<!-- expand: UML-PACKAGE-SAMPLE -->
```
Figure. uml-package のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-PACKAGE-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-package マクロ
* with-uml-package-options マクロ

#### uml-partition
<!-- autolink: [$$](#uml-partition) -->

　uml-partition は UML のアクティビティ図における区画（パーティション）を表記するための
図形要素です。

<!-- snippet: UML-PARTITION-SAMPLE
(diagram (560 300)
  (grid)
  (with-theme (:uml-activity-default)
    (uml-partition canvas.center '(("FrontEnd" 140)
                                   ("BackEnd"  140)) 540 :id :lanes)
    (with-uml-partition-lane (:lanes "FrontEnd")
      (uml-activity-start (x+ canvas.left 30)  :id :start)
      (uml-action (x+ $1.center 100) "action1" :id :act1)
      (uml-action (x+ $1.center 130) "action3" :id :act3)
      (uml-merge  (x+ $1.center 120)           :id :merge)
      (uml-activity-final (x+ $1.center 90)   :id :final))
    (with-uml-partition-lane (:lanes "BackEnd")
      (uml-action   (y+ act1.center 140)  "action2" :id :act2)
      (uml-decision (y+ act3.center 140)            :id :decision)
      (uml-action   (y+ merge.center 140) "action4" :id :act4))
    (labels ((route (lst)
               (when (and (car lst) (cadr lst))
                 (uml-flow (car lst) (cadr lst))
                 (route (cdr lst)))))
      (route '(:decision :act4 :merge))
      (route '(:start :act1 :act2 :decision :act3 :merge :final)))))
-->

```kaavio
<!-- expand: UML-PARTITION-SAMPLE -->
```
Figure. uml-partition のサンプル

　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-PARTITION-SAMPLE -->
```

${BLANK_PARAGRAPH}

　uml-partition のパラメータとして必須なのは `center rows cols` の３つで、 `rows cols` の指定
の仕方で垂直・水平方向のスイムレーンとマトリクス状の区画を作成することができます。以下に例を示します。

<!-- snippet: UML-PARTITION-SAMPLE-2
(diagram (600 140)
  (grid)
  (with-uml-partition-options (:stroke :black :fill :azure :lines :mid)
    (with-subcanvas ('( 0 0) 200 140)
      (uml-partition canvas.center 120 '((A 60) (B 60) (C 60))))
    (with-subcanvas ('(200 0) 200 140)
      (uml-partition canvas.center '((X 40) (Y 40) (Z 40)) 180))
    (with-subcanvas ('(400 0) 200 140)
      (uml-partition canvas.center
                     '((X 30) (Y 30) (Z 30)) '((A 50) (B 50) (C 50))))))
-->

```kaavio
<!-- expand: UML-PARTITION-SAMPLE-2 -->
```
Figure. uml-partition のサンプル - 2

　上記の作図は以下のコードで行なっています。左の縦方向のスイムレーンでは、 `cols` に `'((A 60) (B 60) (C 60))` を、 
`rows` に `120` を指定しています。中央の横方向のスイムレーンでは、 `cols` に `180` を、 `rows` に 
`'((X 40) (Y 40) (Z 40))` を指定しています。このように `rows cols` の一方を数値にして他方をリストにすると
縦または横のスイムレーンになります。両方をリストにするとマトリクス状の区画になります。

```lisp
<!-- expand: UML-PARTITION-SAMPLE-2 -->
```

　注意が必要なのは、区画の全体サイズを決めるルールです。

* スイムレーンになる場合、
    * `rows` または `cols` に数値を指定した側は、その値がそのまま幅または高さになります
    * レーンの名前部分の領域は、 `header` で指定されたサイズが上記の中で確保されます
    * `rows` または `cols` にリストを指定した側はその幅の合計が幅または高さになります
* マトリクスになる場合、
    * `rows` または `cols` に指定したリストに含まれる幅の合計に `header` の指定値を加えたものが幅または高さになります

${BLANK_PARAGRAPH}

　uml-partition はテーブルをベースに作成されていますが、描画される「罫線」は `lines` パラメータに
よってカスタマイズすることができます。指定する値は `:min :mid :max` のいずれかです。区画の形状別の
サンプルを以下に示します。

```kaavio
(diagram (630 450)
; (grid)
  (with-textbox-options  (:align :center :font '(:fill :brown :size 20))
    (textbox '(130  20) ":lines :min" :no-frame t)
    (textbox '(330  20) ":lines :mid" :no-frame t)
    (textbox '(530  20) ":lines :max" :no-frame t)
    (textbox '( 20 100) "vertical"    :no-frame t :rotate -90)
    (textbox '( 20 240) "horizontal"  :no-frame t :rotate -90)
    (textbox '( 20 380) "matrix"      :no-frame t :rotate -90))
  (with-uml-partition-options (:stroke :black :fill :azure)
    (labels ((vertical (x lines)
               (with-subcanvas (`(,x 30) 200 140)
                 (uml-partition canvas.center 120 '((A 60) (B 60) (C 60)) :lines lines)))
             (horizontal (x lines)
               (with-subcanvas (`(,x 170) 200 140)
                 (uml-partition canvas.center '((X 40) (Y 40) (Z 40)) 180 :lines lines)))
             (matrix (x lines)
               (with-subcanvas (`(,x 310) 200 140)
                 (uml-partition canvas.center '((X 30) (Y 30) (Z 30))
                                              '((A 50) (B 50) (C 50)) :lines lines))))
      (dolist (func (list #'vertical #'horizontal #'matrix))
        (map nil func '(30 230 430) '(:min :mid :max))))))
```
Figure. uml-partition における lines パラメータのサンプル

　詳細は以下を参照してください。

* uml-partition マクロ
* with-uml-partition-lane マクロ
* with-uml-partition-options マクロ

#### uml-pin
<!-- autolink: [$$](#uml-pin) -->

　uml-pin は UML のアクティビティ図におけるピンを表記するための図形要素です。

<!-- snippet: UML-PIN-SAMPLE
(diagram (620 120)
  (grid)
  (drop-shadow)
  (with-theme (:uml-activity-default)
    (with-options (:filter :drop-shadow)
      (uml-action (x+ canvas.left 60) "action1" :id :act1)
      (uml-action (x+ $1.center  200) "action2" :id :act2 :height 80)
      (uml-action (x+ $1.center  160) "action3" :id :act3 :height 60)
      (uml-action (x+ $1.center  140) "action4" :id :act4))
    (uml-pin :act1 :R  "foo"  :id :foo)
    (uml-pin :act2 :L1 "bar"  :id :bar)
    (uml-pin :act2 :L3 "baz"  :id :baz)
    (uml-pin :act3 :L  "quux" :id :quux1 :multi t :offset '(-10 5))
    (uml-pin :act3 :R  nil    :id :quux2 :multi t)
    (uml-flow :foo   :bar  :style :RL)
    (uml-flow :foo   :baz  :style :RL)
    (uml-flow :act2  :quux1)
    (uml-flow :quux2 :act4)))
-->

```kaavio
<!-- expand: UML-PIN-SAMPLE -->
```
Figure. uml-pin のサンプル

　上記の作図は以下のコードで行なっています。ピンはフローの接続点に付けるものなので、
対象アクションの位置とその接続点を指定します。接続点は、コネクタであれば `:RL` などと
記述する表記の片側分なので、 `:R` や `:T3` などと記述することになります。また、通常は
[拡張領域](#uml-expansion-region)に渡すことになる入出力コレクションは `:multi t` と
することで表現できます。

```lisp
<!-- expand: UML-PIN-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-pin マクロ
* with-uml-pin-options マクロ

#### uml-port
<!-- autolink: [$$](#uml-port) -->

　uml-port は UML のコンポーネント図などにおけるポートを表記するための図形要素です。

<!-- snippet: UML-PORT-SAMPLE
(diagram (400 200)
  (grid)
  (with-theme (:uml-component-default)
    (uml-component (x+ canvas.cc 40) "server"
                   :width 200 :height 150 :contents t :id :svr)
    (uml-port svr.CL :name '("port" :offset (-20 2)) :id :port1)
    (with-subcanvas-of (:svr)
      (uml-component (xy+ port1.cc 100 0)
                     "transactor" :height 70 :id :tran)
      (uml-dependency :port1 :tran))
    (uml-interface (xy+ port1.cc -100 0) "interface" :id :interface)
    (uml-association :port1 :interface)))
-->

```kaavio
<!-- expand: UML-PORT-SAMPLE -->
```
Figure. uml-port のサンプル

　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-PORT-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-port マクロ
* with-uml-port-options マクロ

#### uml-realization
<!-- autolink: [$$](#uml-realization) -->

　uml-realization は UML のクラス図などにおける実現関係を表記するための図形要素です。

<!-- snippet: UML-REALIZATION-SAMPLE
(diagram (400 80)
  (grid)
  (with-theme (:uml-class-default)
    (uml-class   (x+ canvas.cl  50) "foo" :id :foo)
    (uml-class   (x+ canvas.cr -50) "bar" :id :bar)
    (uml-realization :foo :bar
                     :stereotype '("stereotype" :font 9)
                     :name       '("name" :offset (0 5)))))
-->

```kaavio
<!-- expand: UML-REALIZATION-SAMPLE -->
```
Figure. uml-realization のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-REALIZATION-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-realization マクロ
* with-uml-realization-options マクロ

#### uml-role-info
<!-- autolink: [$$](#uml-role-info) -->

　uml-role-info は UML の関連などにおいて使用されるロール情報の記述をするための
要素です。以下に例を示します。

<!-- snippet: UML-ROLE-INFO-SAMPLE
(diagram (340 100)
  (grid)
  (drop-shadow)
  (with-theme (:uml-class-default)
    (with-uml-class-options (:width 80 :height 40 :filter :drop-shadow)
      (uml-class (x+ canvas.cl  60) "foo")
      (uml-class (x+ canvas.cr -60) "bar")
      (uml-association $2.id $1.id :arrows 1 :role1 :parent :role2 :child))))
-->

```kaavio
<!-- expand: UML-ROLE-INFO-SAMPLE -->
```
Figure. uml-role-info のサンプル

　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-ROLE-INFO-SAMPLE -->
```

　関連などでは両端にロール情報の指定がありうるため、 `:role1 :role2` で指定を行ない
ます。指定されたパラメータは make-uml-role 関数に渡されます。詳細は以下を参照して
ください。

* make-uml-role 関数

#### uml-signal
<!-- autolink: [$$](#uml-signal) -->

　uml-signal は UML のアクティビティ図におけるシグナル送受信を表記するための図形要素です。
ほぼテキストボックスと同じように使えますが、少なくとも送信か受信かの区別をパラメータで与える必要
があります。また、形状は `:direction` パラメータで `:left` か `:right` で与えます。

<!-- snippet: UML-SIGNAL-SAMPLE
(diagram (400 160)
  (grid)
  (drop-shadow)
  (with-theme (:uml-activity-default)
    (uml-activity-start '(50 40) :id :start)
    (with-options (:filter :drop-shadow)
      (uml-signal (x+ start.center 150) :send
                  "sending~%signal"   :direction :right :id :step1)
      (uml-signal (y+ step1.center  80) :receive
                  "receiving~%signal" :direction :left  :id :step2))
    (uml-activity-final (x+ step2.center 150) :id :final)
    (uml-flow :start :step1)
    (uml-flow :step1 :step2)
    (uml-flow :step2 :final)))
-->

```kaavio
<!-- expand: UML-SIGNAL-SAMPLE -->
```
Figure. uml-signal のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-SIGNAL-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-signal マクロ
* with-uml-signal-options マクロ


#### uml-state-begin
<!-- autolink: [$$](#uml-state-begin) -->

　uml-state-begin は UML の状態マシン図における開始状態を表記するための図形要素です。

<!-- snippet: UML-STATE-BEGIN-SAMPLE
(diagram (200 80)
  (grid)
  (with-theme (:uml-statemachine-default)
    (uml-state-begin (x+ canvas.cc -60) :id :start)
    (uml-state (x+ $1.cc 100) "state" :id :state)
    (uml-transition :start :state)))
-->

```kaavio
<!-- expand: UML-STATE-BEGIN-SAMPLE -->
```
Figure. uml-state-begin のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-STATE-BEGIN-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-state-begin マクロ
* with-uml-state-begin-options マクロ

#### uml-state-end
<!-- autolink: [$$](#uml-state-end) -->

　uml-state-end は UML の状態マシン図における終了状態を表記するための図形要素です。

<!-- snippet: UML-STATE-END-SAMPLE
(diagram (200 80)
  (grid)
  (with-theme (:uml-statemachine-default)
    (uml-state (x+ canvas.cc -40) "state" :id :state)
    (uml-state-end (x+ $1.cc 100) :id :end)
    (uml-flow :state :end)))
-->

```kaavio
<!-- expand: UML-STATE-END-SAMPLE -->
```
Figure. uml-state-end のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-STATE-END-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-state-end マクロ
* with-uml-state-end-options マクロ

#### uml-state-history
<!-- autolink: [$$](#uml-state-history) -->

　uml-state-history は UML の状態マシン図における履歴状態を表記するための
図形要素です。

<!-- snippet: UML-STATE-HISTORY-SAMPLE
(diagram (500 180)
  (grid)
  (drop-shadow)
  (with-theme (:uml-statemachine-default)
    (with-uml-state-options (:filter :drop-shadow)
      (uml-state (xy+ canvas.tc -70 90) "active"
        :width 320 :height 150 :id :active
        :contents
        ((uml-state-begin   (xy+ canvas.tl 20 50) :id :begin)
         (uml-state (x+  $1.cc  90) "state1" :id :state1)
         (uml-state (x+  $1.cc 140) "state2" :id :state2)
         (uml-state-history (xy+ canvas.br -40 -20) :id :history)
         (uml-transition :begin  :state1)
         (uml-transition :state1 :state2 :style :R1L1)
         (uml-transition :state2 :state1 :style :L3R3)))
      (uml-state (x+ active.cr 100) "sleep" :id :sleep)
      (uml-transition :active :sleep)
      (uml-transition :sleep  :history :style :BR))))
-->

```kaavio
<!-- expand: UML-STATE-HISTORY-SAMPLE -->
```
Figure. uml-state-history のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-STATE-HISTORY-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-state-history マクロ
* with-uml-state-history-options マクロ

#### uml-state
<!-- autolink: [$$](#uml-state) -->

　uml-state は UML の状態マシン図における状態を表記するための図形要素です。

<!-- snippet: UML-STATE-SAMPLE
(diagram (400 220)
  (grid)
  (with-theme (:uml-statemachine-default)
    (uml-state-begin '(50 40) :id :start)
    (uml-state (x+ start.cc 100) "state1" :id :state1)
    (uml-state (x+ state1.cc 150) "state2" :id :state2)
    (uml-state (xy+ canvas.cc -60 30) "state3" :id :state3 :width 250 :height 110
        :contents
        ((uml-state (x+ canvas.cc -70) "sub-state1")
         (uml-state (x+ canvas.cc  70) "sub-state2")
         (uml-transition $2.id $1.id)))
    (uml-state-end (xy+ state3.cc 220 50) :id :final)
    (uml-transition :start :state1)
    (uml-transition :state1 :state2)
    (uml-transition :state2 :state3 :style :BR1)
    (uml-transition :state3 :final)))
-->

```kaavio
<!-- expand: UML-STATE-SAMPLE -->
```
Figure. uml-state のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-STATE-SAMPLE -->
```

　`:contents` パラメータと with-subcanvas-of マクロは通常は同じように使えますが、
uml-state では違いが存在します。 `:contents` で内部を描画する場合、uml-state は
テキストを上端付近に描画しますが、 `:contents` を使わずに with-subcanvas-of マクロを
使用した場合、テキストは中央に描画されます。この問題を解決するため、uml-state で
は `:contents t` と指定することでテキストを上端付近に描画させることができます。

　詳細は以下を参照してください。

* uml-state マクロ
* with-uml-state-options マクロ

#### uml-stereotype-info
<!-- autolink: [$$](#uml-stereotype-info) -->

　uml-stereotype-info は UML の各種要素において使用されるステレオタイプおよび
キーワードの指定をするための要素です。以下に例を示します。

<!-- snippet: UML-STEREOTYPE-INFO-SAMPLE
(diagram (340 100)
  (grid)
  (drop-shadow)
  (with-theme (:uml-class-default)
    (with-uml-class-options (:width 80 :height 40 :filter :drop-shadow)
      (uml-class (x+ canvas.cl  60) "foo")
      (uml-class (x+ canvas.cr -60) "bar" :keyword '(:thread :font 8))
      (uml-dependency $2.id $1.id :keyword :uses))))
-->

```kaavio
<!-- expand: UML-STEREOTYPE-INFO-SAMPLE -->
```
Figure. uml-stereotype-info のサンプル

　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-STEREOTYPE-INFO-SAMPLE -->
```

　どの UML 描画要素でも、 `:stereotype` と `:keyword` のいずれかのパラメータを使用
します。指定されたパラメータは make-uml-stereotype 関数に渡されます。詳細は以下を
参照してください。

* make-uml-stereotype 関数

#### uml-time-event
<!-- autolink: [$$](#uml-time-event) -->

　uml-time-event は UML のアクティビティ図における時間シグナルを表記するための図形要素です。


<!-- snippet: UML-TIME-EVENT-SAMPLE
(diagram (300 100)
  (grid)
  (drop-shadow)
  (with-theme (:uml-activity-default)
    (with-options (:filter :drop-shadow)
      (uml-time-event (x+ canvas.center -80) :label "wait 20min." :id :clock)
      (uml-action     (x+ canvas.center  60) "action" :id :next))
    (uml-flow :clock :next)))
-->

```kaavio
<!-- expand: UML-TIME-EVENT-SAMPLE -->
```
Figure. uml-time-event のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-TIME-EVENT-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-time-event マクロ
* with-uml-time-event-options マクロ

#### uml-transition
<!-- autolink: [$$](#uml-transition) -->

　uml-transition は UML の状態マシン図における状態間の遷移を表記するための
図形要素です。

<!-- snippet: UML-TRANSITION-SAMPLE
(diagram (400 100)
  (grid)
  (drop-shadow)
  (with-theme (:uml-statemachine-default)
    (with-uml-state-options (:filter :drop-shadow)
      (uml-state '( 60 50) "state1" :id :state1)
      (uml-state '(340 50) "state2" :id :state2)
      (uml-transition :state1 :state2
                      :spec '(:trigger :on-click
                              :action  "OnClick()")))))
-->

```kaavio
<!-- expand: UML-TRANSITION-SAMPLE -->
```
Figure. uml-transition のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-TRANSITION-SAMPLE -->
```

　必須パラメータは接続元／接続先の指定のみで、それ以外は名前付きパラメータです。
そのうち、 `:stereotype :keyword :spec` 以外はほぼコネクタと同じように使えます。
`:spec` は遷移のトリガーやガード条件、アクションを指定するためのものです。これら
についての詳細は以下を参照してください。

* uml-transition-spec
* uml-transition マクロ
* with-uml-transition-options マクロ

#### uml-transition-spec
<!-- autolink: [$$](#uml-transition-spec) -->

　uml-transition-spec は UML の状態マシン図における遷移仕様を記述するための、
トリガー、ガード条件、アクションなどからなる情報です。以下に例を示します。

<!-- snippet: UML-TRANSITION-SPEC-SAMPLE
(diagram (400 190)
  (grid)
  (drop-shadow)
  (with-theme (:uml-statemachine-default)
    (with-uml-state-options (:filter :drop-shadow)
      (uml-state '( 60  40) "state1" :id :st1)
      (uml-state '(340  40) "state2" :id :st2)
      (uml-transition :st1 :st2 :spec :on-idle)
      (uml-state '( 60 100) "state3" :id :st3)
      (uml-state '(340 150) "state4" :id :st4)
      (uml-transition :st3 :st4
                      :style :RL
                      :spec '(:trigger "trigger"
                              :guard   "guard"
                              :action  "state()"
                              :offset  (-90 -35)
                              :font    (:fill :brown :size 9))))))
-->

```kaavio
<!-- expand: UML-TRANSITION-SPEC-SAMPLE -->
```
Figure. uml-transition-spec のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-TRANSITION-SPEC-SAMPLE -->
```

　状態マシン図における遷移仕様は、uml-transition マクロに渡す `:spec` パラメータで
指定します。このパラメータは make-uml-transition-spec 関数に渡されます。単一のキーワード
シンボルや文字列を与えた場合、トリガーとして扱われます。リストを渡せばトリガーやガード条件、
アクション、表示位置の調整やフォント指定を行なうことができます。

　詳細は以下を参照してください。

* uml-transition
* make-uml-transition-spec 関数

#### uml-usecase
<!-- autolink: [$$](#uml-usecase) -->

　uml-usecase は UML のユースケース図におけるユースケースを表記するための図形要素です。

```kaavio
<!-- expand: UML-ACTOR-USECASE-SAMPLE -->
```
Figure. uml-usecase のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-ACTOR-USECASE-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-usecase マクロ
* with-uml-usecase-options マクロ

## リファレンス

### 関数とマクロ
<!-- include: reference.md -->

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

```kaavio
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

　kaavio では、上記に加えて以下の名前も使用することができます。これらは規格に含まれていない
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
                          (  0 255   0 "green"           )    ; duplicated
                          (  0 255   0 "green1"          )
                          (  0 238   0 "green2"          )
                          (  0 205   0 "green3"          )
                          (  0 139   0 "green4"          )
                          (190 190 190 "grey"            )    ; duplicated
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
                          (176  48  96 "maroon"          )    ; duplicated
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
                          (160  32 240 "purple"          )    ; duplicated
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

```kaavio
<!-- expand: EXTERNAL-COLOR-NAME-SAMPLE -->
```
Figure. 色の名前とサンプル - 2

## 更新履歴

　更新履歴です。

* __2022/08/21 - version 0.001__
    * とりあえず使えそうになったのでリリース
* __2022/08/31 - version 0.002__
    * ENHANCE : with-subcanvas-of マクロを追加
    * DOCUMENT : 「[](#座標と位置)」、および「[](#サブキャンバス)」を執筆
* __2022/09/04 - version 0.003__
    * __INCOMPATIBLE CHANGE : with-canvas マクロの第１パラメータを topleft から center に変更__
    * DOCUMENT : 「[](#定義と再使用)」を執筆
    * ENHANCE : 「[](#キューブ)」を追加
    * ENHANCE : 「[](#十字)」を追加
    * ENHANCE : 「[](#人物)」を追加
* __2022/09/11__
    * DOCUMENT : 「[](#ストローク)」を執筆
    * DOCUMENT : 「[](#フィル)」を執筆
* __2022/09/19__
    * プロダクト名を diagram から kaavio に変更
    * DOCUMENT : 「[](#フォント)」を執筆
* __2022/09/20__
    * DOCUMENT : 「[](#生の SVG コード片の挿入)」を執筆
    * ENHANCE : 「[](#テキストボックス)」と「[](#爆発)」に contents パラメータを追加
* __2022/09/22 - version 0.004__
    * __INCOMPATIBLE CHANGE : defs マクロを defgroup マクロに改名__
    * ENHANCE : 「[](#パターン)」を追加
    * ENHANCE : 「[](#function make-fill)」に `url` パラメータを追加
* __2022/09/25 - version 0.005__
    * ENHANCE : 「[](#function make-stroke)」に `url` パラメータを追加
    * ENHANCE : 「[](#グラデーション)」を追加
* __2022/09/27__
    * ENHANCE : 「[](#ひし形)」を追加
    * ENHANCE : 「[](#平行四辺形)」を追加
* __2022/10/01 - version 0.006__
    * ENHANCE : ID 指定のない要素に gensym ID を付与し、 `$N.id` で参照を可能とする修正
* __2022/10/03__
    * __MINOR INCOMPATIBLE CHANGE : [$$](#パターン)と[$$](#グラデーション)の追加に伴い、fill 指定から別の色を \
導出する機能を廃止__
    * ENHANCE : 上記の対応として「[](#メモ)」に `:fill2` パラメータを追加
* __2022/10/10 - version 0.007__
    * DOCUMENT : 「[](#IDと参照)」を執筆
* __2022/10/11__
    * DOCUMENT : 「[](#回転)」を執筆
* __2022/10/14 - version 0.008__
    * __INCOMPATIBLE CHANGE : フィルタのデフォルトを line / shape で区別する仕様を廃止__
    * ENHANCE : with-options でデフォルトフィルタを指定できるようにする修正
    * DOCUMENT : 「[](#フィルタ)」を執筆
* __2022/10/16 - version 0.009__
    * __MINOR INCOMPATIBLE CHANGE : レイヤー機能においてレイヤー非所属の図形要素が描画される順序を変更__
    * DOCUMENT : 「[](#レイヤー)」を執筆
* __2022/10/17__
    * DOCUMENT : 「[](#リンク)」を執筆
* __2022/10/22 - version 0.010__
    * ENHANCE : [$$](#終端マーク)で stroke や fill を指定しなかった場合の挙動を改善
    * DOCUMENT : 「[](#終端マーク)」を執筆
* __2022/10/23 - version 0.011__
    * ENHANCE : [$$](#ラベル)に関するデフォルト設定周辺の仕様を確定
    * DOCUMENT : 「[](#ラベル)」を執筆
* __2022/10/24__
    * BUGFIX : [$$](#ラベル)描画時の文字エスケープに関するバグを修正
    * BUGFIX : 直線やコネクタの端点、および中点座標に ID.end1 などの記法でアクセスできるようにする機能追加
    * ENHANCE : with-current-canvas を追加
    * __INCOMPATIBLE CHANGE : 上記に伴い、with-canvas マクロを非推奨に変更__
* __2022/10/25 - version 0.012__
    * ENHANCE : [$$](#ブロック矢印)でコネクタ同様に center, end1, end2 をサポートする機能追加
* __2022/10/26 - version 0.013__
    * ENHANCE : [$$](#円弧)でコネクタ同様に center, end1, end2 をサポートする機能追加
    * ENHANCE : [$$](#円弧)で[$$](#終端マーク)を指定可能にする機能追加
* __2022/11/06 - version 0.014__
    * DOCUMENT : パスの undocumented な未実装部分を完成させ、「[](#パス)」を執筆
* __2022/11/08 - version 0.015__
    * ENHANCE : [$$](#二次ベジェ曲線)を追加
    * ENHANCE : [$$](#三次ベジェ曲線)を追加
* __2022/11/09__
    * ENHANCE : [$$](#円弧)に debug パラメータを追加
* __2022/11/10__
    * ENHANCE : repeat 関数を追加
    * BUGFIX : [$$](#円弧)における[$$](#終端マーク)のバグを改修
    * ENHANCE : with-block-arrow-options マクロに length, size, margin パラメータを追加
* __2022/11/12 - version 0.016__
    * DOCUMENT : 「[](#画像ファイルの埋め込み)」を執筆
* __2022/11/16__
    * REFACTORING : 出力 SVG のサイズ低減措置
* __2022/11/17__
    * ENHANCE : with-subcanvas に debug パラメータを追加
    * ENHANCE : use に debug パラメータを追加
    * ENHANCE : [$$](#テーブル) の fills パラメータで `:r1-2` などの範囲指定をサポート
* __2022/11/18 - version 0.017__
    * ENHANCE : with-table-range を追加
    * ENHANCE : 出力 SVG のサイズ低減措置における table の不具合を改修
* __2022/11/20 - version 0.018__
    * BUGFIX : memo と cube における描画上のバグを改修
* __2022/11/27 - version 0.019__
    * ENHANCE : テーマ機能を追加
* __2022/12/11 - version 0.020__
    * ENHANCE : UML アクティビティ図を追加
* __2022/12/15 - version 0.021__
    * BUGFIX : uml-flow で `:spacing` パラメータを指定できない問題を改修
    * BUGFIX : uml-action の `:rake` パラメータに関するバグを改修
    * ENHANCE : uml-action で `:contents t` という記述をサポート
    * ENHANCE : uml-partition を追加
* __2022/12/25 - version 0.022__
    * ENHANCE : `obj.center` に対する `obj.cc` などの簡略記法を導入（[$@ 章](#座標と位置)参照）
    * ENHANCE : ml-note の接続先として point 値を指定可能にする機能追加
* __2023/03/12 - version 0.023__
    * ENHANCE : コネクタで接続先として point 値を指定可能にする機能追加
    * __MINOR INCOMPATIBLE CHANGE : make-font 関数に単一パラメータとしてキーワードパラメータを与えた場合の扱いを（:fill に）変更__
* __2024/02/07 - version 0.024__
    * ENHANCE : 多くの shape において center パラメータを position に変更し、新規追加の pivot パラメータで position に対する描画位置を調整可能とする変更
    * __INCOMPATIBLE CHANGE : 上記変更に伴い、 `cross` における既存の `pivot` パラメータを `intersection` に名称変更__
* __2024/05/11 - version 0.025__
    * BUGFIX : version 0.024 における connector のバグを改修
* __2025/02/20 - version 0.026__
    * ENHANCE : [$$](#正多角形)を追加
* __2025/03/08 - version 0.027__
    * ENHANCE : [$$](#クリッピング)機能を追加
* __2025/05/06 - version 0.028__
    * ENHANCE : UML 周辺の実装課題を解消。
* __2025/05/27 - version 0.029__
    * ENHANCE : UML 周辺の実装課題をさらに解消。
* __2025/06/10 - version 0.030__
    * BUGFIX : with- 系マクロの問題を解消
* __2025/06/14 - version 0.031__
    * ENHANCE : Common Lisp 処理系の REPL から利用できる sandbox mode を追加

## 図表一覧
<!-- embed:figure-list -->

　　

<!-- embed:table-list -->

　　

## 索引
<!-- embed:index-x -->

--------------------------------------------------------------------------------

<!-- embed:footnotes -->

