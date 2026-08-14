
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

