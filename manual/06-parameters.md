
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
は [$@ 節](#パターンとグラデーション)を参照してください。定義されたグラデーションを指定する
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
は [$@ 節](#パターンとグラデーション)を参照してください。定義されたパターンを指定する
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

${BLANK_PARAGRAPH}

