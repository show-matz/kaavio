#### macro 2d-curve

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{2d-curve}} points ${KEY} stroke end1 end2 layer filter id debug

${ARGS_AND_VALS}

* points ---- 
* stroke ---- 
* end1 ---- 
* end2 ---- 
* layer ---- 
* filter ---- 
* id ---- 
* debug ---- 

<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [2d-curve](#macro 2d-curve) -->

${BLANK_PARAGRAPH}

#### macro 3d-curve

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{3d-curve}} points ${KEY} stroke end1 end2 layer filter id debug


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [3d-curve](#macro 3d-curve) -->

${BLANK_PARAGRAPH}

#### macro arc

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{arc}} center rx ry x-axis-rotation degree1 degree2 ${KEY} stroke end1 end2 layer filter id debug


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [arc](#macro arc) -->

${BLANK_PARAGRAPH}

#### macro balloon

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{balloon}} position text anchor ${KEY} pivot width height round align valign margin font fill stroke link rotate layer id filter contents


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [balloon](#macro balloon) -->

${BLANK_PARAGRAPH}

#### macro block-arrow1

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{block-arrow1}} pt1 pt2 width ${KEY} (length nil length-p) (size nil size-p) (margin nil margin-p) fill stroke link layer filter id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [block-arrow1](#macro block-arrow1) -->

${BLANK_PARAGRAPH}

#### macro block-arrow2

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{block-arrow2}} (pt1 pt2 width ${KEY} (length nil length-p) (size   nil size-p) (margin nil margin-p) fill stroke link layer filter id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [block-arrow2](#macro block-arrow2) -->

${BLANK_PARAGRAPH}

#### macro brace

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{brace}} position direction width height ${KEY} pivot r point text font stroke layer filter id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [brace](#macro brace) -->

${BLANK_PARAGRAPH}

#### macro circle

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{circle}} position radius ${KEY} pivot fill stroke link layer id filter contents


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [circle](#macro circle) -->

${BLANK_PARAGRAPH}

#### macro connect

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{connect}} from to ${KEY} style spacing label stroke end1 end2 layer filter id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [connect](#macro connect) -->

${BLANK_PARAGRAPH}

#### function copy-point

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{copy-point}} pt => result

${ARGS_AND_VALS}

* pt ---- a point
* result ---- a point

<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [copy-point](#function copy-point) -->

${BLANK_PARAGRAPH}

#### macro cross

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{cross}} position width height size ${KEY} pivot size-v intersection fill stroke filter rotate link layer id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [cross](#macro cross) -->

${BLANK_PARAGRAPH}

#### macro cube

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{cube}} position width height text ${KEY} pivot depth align valign margin font fill fill2 stroke link rotate layer id filter contents


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [cube](#macro cube) -->

${BLANK_PARAGRAPH}

#### macro cylinder

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{cylinder}} position width height text ${KEY} pivot depth align valign margin font fill stroke link rotate layer id filter contents


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [cylinder](#macro cylinder) -->

${BLANK_PARAGRAPH}

#### macro defgradient

　defgradient マクロはグラデーションを定義します。グラデーションの詳細は「[](#グラデーション)」を
参照してください。説明不足ですが、基本的に SVG 規格に沿っていますので必要に応じて書籍や規格にあたって
ください。マクロシグネチャは以下の通りです。

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{defgradient}} (type id ${REST} params) ${REST} stops


<!-- stack:pop li -->

${DESCRIPTION}

${BLANK_PARAGRAPH}

Table. defgradient マクロのパラメータ
| parameter       | description                               |
|:================|:------------------------------------------|
| `type`          | グラデーションの種類を `:linear` または `:radial` から選択します。 |
| `id`            | ID をキーワードで指定します。              |
| `params`        | グラデーションの付加パラメータを指定します。 `type` により異なるため後述します。  |
| `stops`         | グラデーションストップをリストで指定します。詳細は「[](#グラデーション)」<br> \
を参照してください。後述する `href` パラメータで他の定義を参照する場合は<br> \
指定する必要はありません。 |

${BLANK_PARAGRAPH}

　`params` はグラデーションの種類（linear/radial）によって異なりますが、いずれの場合でも
名前付きパラメータとして扱われます。まず、共通のパラメータを以下に示します。


Table. defgradient マクロの付加パラメータ（linear/radial 共通）
| parameter       | description                               |
|:================|:------------------------------------------|
| `href`          | 他のグラデーション定義を参照する場合、その ID をキーワードで指定します。 |
| `units`         | patternUnits 属性を `:userSpaceOnUse` または `:objectBoundingBox` から<br> \
選択します。 |
| `spread`        | spreadMethod 属性を指定する場合、 `:pad :repeat :reflect` から選択します。 |
| `transform`     | gradientTtransform 属性を指定する場合、文字列で指定します。    |

${BLANK_PARAGRAPH}

　線型グラデーション特有の付加パラメータは以下の通りです。


Table. defgradient マクロの付加パラメータ（linear）
| parameter       | description                               |
|:================|:------------------------------------------|
| `x1`            | x1 属性を指定する場合、数値または文字列で指定します。 |
| `y1`            | y1 属性を指定する場合、数値または文字列で指定します。 |
| `x2`            | x2 属性を指定する場合、数値または文字列で指定します。 |
| `y2`            | y2 属性を指定する場合、数値または文字列で指定します。 |

${BLANK_PARAGRAPH}

　円形グラデーション特有の付加パラメータは以下の通りです。


Table. defgradient マクロの付加パラメータ（radial）
| parameter       | description                               |
|:================|:------------------------------------------|
| `cx`            | cx 属性を指定する場合、数値または文字列で指定します。 |
| `cy`            | cy 属性を指定する場合、数値または文字列で指定します。 |
| `fx`            | fx 属性を指定する場合、数値または文字列で指定します。 |
| `fy`            | fy 属性を指定する場合、数値または文字列で指定します。 |
| `radius`        | r 属性を指定する場合、数値または文字列で指定します。 |


${BLANK_PARAGRAPH}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [defgradient](#macro defgradient) -->

${BLANK_PARAGRAPH}

#### macro defgroup

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{defgroup}} width height id ${BODY} body
* ${{B}{defs}} width height id ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [defgroup](#macro defgroup) -->

${BLANK_PARAGRAPH}

#### macro defpattern

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{defpattern}} (id ${KEY} x y width height href units content-units view-box transform) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　defpattern マクロはパターンを定義します。パターンの詳細は「[](#パターン)」を参照してください。
説明不足ですが、基本的に SVG 規格に沿っていますので必要に応じて書籍や規格にあたってください。
マクロシグネチャは以下の通りです。なお、現在、preserveAspectRatio 属性には対応していません。
将来対応する可能性はありますが、未確定です。

${BLANK_PARAGRAPH}

Table. defpattern マクロのパラメータ
| parameter       | description                               |
|:================|:------------------------------------------|
| `id`            | ID をキーワードで指定します。              |
| `x`             |   |
| `y`             |   |
| `width`         | 幅を数値で指定します。                     |
| `height`        | 高さを数値で指定します。                   |
| `href`          | 高さを数値で指定します。                   |
| `units`         | patternUnits 属性を `:userSpaceOnUse` または `:objectBoundingBox` から<br> \
選択します。 |
| `content-units` | patternContentUnits 属性を `:userSpaceOnUse` または `:objectBoundingBox` <br> \
から選択します。 |
| `view-box`      | viewBox 属性を 4 つの数値からなるリストで指定します。 |
| `transform`     | patternTtransform 属性を指定する場合、文字列で指定します。  |


${BLANK_PARAGRAPH}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [defpattern](#macro defpattern) -->

${BLANK_PARAGRAPH}

#### macro diagram

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{diagram}} (w h ${KEY} fill) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [diagram](#macro diagram) -->

${BLANK_PARAGRAPH}

#### macro diamond

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{diamond}} position width height ${KEY} pivot fill stroke rotate link layer id filter contents


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [diamond](#macro diamond) -->

${BLANK_PARAGRAPH}

#### macro document

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{document}} position width height text ${KEY} pivot depth align valign margin font fill stroke link rotate layer id filter contents


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [document](#macro document) -->

${BLANK_PARAGRAPH}

#### macro drop-shadow

　生成画像にドロップシャドウを導入します。詳細は SVG 規格を参照してください。

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{drop-shadow}} ${KEY} id color-matrix deviation dx dy


<!-- stack:pop li -->

${DESCRIPTION}

${BLANK_PARAGRAPH}

Table. drop-shadow マクロのパラメータ
| parameter       | description                               |
|:================|:------------------------------------------|
| `id`            | ID をキーワードで指定します。省略した場合のデフォルト値は `:drop-shadow` です。 |
| `color-matrix`  | `<feColorMatrix>` の values 値を数値のリストで指定します。詳細は SVG 規格を \
参照してください。省略した場合のデフォルト値は `'(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.4 0)` です。 |
| `deviation`     | `<feGaussianBlur>` における `stdDeviation` 値を指定します。詳細は SVG 規格を \
参照してください。省略した場合のデフォルト値は 2 です。  |
| `dx, dy`        | `<feOffset>` における `dx` および `dy` 値を指定します。詳細は SVG 規格を \
参照してください。省略した場合のデフォルト値は 4 です。  |


${BLANK_PARAGRAPH}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [drop-shadow](#macro drop-shadow) -->

${BLANK_PARAGRAPH}

#### macro ellipse

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{ellipse}} position rx ry ${KEY} pivot fill stroke rotate link layer id filter contents


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [ellipse](#macro ellipse) -->

${BLANK_PARAGRAPH}

#### macro explosion1

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{explosion1}} position width height text ${KEY} pivot font fill stroke link rotate layer id filter contents


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [explosion1](#macro explosion1) -->

${BLANK_PARAGRAPH}

#### macro explosion2

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{explosion2}} position width height text ${KEY} pivot font fill stroke link rotate layer id filter contents


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [explosion2](#macro explosion2) -->

${BLANK_PARAGRAPH}

#### macro folder

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{folder}} position text ${KEY} pivot width height tab-width tab-height align valign font fill stroke margin link rotate layer filter id contents


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [folder](#macro folder) -->

${BLANK_PARAGRAPH}

#### macro glow-shadow

　生成画像にグローシャドウを導入します。詳細は SVG 規格を参照してください。

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{glow-shadow}} ${KEY} id color-matrix deviation


<!-- stack:pop li -->

${DESCRIPTION}

${BLANK_PARAGRAPH}

Table. glow-shadow マクロのパラメータ
| parameter       | description                               |
|:================|:------------------------------------------|
| `id`            | ID をキーワードで指定します。省略した場合のデフォルト値は `:glow-shadow` です。 |
| `color-matrix`  | `<feColorMatrix>` の values 値を数値のリストで指定します。詳細は SVG 規格を \
参照してください。省略した場合のデフォルト値は `'(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0)` です。 |
| `deviation`     | `<feGaussianBlur>` における `stdDeviation` 値を指定します。詳細は SVG 規格を \
参照してください。省略した場合のデフォルト値は 3 です。  |


${BLANK_PARAGRAPH}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [glow-shadow](#macro glow-shadow) -->

${BLANK_PARAGRAPH}

#### macro grid

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{grid}} ${KEY} (size 10) (bgcolor :white) stroke layer


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [grid](#macro grid) -->

${BLANK_PARAGRAPH}

#### macro image

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{image}} position filename ${KEY} pivot width height label link rotate layer id filter contents


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [image](#macro image) -->

${BLANK_PARAGRAPH}

#### function layer

　レイヤーの導入を宣言します。レイヤーについては「[](#レイヤー)」を参照してください。関数シグネチャは
以下の通りです。

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{layer}} name ${OPTIONAL} (display :inline)


<!-- stack:pop li -->

${DESCRIPTION}

${BLANK_PARAGRAPH}

Table. layer 関数のパラメータ
| parameter       | description                               |
|:================|:------------------------------------------|
| `name`          | レイヤーの名前をキーワードで指定します。     |
| `display`       | レイヤー全体の表示を `:inline` または `:none` で指定します。省略時の<br> \
                  デフォルトは `:inline` で、レイヤー全体を表示します。 `:none` を指定<br> \
                  すると、レイヤー全体を非表示にします。 |


${BLANK_PARAGRAPH}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [layer](#function layer) -->

${BLANK_PARAGRAPH}

#### macro line

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{line}} points ${KEY} stroke label end1 end2 layer filter id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [line](#macro line) -->

${BLANK_PARAGRAPH}

#### function make-endmark

　make-endmark 関数は終端マーク情報を生成します。終端マーク情報の詳細は「[](#終端マーク)」を
参照してください。関数シグネチャは以下の通りです。

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{make-endmark}} ${REST} params


<!-- stack:pop li -->

${DESCRIPTION}

　上記は簡潔な記述で柔軟な終端マーク情報の生成を可能にするためのもので、 `params` として渡される
パラメータ数に応じて以下のことをします。

* パラメータ数が 0 の場合
    * nil を返します
* パラメータ数が 1 の場合
    * 終端マーク情報が渡された場合、それをそのまま返します
    * 数値 N が渡された場合、 `(make-endmark :size N)` を返します
    * リスト lst が渡された場合、 `(apply #'make-endmark lst)` を返します
    * 上記のいずれでもない prm の場合、 `(make-endmark :type prm)` を返します
* パラメータ数が 2 以上の場合
    * 後述します

　パラメータ数が 2 以上の場合、make-endmark 関数は実質的に以下の関数であるかのように振舞います。

<!-- stack:push li class='syntax' -->

* ${{B}{make-endmark}} ${KEY} type size stroke fill

<!-- stack:pop li -->

Table. make-endmark 関数のパラメータ
| parameter   | description                      |
|:============|:---------------------------------|
| `type`      | 終端マークの形状を指定します。 `:arrow :triangle :diamond :circle :rect` のいずれか、または \
カスタム描画関数を指定します。 |
| `size`      | 終端マークのサイズを指定します。 `:small :medium :large :xlarge` のいずれか、または数値を指定します。 |
| `stroke`    | 終端マークを描画する線を指定します。詳細は「[](#ストローク)」を参照してください。省略した場合、実際に \
終端マークが描画される対象（直線やコネクタ）のストロークが使用されます。ただしその場合、そのストロークの `:dasharray`  \
など一部のパラメータは引き継がれません。 |
| `fill`      | 終端マーク内部の塗り潰しを指定します。詳細は「[](#フィル)」を参照してください。省略した場合、ストローク \
と同じ色で塗り潰されます。 |


${BLANK_PARAGRAPH}

　`type` パラメータでキーワードのかわりに指定するカスタム描画関数ですが、以下のシグネチャの関数を
指定する必要があります。これは現状では暫定的で undocumented な状態です。詳細はコードを参照して
ください。

```lisp
(lambda (points size stroke fill writer) ...)
```

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [make-endmark](#function make-endmark) -->

${BLANK_PARAGRAPH}

#### function make-fill

　make-fill 関数はフィル情報を生成します。フィル情報の詳細は「[](#フィル)」を参照してください。
関数シグネチャは以下の通りです。

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{make-fill}} ${REST} params


<!-- stack:pop li -->

${DESCRIPTION}

　上記は簡潔な記述で柔軟なフィル情報の生成を可能にするためのもので、 `params` として渡される
パラメータ数に応じて以下のことをします。

* パラメータ数が 0 の場合
    * デフォルトのフィル情報を返します
* パラメータ数が 1 の場合
    * フィル情報が渡された場合、それをそのまま返します
    * リスト lst が渡された場合、 `(apply #'make-fill lst)` を返します
    * 上記のいずれでもない prm の場合、 `(make-fill :color prm)` を返します
* パラメータ数が 2 以上の場合
    * 後述します

　パラメータ数が 2 以上の場合、make-fill 関数は実質的に以下の関数であるかのように振舞います。

<!-- stack:push li class='syntax' -->

* ${{B}{make-fill}} ${KEY} color opacity rule url base

<!-- stack:pop li -->

Table. make-fill 関数のパラメータ
| parameter   | description          |
|:============|:---------------------|
| `color`     | 塗り潰しの色を指定します。色の指定方法については [$@ 節](#色の指定)を、色の名前については<br> \
[$@ 節](#色の名前)を参照してください。  |
| `opacity`   | 塗り潰しの不透明度を 0.0 ～ 1.0 の数値で指定します。  |
| `rule`      | 塗りつぶしのルールを `:nonzero` または `:evenodd` で指定します。|
| `url`       | パターンやグラデーションの ID を指定します。詳細は [$@ 節](#フィルにおけるパターンとグラデーションの指定)を参照してください。 |
| `base`      | ${{TODO}{まだ記述されていません}} |


${BLANK_PARAGRAPH}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [make-fill](#function make-fill) -->

${BLANK_PARAGRAPH}

#### function make-font

　make-font 関数はフォント情報を生成します。フォント情報の詳細は「[](#フォント)」を参照して
ください。関数シグネチャは以下の通りです。

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{make-font}} ${REST} params


<!-- stack:pop li -->

${DESCRIPTION}

　上記は簡潔な記述で柔軟なフォント情報の生成を可能にするためのもので、 `params` として渡される
パラメータ数に応じて以下のことをします。

* パラメータ数が 0 の場合
    * デフォルトのフォント情報を返します
* パラメータ数が 1 の場合
    * フォント情報が渡された場合、それをそのまま返します
    * 数値 N が渡された場合、 `(make-font :size N)` を返します
    * キーワードシンボル CLR が渡された場合、 `(make-font :fill CLR)` を返します
    * リスト lst が渡された場合、 `(apply #'make-font lst)` を返します
    * 上記のいずれでもない prm の場合、 `(make-font :family prm)` を返します
* パラメータ数が 2 以上の場合
    * 後述します

　パラメータ数が 2 以上の場合、make-font 関数は実質的に以下の関数であるかのように振舞います。

<!-- stack:push li class='syntax' -->

* ${{B}{make-font}} ${KEY} family size fill stroke style decoration weight filter line-spacing width-spice base

<!-- stack:pop li -->

　各パラメータの意味は以下の通りです。詳細は「[](#フォント)」を参照してください。

Table. make-font 関数のパラメータ
| parameter      | description          |
|:===============|:---------------------|
| `family`       | 使用するフォントの名前をカンマで区切って並べた文字列を指定します（後述）。 |
| `size`         | フォントサイズを数値で指定します。 |
| `fill`         | フォントの塗り潰しを指定します。詳細は「[](#フィル)」を参照してください。 |
| `stroke`       | フォントの輪郭を描くストロークを指定します。詳細は「[](#ストローク)」を参照してください。 \
通常、フォントでは輪郭線は指定しません。 |
| `style`        | スタイルを `:normal, :italic, :oblique` のいずれかから指定します。 |
| `decoration`   | 装飾を `:none, :underline, :overline, :line-through` のいずれかから指定します。 |
| `weight`       | 文字の太さを `:normal, :bold, :bolder, :lighter` のいずれか、または  \
100 200 300 400 500 600 700 800 900 のいずれかから指定します。 |
| `filter`       | フォントの描画に適用するフィルタを指定します。詳細は「[](#フィルタ)」を参照してください。 |
| `line-spacing` | パラグラフなどで複数行のテキストを描画する際の行間を数値で指定します。 |
| `width-spice`  | フォントサイズとテキスト内容から描画幅を計算する際の係数を数値で指定します。 |
| `base`         | ${{TODO}{まだ記述されていません}} |


${BLANK_PARAGRAPH}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [make-font](#function make-font) -->

${BLANK_PARAGRAPH}

#### function make-id

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{make-id}} prefix ${REST} args


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [make-id](#function make-id) -->

${BLANK_PARAGRAPH}

#### function make-label

　make-label 関数はラベル情報を生成します。ラベル情報の詳細は「[](#ラベル)」を参照してください。
関数シグネチャは以下の通りです。

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{make-label}} ${REST} params


<!-- stack:pop li -->

${DESCRIPTION}

　上記は簡潔な記述で柔軟なラベル情報の生成を可能にするためのもので、 `params` として渡される
パラメータ数に応じて以下のことをします。

* パラメータ数が 0 の場合
    * nil を返します
* パラメータ数が 1 の場合
    * ラベル情報が渡された場合、それをそのまま返します
    * リスト lst が渡された場合、 `(apply #'make-label lst)` を返します
    * 上記のいずれでもない prm の場合、 `(make-label prm :offset nil)` を返します
* パラメータ数が 2 以上の場合
    * 後述します

　パラメータ数が 2 以上の場合、make-label 関数は実質的に以下の関数であるかのように振舞います。

<!-- stack:push li class='syntax' -->

* ${{B}{make-label}} text ${KEY} position offset font

<!-- stack:pop li -->

　各パラメータの意味は以下の通りです。詳細は「[](#ラベル)」を参照してください。

Table. make-label 関数のパラメータ
| parameter    | description          |
|:=============|:---------------------|
| `text`       | ラベルのテキストを文字列かキーワードで指定します。  |
| `position`   | ラベルを表示する位置を `:above, :below, :left, :right` から指定します。<br> \
直線やコネクタでは無視されます。  |
| `offset`     | ラベルの表示位置を微調整する `(x y)` 値を指定します。  |
| `font`       | ラベルの描画に使用するフォントを指定します。詳細は「[](#フォント)」を参照<br> \
してください。 |

${BLANK_PARAGRAPH}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [make-label](#function make-label) -->

${BLANK_PARAGRAPH}

#### function make-link

　make-link 関数はリンク情報を生成します。リンク情報の詳細は「[](#リンク)」を参照してください。
関数シグネチャは以下の通りです。

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{make-link}} ${REST} params


<!-- stack:pop li -->

${DESCRIPTION}

　上記は簡潔な記述で柔軟なリンク情報の生成を可能にするためのもので、 `params` として渡される
パラメータ数に応じて以下のことをします。

* パラメータ数が 0 の場合
    * nil を返します
* パラメータ数が 1 の場合
    * リンク情報が渡された場合、それをそのまま返します
    * リスト lst が渡された場合、 `(apply #'make-link lst)` を返します
    * 上記のいずれでもない prm の場合、 `(make-link :url prm)` を返します
* パラメータ数が 2 以上の場合
    * 後述します

　パラメータ数が 2 以上の場合、make-link 関数は実質的に以下の関数であるかのように振舞います。

<!-- stack:push li class='syntax' -->

* ${{B}{make-link}} ${KEY} url target

<!-- stack:pop li -->

　各パラメータの意味は以下の通りです。詳細は「[](#リンク)」を参照してください。

Table. make-link 関数のパラメータ
| parameter    | description                          |
|:=============|:-------------------------------------|
| `url`        | リンク先の URL を文字列で指定します。  |
| `target`     | a タグにおける target をキーワード `:replace :self :parent :top :blank` の<br> \
いずれかで指定します。省略可能です。 |


${BLANK_PARAGRAPH}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [make-link](#function make-link) -->

${BLANK_PARAGRAPH}

#### function make-point

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{make-point}} x y ${OPTIONAL} type => result


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [make-point](#function make-point) -->

${BLANK_PARAGRAPH}

#### function make-stroke

　make-stroke 関数はストローク情報を生成します。ストローク情報の詳細は「[](#ストローク)」を
参照してください。関数シグネチャは以下の通りです。

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{make-stroke}} ${REST} params


<!-- stack:pop li -->

${DESCRIPTION}

　上記は簡潔な記述で柔軟なストローク情報の生成を可能にするためのもので、 `params` として渡される
パラメータ数に応じて以下のことをします。

* パラメータ数が 0 の場合
    * デフォルトのストローク情報を返します
* パラメータ数が 1 の場合
    * ストローク情報が渡された場合、それをそのまま返します
    * 数値 N が渡された場合、 `(make-stroke :width N)` を返します
    * リスト lst が渡された場合、 `(apply #'make-stroke lst)` を返します
    * 上記のいずれでもない prm の場合、 `(make-stroke :color prm)` を返します
* パラメータ数が 2 以上の場合
    * 後述します

　パラメータ数が 2 以上の場合、make-stroke 関数は実質的に以下の関数であるかのように振舞います。

<!-- stack:push li class='syntax' -->

* ${{B}{make-stroke}} ${KEY} color width opacity linecap linejoin miterlimit dasharray dashoffset url base)

<!-- stack:pop li -->

　各パラメータの意味は以下の通りです。詳細は「[](#ストローク)」を参照してください。

Table. make-stroke 関数のパラメータ
| parameter    | description          |
|:=============|:---------------------|
| `color`      | 線の色を指定します。色の指定方法については [$@ 節](#色の指定)を、色の名前については<br> \
[$@ 節](#色の名前)を参照してください。  |
| `width`      | 線の幅を数値で指定します。                     |
| `opacity`    | 線の不透明度を 0.0 ～ 1.0 の数値で指定します。  |
| `linecap`    | 線の両端の形状を `:butt, :round, :square` から指定します。  |
| `linejoin`   | 線が折れ曲ってできる角の形状を `:miter, :round, :bevel` から指定します。  |
| `miterlimit` | `linejoin` が `:miter` の場合の、結合される線の太さに対する結合部の長さの<br> \
比率を数値で指定します。デフォルト値は 4 です。 |
| `dasharray`  | 点線や破線を描画したい場合に、繰り返される線の長さと間隔の長さをリストで<br> \
指定します。デフォルト値は nil で、直線になります。 |
| `dashoffset` | `dasharray` を指定する場合に、線の開始を `dasharray` のどこから始めるかの<br> \
オフセットを数値で指定します。  |
| `url`        | パターンやグラデーションの ID を指定します。詳細は [$@ 節](#ストロークにおけるパターンとグラデーションの指定)を参照してください。 |
| `base`       | ${{TODO}{まだ記述されていません}} |


${BLANK_PARAGRAPH}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [make-stroke](#function make-stroke) -->

${BLANK_PARAGRAPH}

#### macro memo

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{memo}} position text ${KEY} pivot width height crease align valign margin font fill fill2 stroke link rotate layer id filter contents


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [memo](#macro memo) -->

${BLANK_PARAGRAPH}

#### macro paragraph

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{paragraph}} position text ${KEY} align valign rotate font link layer id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [paragraph](#macro paragraph) -->

${BLANK_PARAGRAPH}

#### macro parallelogram

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{parallelogram}} position width height direction offset ${KEY} pivot fill stroke rotate link layer id filter contents


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [parallelogram](#macro parallelogram) -->

${BLANK_PARAGRAPH}

#### macro path

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{path}} data ${KEY} fill stroke layer filter id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [path](#macro path) -->

${BLANK_PARAGRAPH}

#### macro person

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{person}} position size ${KEY} pivot fill stroke label link rotate layer filter id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [person](#macro person) -->

${BLANK_PARAGRAPH}

#### function point*

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{point*}} pt n


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [point*](#function point*) -->

${BLANK_PARAGRAPH}

#### function point+

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{point+}} pt1 pt2


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [point+](#function point+) -->

${BLANK_PARAGRAPH}

#### function point-

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{point-}} pt1 pt2


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [point-](#function point-) -->

${BLANK_PARAGRAPH}

#### function point-absolute-p

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{point-absolute-p}} pt


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [point-absolute-p](#function point-absolute-p) -->

${BLANK_PARAGRAPH}

#### function point-distance

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{point-distance}} pt1 pt2


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [point-distance](#function point-distance) -->

${BLANK_PARAGRAPH}

#### function point-p

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{point-p}} pt


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [point-p](#function point-p) -->

${BLANK_PARAGRAPH}

#### function point-relative-p

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{point-relative-p}} pt


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [point-relative-p](#function point-relative-p) -->

${BLANK_PARAGRAPH}

#### function point-x

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{point-x}} pt
* ${{B}{(setf point-x)}} val pt


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [point-x](#function point-x) -->

${BLANK_PARAGRAPH}

#### function point-y

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{point-y}} pt
* ${{B}{(setf point-y)}} val pt


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [point-y](#function point-y) -->

${BLANK_PARAGRAPH}

#### function point/

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{point/}} pt n


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [point/](#function point/) -->

${BLANK_PARAGRAPH}

#### function point/x+

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{point/x+}} pt x


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [point/x+](#function point/x+) -->

${BLANK_PARAGRAPH}

#### function point/xy+

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{point/xy+}} pt x y


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [point/xy+](#function point/xy+) -->

${BLANK_PARAGRAPH}

#### function point/y+

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{point/y+}} pt y


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [point/y+](#function point/y+) -->

${BLANK_PARAGRAPH}

#### macro polygon

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{polygon}} points ${KEY} fill stroke link layer filter id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [polygon](#macro polygon) -->

${BLANK_PARAGRAPH}

#### function pt*

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{pt*}} pt n


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [pt*](#function pt*) -->

${BLANK_PARAGRAPH}

#### function pt+

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{pt+}} pt1 pt2


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [pt+](#function pt+) -->

${BLANK_PARAGRAPH}

#### function pt-

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{pt-}} pt1 pt2


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [pt-](#function pt-) -->

${BLANK_PARAGRAPH}

#### function pt/

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{pt/}} pt n


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [pt/](#function pt/) -->

${BLANK_PARAGRAPH}

#### macro raw-svg

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{raw-svg}} svgdata ${KEY} (layer nil)


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

* [](#生の SVG コード片の挿入)

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [raw-svg](#macro raw-svg) -->

${BLANK_PARAGRAPH}

#### macro rect

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{rect}} position width height ${KEY} pivot rx ry fill stroke rotate link layer id filter contents


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [rect](#macro rect) -->

${BLANK_PARAGRAPH}

#### macro register-theme

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{register-theme}} (name ${OPTIONAL} base) ${REST} settings


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [register-theme](#macro register-theme) -->

${BLANK_PARAGRAPH}

#### function repeat

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{repeat}} source cnt ${REST} customs


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

<!-- stack:push li class='syntax' -->
${SYNTAX}

```lisp
(repeat 50 4)          ; => '(50 50 50 50)
(repeat 40 5 '(0 80))  ; => '(80 40 40 40 40)
(repeat #'identity 10) ; => '(0 1 2 3 4 5 6 7 8 9)
(repeat (lambda (i)
           (* 10 (1+ i))) 10)  ; => '(10 20 30 40 50 60 70 80 90 100)
```

<!-- stack:pop li -->

${DESCRIPTION}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [repeat](#function repeat) -->

${BLANK_PARAGRAPH}

#### function rgb

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{rgb}} r g b


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [rgb](#function rgb) -->

${BLANK_PARAGRAPH}

#### macro table

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{table}} position rows cols ${KEY} pivot font fills stroke texts layer id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [table](#macro table) -->

${BLANK_PARAGRAPH}

#### macro text

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{text}} position text ${KEY} align font link layer id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [text](#macro text) -->

${BLANK_PARAGRAPH}

#### macro textbox

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{textbox}} position text ${KEY} pivot width height no-frame rx ry align valign margin font fill stroke link rotate layer id filter contents


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [textbox](#macro textbox) -->

${BLANK_PARAGRAPH}

#### macro uml-action

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-action}} position text ${KEY} keyword pivot width height margin corner-r rake font fill stroke link layer filter id contents


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

* rake は t を指定するか、または数値 4 要素のリストを指定する。`(width height x-margin y-margin)`
* `:contents t` がサポートされる。

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-action](#macro uml-action) -->

${BLANK_PARAGRAPH}

#### macro uml-activity-final

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-activity-final}} position ${KEY} radius ratio pivot fill stroke link layer filter id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-activity-final](#macro uml-activity-final) -->

${BLANK_PARAGRAPH}

#### macro uml-activity-start

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-activity-start}} position ${KEY} radius pivot fill link layer filter id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-activity-start](#macro uml-activity-start) -->

${BLANK_PARAGRAPH}

#### macro uml-connector

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-connector}} position1 position2 id ${KEY} pivot1 pivot2 name size fill stroke font filter layer


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-connector](#macro uml-connector) -->

${BLANK_PARAGRAPH}

#### macro uml-decision

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-decision}} position ${KEY} pivot text width height margin font fill stroke link layer filter id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-decision](#macro uml-decision) -->

${BLANK_PARAGRAPH}

#### macro uml-expansion-region

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-expansion-region}} position width height ${KEY} pivot keyword font offset corner-r fill stroke link layer id contents


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-expansion-region](#macro uml-expansion-region) -->

${BLANK_PARAGRAPH}

#### macro uml-flow

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-flow}} from to ${KEY} keyword spec style spacing filter layer id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-flow](#macro uml-flow) -->

${BLANK_PARAGRAPH}

#### macro uml-flow-final

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-flow-final}} position ${KEY} pivot radius fill stroke link layer filter id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-flow-final](#macro uml-flow-final) -->

${BLANK_PARAGRAPH}

#### macro uml-fork

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-fork}} position direction ${KEY} pivot width length fill link filter layer id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-fork](#macro uml-fork) -->

${BLANK_PARAGRAPH}

#### macro uml-frame

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-frame}} position width height title ${KEY} pivot margin font fill stroke link layer filter id contents


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-frame](#macro uml-frame) -->

${BLANK_PARAGRAPH}

#### macro uml-join

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-join}} position direction ${KEY} pivot spec width length fill link filter layer id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

* ${{TODO}{spec パラメータの指定は、ラベルに準拠。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-join](#macro uml-join) -->

${BLANK_PARAGRAPH}

#### macro uml-merge

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-merge}} position ${KEY} pivot width height margin font fill stroke link layer filter id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-merge](#macro uml-merge) -->

${BLANK_PARAGRAPH}

#### macro uml-note

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-note}} position width height text ${KEY} pivot keyword targets align valign margin crease font fill stroke link layer filter id contents


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

* ${{TODO}{targets は接続先を複数指定する場合はリスト、単一ならリストでなくてもよい。}}
* ${{TODO}{targets で指定する接続先は、図形要素の ID または point 値}}

　以下も参照してください。

* [](#uml-note)

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-note](#macro uml-note) -->

${BLANK_PARAGRAPH}

#### macro uml-partition

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-partition}} position rows cols ${KEY} pivot lines header fills stroke font layer id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-partition](#macro uml-partition) -->

${BLANK_PARAGRAPH}

#### macro uml-pin

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-pin}} target position name ${KEY} offset multi size fill stroke font filter layer id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-pin](#macro uml-pin) -->

${BLANK_PARAGRAPH}

#### macro uml-signal

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-signal}} position type text ${KEY} pivot keyword width height direction depth font fill stroke margin link filter layer id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-signal](#macro uml-signal) -->

${BLANK_PARAGRAPH}

#### macro uml-time-event

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-time-event}} position ${KEY} label pivot width height fill stroke link filter layer id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-time-event](#macro uml-time-event) -->

${BLANK_PARAGRAPH}

#### macro use

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{use}} ref position ${KEY} pivot link rotate layer id contents debug


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [use](#macro use) -->

${BLANK_PARAGRAPH}

#### macro with-balloon-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-balloon-options}} (${KEY} round align valign margin font fill stroke filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-balloon-options](#macro with-balloon-options) -->

${BLANK_PARAGRAPH}

#### macro with-block-arrow-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-block-arrow-options}} (${KEY} length size margin fill stroke filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-block-arrow-options](#macro with-block-arrow-options) -->

${BLANK_PARAGRAPH}

#### macro with-brace-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-brace-options}} (${KEY} font stroke filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-brace-options](#macro with-brace-options) -->

${BLANK_PARAGRAPH}

#### macro with-canvas

　with-current-canvas マクロの導入に伴い、with-canvas マクロは非推奨となりました。

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-canvas}} (sym-center sym-width sym-height) canvas ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-canvas](#macro with-canvas) -->

${BLANK_PARAGRAPH}

#### macro with-cross-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-cross-options}} (${KEY} fill stroke filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-cross-options](#macro with-cross-options) -->

${BLANK_PARAGRAPH}

#### macro with-cube-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-cube-options}} (${KEY} depth align valign margin font fill fill2 stroke filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-cube-options](#macro with-cube-options) -->

${BLANK_PARAGRAPH}

#### macro with-current-canvas

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-current-canvas}} (${REST} vars) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-current-canvas](#macro with-current-canvas) -->

${BLANK_PARAGRAPH}

#### macro with-cylinder-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-cylinder-options}} (${KEY} depth align valign margin font fill stroke filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-cylinder-options](#macro with-cylinder-options) -->

${BLANK_PARAGRAPH}

#### macro with-document-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-document-options}} (${KEY} align valign margin font fill stroke filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-document-options](#macro with-document-options) -->

${BLANK_PARAGRAPH}

#### macro with-endmark-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-endmark-options}} (${KEY} type size fill end1 end2) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-endmark-options](#macro with-endmark-options) -->

${BLANK_PARAGRAPH}

#### macro with-explosion-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-explosion-options}} (${KEY} font fill stroke filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-explosion-options](#macro with-explosion-options) -->

${BLANK_PARAGRAPH}

#### macro with-folder-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-folder-options}} (${KEY} tab-width tab-height align valign margin font fill stroke filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-folder-options](#macro with-folder-options) -->

${BLANK_PARAGRAPH}

#### macro with-label-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-label-options}} (${KEY} position offset font) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-label-options](#macro with-label-options) -->

${BLANK_PARAGRAPH}

#### macro with-memo-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-memo-options}} (${KEY} crease align valign margin font fill fill2 stroke filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-memo-options](#macro with-memo-options) -->

${BLANK_PARAGRAPH}

#### macro with-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-options}} (${KEY} fill stroke font filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-options](#macro with-options) -->

${BLANK_PARAGRAPH}

#### macro with-person-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-person-options}} (${KEY} fill stroke layer filter) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-person-options](#macro with-person-options) -->

${BLANK_PARAGRAPH}

#### macro with-point

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-point}} (sym-x sym-y) pt ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-point](#macro with-point) -->

${BLANK_PARAGRAPH}

#### macro with-subcanvas

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-subcanvas}} (top-left width height ${KEY} debug) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-subcanvas](#macro with-subcanvas) -->

${BLANK_PARAGRAPH}

#### macro with-subcanvas-of

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-subcanvas-of}} (id) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-subcanvas-of](#macro with-subcanvas-of) -->

${BLANK_PARAGRAPH}

#### macro with-table-cell

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-table-cell}} (id r c) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-table-cell](#macro with-table-cell) -->

${BLANK_PARAGRAPH}

#### macro with-table-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-table-options}} (${KEY} font fill stroke layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-table-options](#macro with-table-options) -->

${BLANK_PARAGRAPH}

#### macro with-table-range

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-table-range}} (id kwd) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-table-range](#macro with-table-range) -->

${BLANK_PARAGRAPH}

#### macro with-textbox-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-textbox-options}} (${KEY} rx ry align valign margin font fill stroke filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-textbox-options](#macro with-textbox-options) -->

${BLANK_PARAGRAPH}

#### macro with-theme

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-theme}} (name) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-theme](#macro with-theme) -->

${BLANK_PARAGRAPH}

#### macro with-uml-action-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-action-options}} (${KEY} font fill stroke width height corner-r margin rake filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

* ここで指定する width / height はデフォルトの最低サイズとして使用される

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-uml-action-options](#macro with-uml-action-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-activity-final-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-activity-final-options}} (${KEY} radius ratio fill stroke  filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-uml-activity-final-options](#macro with-uml-activity-final-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-activity-start-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-activity-start-options}} (${KEY} radius fill filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-uml-activity-start-options](#macro with-uml-activity-start-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-connector-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-connector-options}} (${KEY} font fill stroke size filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-uml-connector-options](#macro with-uml-connector-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-decision-merge-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-decision-merge-options}} (${KEY} font fill stroke width height margin filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-uml-decision-merge-options](#macro with-uml-decision-merge-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-expansion-region-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-expansion-region-options}} (${KEY} font fill stroke corner-r filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-uml-expansion-region-options](#macro with-uml-expansion-region-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-flow-final-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-flow-final-options}} (${KEY} radius fill stroke filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-uml-flow-final-options](#macro with-uml-flow-final-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-flow-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-flow-options}} (${KEY} stroke arrow-size font filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-uml-flow-options](#macro with-uml-flow-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-fork-join-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-fork-join-options}} (${KEY} width length color filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-uml-fork-join-options](#macro with-uml-fork-join-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-frame-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-frame-options}} (${KEY} font fill stroke margin filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-uml-frame-options](#macro with-uml-frame-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-note-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-note-options}} (${KEY} font fill stroke margin align valign crease filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-uml-note-options](#macro with-uml-note-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-partition-lane

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-partition-lane}} (id name ${OPTIONAL} name2) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-uml-partition-lane](#macro with-uml-partition-lane) -->

${BLANK_PARAGRAPH}

#### macro with-uml-partition-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-partition-options}} (${KEY} lines header font fill stroke layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-uml-partition-options](#macro with-uml-partition-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-pin-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-pin-options}} (${KEY} font fill stroke size filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-uml-pin-options](#macro with-uml-pin-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-signal-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-signal-options}} (${KEY} font fill stroke width height direction depth margin filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-uml-signal-options](#macro with-uml-signal-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-time-event-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-time-event-options}} (${KEY} fill stroke width height filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-uml-time-event-options](#macro with-uml-time-event-options) -->

${BLANK_PARAGRAPH}

#### function x+

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{x+}} pt x => result


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [x+](#function x+) -->

${BLANK_PARAGRAPH}

#### function xy+

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{xy+}} pt x y => result


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [xy+](#function xy+) -->

${BLANK_PARAGRAPH}

#### function y+

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{y+}} pt y => result


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [y+](#function y+) -->

${BLANK_PARAGRAPH}

