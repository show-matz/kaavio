#### macro 2d-curve

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{2d-curve}} points ${KEY} stroke end1 end2 layer filter id debug

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `points` ---- 曲線を構成する点のリストを指定します
* `stroke` ---- 曲線を描画するストロークを指定します
* `end1` ---- 始端に終端マークをつける場合は指定します
* `end2` ---- 終端に終端マークをつける場合は指定します
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します
* `debug` ---- 補助線を描画する場合、 `t` または色名を指定します

${DESCRIPTION}

　二次ベジェ曲線を描画します。 `points` で指定する点のリストは最低限でも３要素以上である必要があり、
始点、制御点、終点の順で指定します。また、パスにおける `:2d-curve-to` 同様、任意数の点を追加する
ことが可能です。詳細は [$@ 節](#パス)の `:2d-curve-to` の説明を参照してください。

${SEE_ALSO}

* 二次ベジェ曲線
* 3d-curve マクロ

${NO_NOTES}


<!-- autolink: [2d-curve マクロ](#macro 2d-curve) -->

${BLANK_PARAGRAPH}

#### macro 3d-curve

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{3d-curve}} points ${KEY} stroke end1 end2 layer filter id debug

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `points` ---- 曲線を構成する点のリストを指定します
* `stroke` ---- 曲線を描画するストロークを指定します
* `end1` ---- 始端に終端マークをつける場合は指定します
* `end2` ---- 終端に終端マークをつける場合は指定します
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します
* `debug` ---- 補助線を描画する場合、 `t` または色名を指定します

${DESCRIPTION}

　三次ベジェ曲線を描画します。 `points` で指定する点のリストは最低限でも４要素以上である必要があり、
始点、制御点1、制御点2、終点の順で指定します。また、パスにおける `:3d-curve-to` 同様、任意数の点を
追加することが可能です。詳細は [$@ 節](#パス)の `:3d-curve-to` の説明を参照してください。

${SEE_ALSO}

* 三次ベジェ曲線
* 2d-curve マクロ

${NO_NOTES}


<!-- autolink: [3d-curve マクロ](#macro 3d-curve) -->

${BLANK_PARAGRAPH}

#### macro arc

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{arc}} center rx ry x-axis-rotation degree1 degree2 ${KEY} stroke end1 end2 layer filter id debug

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `center` ---- ベースとなる楕円の中心点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `rx` ---- ベースとなる楕円の x 軸方向の半径を数値で指定します。
* `ry` ---- ベースとなる楕円の y 軸方向の半径を数値で指定します。
* `x-axis-rotation` ---- ベースとなる楕円の回転角（x 軸に対してどれだけ回転させるか）を数値で指定します。
* `degree1` ---- 円弧を「時計回りに描く」場合の開始角度を数値で指定します。
* `degree2` ---- 円弧を「時計回りに描く」場合の終了角度を数値で指定します。
* `stroke` ---- 曲線を描画するストロークを指定します
* `end1` ---- 始端に終端マークをつける場合は指定します
* `end2` ---- 終端に終端マークをつける場合は指定します
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します
* `debug` ---- 補助線を描画する場合、 `t` または色名を指定します

${DESCRIPTION}

　円弧を描画します。円弧のベースとなる楕円の中心点や始点・終点の角度が明らかな
場合の使用を想定しています。始点・終点の座標がわかっている場合は path の使用を
検討してください。詳細は [$@ 節](#パス)の `:arc-to` の説明を参照してください。

${SEE_ALSO}

* 円弧
* path マクロ

${NO_NOTES}


<!-- autolink: [arc マクロ](#macro arc) -->

${BLANK_PARAGRAPH}

#### macro balloon

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{balloon}} position text anchor ${KEY} pivot width height round align valign margin font fill stroke link rotate layer id filter contents

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `text` ---- 内部に描画するテキストを文字列で指定します。改行は "~%" で表現します。
* `anchor` ---- 引き出し線の位置を指定します。詳細は「[](#座標と位置)」を参照してください。
* `pivot` ---- 基準点が吹き出しのどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `width` ---- 幅を自動決定せず、明示的に指定したい場合に数値で指定します。
* `height` ---- 高さを自動決定せず、明示的に指定したい場合に数値で指定します。
* `round` ---- 角を丸くしたい場合に、角の半径を数値で指定します。省略した場合のデフォルト値は 0（つまり角を丸くしない）です。
* `align` ---- テキストの水平方向のアライメントを `:left :center :right` のいずれかで指定します。省略した場合のデフォルト値は `:center` です。
* `valign` ---- テキストの垂直方向のアライメントを `:top :center :bottom` のいずれかで指定します。省略した場合のデフォルト値は `:center` です。
* `margin` ---- テキスト描画における「余白」のサイズです。省略した場合のデフォルト値は 10 です。
* `font` ---- フォントを指定します。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 外枠を描画するストロークを指定します
* `link` ---- リンクにする場合、リンク先を指定します。
* `rotate` ---- 全体を回転させたい場合に、その角度を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。

${DESCRIPTION}

　吹き出しを描画します。複数の吹き出しでスタイルを統一したい場合、with-balloon-options マクロを
使うことができます。

${SEE_ALSO}

* 吹き出し
* with-balloon-options マクロ

${NO_NOTES}


<!-- autolink: [balloon マクロ](#macro balloon) -->

${BLANK_PARAGRAPH}

#### macro block-arrow1

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{block-arrow1}} pt1 pt2 width ${KEY} (length nil length-p) (size nil size-p) (margin nil margin-p) fill stroke link layer filter id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `pt1, pt2` ---- 始点と終点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `width` ---- 矢印の幅を数値で指定します。
* `length` ---- 矢印部分の長さを数値で指定します。
* `size` ---- 矢印部分の大きさを数値で指定します。
* `margin` ---- 始点・終点とブロック矢印の間にあける隙間を数値で指定します。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ----  外枠を描画する線を指定します。
* `link` ---- リンクにする場合、リンク先を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します

${DESCRIPTION}

　ブロック矢印を描画します。複数のブロック矢印でスタイルを統一したい場合、
with-block-arrow-options マクロを使うことができます。

${SEE_ALSO}

* ブロック矢印
* block-arrow2 マクロ
* with-block-arrow-options マクロ

${NO_NOTES}


<!-- autolink: [block-arrow1 マクロ](#macro block-arrow1) -->

${BLANK_PARAGRAPH}

#### macro block-arrow2

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{block-arrow2}} (pt1 pt2 width ${KEY} (length nil length-p) (size   nil size-p) (margin nil margin-p) fill stroke link layer filter id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `pt1, pt2` ---- 始点と終点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `width` ---- 矢印の幅を数値で指定します。
* `length` ---- 矢印部分の長さを数値で指定します。
* `size` ---- 矢印部分の大きさを数値で指定します。
* `margin` ---- 始点・終点とブロック矢印の間にあける隙間を数値で指定します。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ----  外枠を描画する線を指定します。
* `link` ---- リンクにする場合、リンク先を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します

${DESCRIPTION}

　ブロック矢印を描画します。複数のブロック矢印でスタイルを統一したい場合、
with-block-arrow-options マクロを使うことができます。

${SEE_ALSO}

* ブロック矢印
* block-arrow1 マクロ
* with-block-arrow-options マクロ

${NO_NOTES}


<!-- autolink: [block-arrow2 マクロ](#macro block-arrow2) -->

${BLANK_PARAGRAPH}

#### macro brace

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{brace}} position direction width height ${KEY} pivot r point text font stroke layer filter id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `direction` ---- 波括弧の向きを `:upper :bottom :left :right` のいずれかで指定します。
* `width` ---- 幅を数値で指定します。
* `height` ---- 高さを数値で指定します。
* `pivot` ---- 基準点が波括弧のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `r` ---- 曲線部分の半径を数値で指定します。
* `point` ---- 中央の「つまみ」の曲線部分の端からの距離を数値で指定します。これは水平の波括弧の場合は左から、垂直の波括弧の場合は上からの距離です。
* `text` ---- 描画するテキストを文字列で指定します。改行は "~%" で表現します。
* `font` ---- フォントを指定します。
* `stroke` ----  外枠を描画する線を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードで指定します。

${DESCRIPTION}

　波括弧を描画します。複数の波括弧でスタイルを統一したい場合、with-brace-options マクロを
使うことができます。

${SEE_ALSO}

* 波括弧
* with-brace-options マクロ

${NO_NOTES}


<!-- autolink: [brace マクロ](#macro brace) -->

${BLANK_PARAGRAPH}

#### macro circle

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{circle}} position radius ${KEY} pivot fill stroke link layer id filter contents

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `radius` ---- 半径を数値で指定します。
* `pivot` ---- 基準点が正円のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 円を描画するストロークを指定します。
* `link` ---- リンクにする場合、リンク先を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します。
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します。
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します。
* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。

${DESCRIPTION}

　正円を描画します。複数の基本要素でスタイルを統一したい場合、with-options マクロを
使うことができます。

${SEE_ALSO}

* 正円

${NO_NOTES}


<!-- autolink: [circle マクロ](#macro circle) -->

${BLANK_PARAGRAPH}

#### macro connect

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{connect}} from to ${KEY} style spacing label stroke end1 end2 layer filter id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `from` ---- 接続元となる要素の ID をキーワードで指定します。point 値も指定できます。
* `to` ---- 接続先となる要素の ID をキーワードで指定します。point 値も指定できます。
* `style` ---- 接続線の引き方を指定します。詳細は [$@ 節](#コネクタ)を参照してください。
* `spacing` ---- 2 回以上折れ曲がる接続線における「自由な線分」の位置を調整するためのパラメータです。詳細は [$@ 節](#コネクタ)を参照してください。
* `label` ---- ラベルを付ける場合は指定します。
* `stroke` ---- 接続線を描画するストロークを指定します。
* `end1,end2` ---- 始端・終端に矢印などの終端マークを付ける場合は指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します。
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します。
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します。

${DESCRIPTION}

　図形要素同士を接続します。

${SEE_ALSO}

* コネクタ
* 終端マーク

${NO_NOTES}


<!-- autolink: [connect マクロ](#macro connect) -->

${BLANK_PARAGRAPH}

#### function copy-point

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{copy-point}} pt => result

<!-- stack:pop li -->

${ARGS_AND_VALS}

* pt ---- コピー対象の座標値を指定します。
* result ---- コピーされた座標値が返ります。

${DESCRIPTION}

　座標値をコピーします。あまり使いません。

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [copy-point 関数](#function copy-point) -->

${BLANK_PARAGRAPH}

#### macro cross

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{cross}} position width height size ${KEY} pivot size-v intersection fill stroke filter rotate link layer id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ----  描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `width` ----  幅を数値で指定します。
* `height` ----  高さを数値で指定します。
* `size` ----  太さを指定します。size-v を指定しない限り、縦横両方の太さになります。
* `pivot` ----  基準点が十字のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `size-v` ----  縦棒の太さを指定します。省略した場合、size と同じになります。
* `intersection` ----  縦横の棒が交差する点を center からどれだけズラすかを `'(x y)` の要領で指定します。省略すると `'(0 0)` として扱われます。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ----  外枠を描画する線を指定します。
* `link` ---- リンクにする場合、リンク先を指定します。
* `rotate` ---- 全体を回転させたい場合に、その角度を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します

${DESCRIPTION}

　十字を描画します。複数の十字でスタイルを統一したい場合、with-cross-options マクロを
使うことができます。

${SEE_ALSO}

* 十字
* with-cross-options マクロ

${NO_NOTES}


<!-- autolink: [cross マクロ](#macro cross) -->

${BLANK_PARAGRAPH}

#### macro cube

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{cube}} position width height text ${KEY} pivot depth align valign margin font fill fill2 stroke link rotate layer id filter contents

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `width` ----  幅を数値で指定します。
* `height` ----  高さを数値で指定します。
* `text` ----  内部に描画するテキストを文字列で指定します。改行は "~%" で表現します。
* `pivot` ----  基準点がキューブのどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `depth` ----  上面および側面の大きさを数値で指定します。省略した場合のデフォルト値は height の 1/5 です。
* `align` ----  テキストの水平方向のアライメントを `:left :center :right` のいずれかで指定します。省略した場合のデフォルト値は `:center` です。
* `valign` ----  テキストの垂直方向のアライメントを `:top :center :bottom` のいずれかで指定します。省略した場合のデフォルト値は `:center` です。
* `margin` ----  テキスト描画における「余白」のサイズです。省略した場合のデフォルト値は 10 です。
* `font` ---- フォントを指定します。
* `fill` ---- 内部の塗り潰しを指定します。
* `fill2` ----  上面および側面の塗り潰しを指定します。
* `stroke` ----  外枠を描画する線を指定します。
* `link` ---- リンクにする場合、リンク先を指定します。
* `rotate` ---- 全体を回転させたい場合に、その角度を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。

${DESCRIPTION}

　キューブを描画します。複数のキューブでスタイルを統一したい場合、with-cube-options マクロを
使うことができます。

${SEE_ALSO}

* キューブ
* with-cube-options マクロ

${NO_NOTES}


<!-- autolink: [cube マクロ](#macro cube) -->

${BLANK_PARAGRAPH}

#### macro cylinder

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{cylinder}} position width height text ${KEY} pivot depth align valign margin font fill stroke link rotate layer id filter contents

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `width` ----  幅を数値で指定します。
* `height` ----  高さを数値で指定します。
* `text` ----  内部に描画するテキストを文字列で指定します。改行は "~%" で表現します。
* `pivot` ----  基準点が円柱のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `depth` ----  下部の曲線の深さを指定します。省略した場合のデフォルト値は height の 1/5 です。
* `align` ----  テキストの水平方向のアライメントを `:left :center :right` のいずれかで指定します。省略した場合のデフォルト値は `:center` です。
* `valign` ----  テキストの垂直方向のアライメントを `:top :center :bottom` のいずれかで指定します。省略した場合のデフォルト値は `:center` です。
* `margin` ----  テキスト描画における「余白」のサイズです。省略した場合のデフォルト値は 10 です。
* `font` ----  フォントを指定します。
* `fill` ----  内部の塗り潰しを指定します。
* `stroke` ----  外枠を描画する線を指定します。
* `link` ----  リンクにする場合、リンク先を指定します。
* `rotate` ----  回転させたい場合に、その角度を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。

${DESCRIPTION}

　円柱を描画します。複数の円柱でスタイルを統一したい場合、with-cylinder-options マクロを
使うことができます。

${SEE_ALSO}

* 円柱
* with-cylinder-options マクロ

${NO_NOTES}


<!-- autolink: [cylinder マクロ](#macro cylinder) -->

${BLANK_PARAGRAPH}

#### macro defgradient

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{defgradient}} (type id ${REST} params) ${REST} stops

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `type` ---- グラデーションの種類を `:linear` または `:radial` から選択します。
* `id` ---- ID をキーワードで指定します。
* `params` ---- グラデーションの付加パラメータを指定します。 `type` により異なるため後述します。
* `stops` ---- グラデーションストップをリストで指定します。詳細は [$@ 節](#グラデーション)を参照してください。後述する `href` パラメータで他の定義を参照する場合は指定する必要はありません。


${DESCRIPTION}

　与えられたパラメータでグラデーションを定義します。グラデーションの詳細は 
[$@ 節](#グラデーション)を参照してください。説明不足ですが、基本的に SVG 規格に沿って
いますので必要に応じて書籍や規格にあたってください。

　`params` はグラデーションの種類（linear/radial）によって異なりますが、いずれの場合でも
名前付きパラメータとして扱われます。まず、共通のパラメータを以下に示します。

* `href` ---- 他のグラデーション定義を参照する場合、その ID をキーワードで指定します。
* `units` ---- patternUnits 属性を `:userSpaceOnUse` または `:objectBoundingBox` から選択します。
* `spread` ---- spreadMethod 属性を指定する場合、 `:pad :repeat :reflect` から選択します。
* `transform` ---- gradientTtransform 属性を指定する場合、文字列で指定します。

${BLANK_PARAGRAPH}

　線型グラデーション特有の付加パラメータは以下の通りです。

* `x1` ---- x1 属性を指定する場合、数値または文字列で指定します。
* `y1` ---- y1 属性を指定する場合、数値または文字列で指定します。
* `x2` ---- x2 属性を指定する場合、数値または文字列で指定します。
* `y2` ---- y2 属性を指定する場合、数値または文字列で指定します。


　円形グラデーション特有の付加パラメータは以下の通りです。

* `cx` ---- cx 属性を指定する場合、数値または文字列で指定します。
* `cy` ---- cy 属性を指定する場合、数値または文字列で指定します。
* `fx` ---- fx 属性を指定する場合、数値または文字列で指定します。
* `fy` ---- fy 属性を指定する場合、数値または文字列で指定します。
* `radius` ---- r 属性を指定する場合、数値または文字列で指定します。

${SEE_ALSO}

* [](#グラデーション)

${NO_NOTES}


<!-- autolink: [defgradient マクロ](#macro defgradient) -->

${BLANK_PARAGRAPH}

#### macro defgroup

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{defgroup}} (width height id) ${BODY} body
* ${{B}{defs}} (width height id) ${BODY} body

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `width` ---- 幅を数値で指定します。
* `height` ---- 高さを数値で指定します。
* `id` ---- 定義に与える ID をキーワードで指定します。
* `body` ---- 定義内での描画コードを記述します。

${DESCRIPTION}

　幅 `width` 、高さ `height` の定義を作成し、内部を `body` のコードで描画します。
作成された定義は use マクロで `id` を指定することで繰り返し利用できます。

${SEE_ALSO}

* [](#定義と再使用)
* use マクロ

${NO_NOTES}


<!-- autolink: [defgroup マクロ](#macro defgroup) -->

${BLANK_PARAGRAPH}

#### macro defpattern

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{defpattern}} (id ${KEY} x y width height href units content-units view-box transform) ${BODY} body

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `id` ---- ID をキーワードで指定します。
* `x` ---- ${{TODO}{まだ記述されていません。}}
* `y` ---- ${{TODO}{まだ記述されていません。}}
* `width` ---- 幅を数値で指定します。
* `height` ---- 高さを数値で指定します。
* `href` ---- ${{TODO}{まだ記述されていません。}}
* `units` ---- patternUnits 属性を `:userSpaceOnUse` または `:objectBoundingBox` から選択します。
* `content-units` ---- patternContentUnits 属性を `:userSpaceOnUse` または `:objectBoundingBox` から選択します。
* `view-box` ---- viewBox 属性を 4 つの数値からなるリストで指定します。
* `transform` ---- patternTtransform 属性を指定する場合、文字列で指定します。

${DESCRIPTION}

　与えられたパラメータでパターンを定義します。パターンの詳細は「[$@ 節](#パターン)」を参照
してください。説明不足ですが、基本的に SVG 規格に沿っていますので必要に応じて書籍や規格に
あたってください。

${SEE_ALSO}

* [](#パターン)

${NOTES}

　現在、 `preserveAspectRatio` 属性には対応していません。将来対応する可能性はありますが、
未確定です。


<!-- autolink: [defpattern マクロ](#macro defpattern) -->

${BLANK_PARAGRAPH}

#### macro diagram

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{diagram}} (width height ${KEY} fill) ${BODY} body

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `width` ---- 幅を数値で指定します。
* `height` ---- 高さを数値で指定します。
* `fill` ---- 背景全体の塗り潰しが必要な場合は指定します。
* `body` ---- 描画コードを記述します。

${DESCRIPTION}

　幅 `width` 、高さ `height` の SVG 画像を作成し、内部を `body` のコードで描画します。

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [diagram マクロ](#macro diagram) -->

${BLANK_PARAGRAPH}

#### macro diamond

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{diamond}} position width height ${KEY} pivot fill stroke rotate link layer id filter contents

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `width` ---- 幅を数値で指定します。
* `height` ---- 高さを数値で指定します。
* `pivot` ---- 基準点がひし形のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 外枠を描画するストロークを指定します。
* `rotate` ---- 全体を回転させたい場合に、その角度を指定します。
* `link` ---- リンクにする場合、リンク先を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します。
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します。
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します。
* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。

${DESCRIPTION}

　ひし形を描画します。複数の基本要素でスタイルを統一したい場合、with-options マクロを
使うことができます。

${SEE_ALSO}

* ひし形

${NO_NOTES}


<!-- autolink: [diamond マクロ](#macro diamond) -->

${BLANK_PARAGRAPH}

#### macro document

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{document}} position width height text ${KEY} pivot depth align valign margin font fill stroke link rotate layer id filter contents

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `width` ---- 幅を数値で指定します。
* `height` ---- 高さを数値で指定します。
* `text` ---- 内部に描画するテキストを文字列で指定します。改行は "~%" で表現します。
* `pivot` ---- 基準点がドキュメントのどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `depth` ---- 下部の曲線の深さを指定します。省略した場合のデフォルト値は height の 1/3 です。
* `align` ---- テキストの水平方向のアライメントを `:left :center :right` のいずれかで指定します。省略した場合のデフォルト値は `:center` です。
* `valign` ---- テキストの垂直方向のアライメントを `:top :center :bottom` のいずれかで指定します。省略した場合のデフォルト値は `:center` です。
* `margin` ---- テキスト描画における「余白」のサイズです。省略した場合のデフォルト値は 10 です。
* `font` ---- フォントを指定します。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 外枠を描画するストロークを指定します
* `link` ---- リンクにする場合、リンク先を指定します。
* `rotate` ---- 全体を回転させたい場合に、その角度を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。

${DESCRIPTION}

　ドキュメントを描画します。複数のドキュメントでスタイルを統一したい場合、
with-document-options マクロを使うことができます。

${SEE_ALSO}

* ドキュメント
* with-document-options マクロ

${NO_NOTES}


<!-- autolink: [document マクロ](#macro document) -->

${BLANK_PARAGRAPH}

#### macro drop-shadow

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{drop-shadow}} ${KEY} id color-matrix deviation dx dy

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `id` ---- ID をキーワードで指定します。省略した場合のデフォルト値は `:drop-shadow` です。
* `color-matrix` ---- `<feColorMatrix>` の values 値を数値のリストで指定します。詳細は SVG 規格を参照してください。省略した場合のデフォルト値は `'(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.4 0)` です。
* `deviation` ---- `<feGaussianBlur>` における `stdDeviation` 値を指定します。詳細は SVG 規格を参照してください。省略した場合のデフォルト値は 2 です。
* `dx` ---- `<feOffset>` における `dx` 値を指定します。詳細は SVG 規格を参照してください。省略した場合のデフォルト値は 4 です。
* `dy` ---- `<feOffset>` における `dy` 値を指定します。詳細は SVG 規格を参照してください。省略した場合のデフォルト値は 4 です。

${DESCRIPTION}

　生成画像にドロップシャドウを導入します。詳細は SVG 規格を参照してください。

${SEE_ALSO}

* [](#フィルタ)

${NO_NOTES}


<!-- autolink: [drop-shadow マクロ](#macro drop-shadow) -->

${BLANK_PARAGRAPH}

#### macro ellipse

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{ellipse}} position rx ry ${KEY} pivot fill stroke rotate link layer id filter contents

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `rx` ---- x 軸方向の半径を数値で指定します。
* `ry` ---- y 軸方向の半径を数値で指定します。
* `pivot` ---- 基準点が楕円のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 楕円を描画するストロークを指定します。
* `rotate` ---- 全体を回転させたい場合に、その角度を指定します。
* `link` ---- リンクにする場合、リンク先を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します。
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します。
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します。
* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。

${DESCRIPTION}

　楕円を描画します。複数の基本要素でスタイルを統一したい場合、with-options マクロを
使うことができます。

${SEE_ALSO}

* 楕円

${NO_NOTES}


<!-- autolink: [ellipse マクロ](#macro ellipse) -->

${BLANK_PARAGRAPH}

#### macro explosion1

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{explosion1}} position width height text ${KEY} pivot font fill stroke link rotate layer id filter contents

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ----  描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `width` ----  幅を数値で指定します。
* `height` ----  高さを数値で指定します。
* `text` ----  内部に描画するテキストを文字列で指定します。改行は "~%" で表現します。
* `pivot` ----  基準点が爆発のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `font` ---- フォントを指定します。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ----  外枠を描画する線を指定します。
* `link` ---- リンクにする場合、リンク先を指定します。
* `rotate` ---- 全体を回転させたい場合に、その角度を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。

${DESCRIPTION}

　爆発を描画します。複数の爆発でスタイルを統一したい場合、with-explosion-options マクロを
使うことができます。

${SEE_ALSO}

* 爆発
* explosion2 マクロ
* with-explosion-options マクロ

${NO_NOTES}


<!-- autolink: [explosion1 マクロ](#macro explosion1) -->

${BLANK_PARAGRAPH}

#### macro explosion2

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{explosion2}} position width height text ${KEY} pivot font fill stroke link rotate layer id filter contents

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ----  描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `width` ----  幅を数値で指定します。
* `height` ----  高さを数値で指定します。
* `text` ----  内部に描画するテキストを文字列で指定します。改行は "~%" で表現します。
* `pivot` ----  基準点が爆発のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `font` ---- フォントを指定します。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ----  外枠を描画する線を指定します。
* `link` ---- リンクにする場合、リンク先を指定します。
* `rotate` ---- 全体を回転させたい場合に、その角度を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。

${DESCRIPTION}

　爆発を描画します。複数の爆発でスタイルを統一したい場合、with-explosion-options マクロを
使うことができます。

${SEE_ALSO}

* 爆発
* explosion1 マクロ
* with-explosion-options マクロ

${NO_NOTES}


<!-- autolink: [explosion2 マクロ](#macro explosion2) -->

${BLANK_PARAGRAPH}

#### macro folder

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{folder}} position text ${KEY} pivot width height tab-width tab-height align valign font fill stroke margin link rotate layer filter id contents

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `text` ---- 内部に描画するテキストを文字列で指定します。改行は "~%" で表現します。
* `pivot` ---- 基準点がフォルダのどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `width` ---- 幅を自動決定せず、明示的に指定したい場合に数値で指定します。
* `height` ---- 高さを自動決定せず、明示的に指定したい場合に数値で指定します。
* `tab-width` ---- 左上に描画されるタブ部分の幅を指定します。省略した場合のデフォルト値は 50 です。
* `tab-height` ---- 左上に描画されるタブ部分の高さを指定します。省略した場合のデフォルト値は 20 です。
* `align` ---- テキストの水平方向のアライメントを `:left :center :right` のいずれかで指定します。省略した場合のデフォルト値は `:center` です。
* `valign` ---- テキストの垂直方向のアライメントを `:top :center :bottom` のいずれかで指定します。省略した場合のデフォルト値は `:center` です。
* `font` ---- フォントを指定します。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 外枠を描画するストロークを指定します。
* `margin` ---- テキスト描画における「余白」のサイズです。省略した場合のデフォルト値は 10 です。
* `link` ---- リンクにする場合、リンク先を指定します。
* `rotate` ---- 全体を回転させたい場合に、その角度を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します。
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します。
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します。
* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。

${DESCRIPTION}

　フォルダを描画します。複数のフォルダでスタイルを統一したい場合、
with-folder-options マクロを使うことができます。

${SEE_ALSO}

* フォルダ
* with-folder-options マクロ

${NO_NOTES}


<!-- autolink: [folder マクロ](#macro folder) -->

${BLANK_PARAGRAPH}

#### macro glow-shadow

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{glow-shadow}} ${KEY} id color-matrix deviation


<!-- stack:pop li -->

${ARGS_AND_VALS}

* `id` ---- ID をキーワードで指定します。省略した場合のデフォルト値は `:glow-shadow` です。
* `color-matrix` ---- `<feColorMatrix>` の values 値を数値のリストで指定します。詳細は SVG 規格を参照してください。省略した場合のデフォルト値は `'(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0)` です。
* `deviation` ---- `<feGaussianBlur>` における `stdDeviation` 値を指定します。詳細は SVG 規格を参照してください。省略した場合のデフォルト値は 3 です。

${DESCRIPTION}

　生成画像にグローシャドウを導入します。詳細は SVG 規格を参照してください。

${SEE_ALSO}

* [](#フィルタ)

${NO_NOTES}


<!-- autolink: [glow-shadow マクロ](#macro glow-shadow) -->

${BLANK_PARAGRAPH}

#### macro grid

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{grid}} ${KEY} (size 10) (bgcolor :white) stroke layer

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `size` ---- 挿入する SVG コード片を文字列で指定します。
* `bgcolor` ---- 背景の塗り潰しを指定します。
* `stroke` ----  グリッド線を描画するストロークを指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します。

${DESCRIPTION}

　グリッド線を描画します。現状では常に SVG 画像全体を埋める矩形のパターンとなります。
（サブキャンバスなど）特定の領域だけにグリッド線を描画することはできません。

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [grid マクロ](#macro grid) -->

${BLANK_PARAGRAPH}

#### macro image

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{image}} position filename ${KEY} pivot width height label link rotate layer id filter contents

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `filename` ---- 埋め込む画像のファイル名を指定します。
* `pivot` ---- 基準点が画像のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `width` ---- 幅を自動決定せず、明示的に指定したい場合に数値で指定します。
* `height` ---- 高さを自動決定せず、明示的に指定したい場合に数値で指定します。
* `label` ---- ラベルを付ける場合は指定します。
* `link` ---- リンクにする場合、リンク先を指定します。
* `rotate` ---- 全体を回転させたい場合に、その角度を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。

${DESCRIPTION}

　SVG 図面の中にラスタ画像を埋め込みます。

${SEE_ALSO}

* 画像ファイルの埋め込み

${NOTES}

　ほとんどの場合、kaavio は画像ファイルの実際のサイズを調べる必要があります。そのため、
`filename` で指定した画像ファイルは kaavio が動作する時点で「その場所に」存在していなければ
なりません。例外は、 `width` および `height` パラメータの両方を明示的に指定した場合で、その
場合に限り kaavio は画像ファイルにアクセスしません。


<!-- autolink: [image マクロ](#macro image) -->

${BLANK_PARAGRAPH}

#### function layer

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{layer}} name ${OPTIONAL} (display :inline)

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `name` ---- レイヤーの名前をキーワードで指定します。
* `display` ---- レイヤー全体の表示を `:inline` または `:none` で指定します。省略時のデフォルトは `:inline` で、レイヤー全体を表示します。 `:none` を指定すると、レイヤー全体を非表示にします。

${DESCRIPTION}

　レイヤーの導入を宣言します。レイヤーについては [$@ 節](#レイヤー)を参照してください。

${SEE_ALSO}

* [](#レイヤー)

${NOTES}

　layer 関数は、実際には diagram マクロ内で作成される局所関数です。


<!-- autolink: [layer 関数](#function layer) -->

${BLANK_PARAGRAPH}

#### macro line

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{line}} points ${KEY} stroke label end1 end2 layer id filter

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `points` ---- 線を構成する点のリストを指定します。
* `stroke` ---- 線を描画するストロークを指定します。
* `label` ---- ラベルをつける場合に指定します。
* `end1` ---- 始端に終端マークをつける場合は指定します
* `end2` ---- 終端に終端マークをつける場合は指定します
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します。
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します。
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します。

${DESCRIPTION}

　`points` で指定された点を順番に通過する直線を描画します。 `points` で指定する点のリストは
最低限でも２要素以上である必要があり、始点 [通過点]... 終点の要領で指定します。

${SEE_ALSO}

* コネクタ

${NO_NOTES}


<!-- autolink: [line マクロ](#macro line) -->

${BLANK_PARAGRAPH}

#### function make-endmark

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{make-endmark}} ${REST} params

<!-- stack:pop li -->

${DESCRIPTION}

　指定されたパラメータで終端マーク情報を生成します。終端マーク情報の詳細は「[](#終端マーク)」を
参照してください。上記は簡潔な記述で柔軟な終端マーク情報の生成を可能にするためのもので、 
`params` として渡されるパラメータ数に応じて以下のことをします。

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

　各パラメータの意味は以下の通りです。詳細は「[](#終端マーク)」を参照してください。

* `type` ---- 終端マークの形状を指定します。 `:arrow :triangle :diamond :circle :rect` のいずれか、またはカスタム描画関数を指定します。
* `size` ---- 終端マークのサイズを指定します。 `:small :medium :large :xlarge` のいずれか、または数値を指定します。
* `stroke` ---- 終端マークを描画するストロークを指定します。省略した場合、実際に終端マークが描画される対象（直線やコネクタ）のストロークが使用されます。ただしその場合、そのストロークの `:dasharray` など一部のパラメータは引き継がれません。
* `fill` ---- 終端マーク内部の塗り潰しを指定します。省略した場合、ストロークと同じ色で塗り潰されます。

${SEE_ALSO}

* [](#終端マーク)

${NOTES}

　`type` パラメータでキーワードのかわりに指定するカスタム描画関数ですが、以下のシグネチャの
関数を指定する必要があります。これは現状では暫定的で undocumented な状態です。詳細はコードを
参照してください。

```lisp
(lambda (points size stroke fill writer) ...)
```


<!-- autolink: [make-endmark 関数](#function make-endmark) -->

${BLANK_PARAGRAPH}

#### function make-fill

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{make-fill}} ${REST} params

<!-- stack:pop li -->

${DESCRIPTION}

　指定されたパラメータでフィル情報を生成します。フィル情報の詳細は「[](#フィル)」を参照して
ください。上記は簡潔な記述で柔軟なフィル情報の生成を可能にするためのもので、 `params` として
渡されるパラメータ数に応じて以下のことをします。

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

　各パラメータの意味は以下の通りです。詳細は [$@ 節](#フィル)を参照してください。

* `color` ---- 塗り潰しの色を指定します。色の指定方法については [$@ 節](#色の指定)を、色の名前については [$@ 節](#色の名前)を参照してください。
* `opacity` ---- 塗り潰しの不透明度を 0.0 ～ 1.0 の数値で指定します。省略した場合のデフォルト値は 1.0 です。
* `rule` ---- 塗りつぶしのルールを `:nonzero` または `:evenodd` で指定します。
* `url` ---- パターンやグラデーションを使用する場合、その ID をキーワードシンボルで指定します。詳細は [$@ 節](#フィルにおけるパターンとグラデーションの指定)を参照してください。
* `base` ---- フィル情報の作成においてベースとする他のフィル情報があれば指定します。

${SEE_ALSO}

* [](#フィル)

${NO_NOTES}


<!-- autolink: [make-fill 関数](#function make-fill) -->

${BLANK_PARAGRAPH}

#### function make-font

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{make-font}} ${REST} params

<!-- stack:pop li -->

${DESCRIPTION}

　指定されたパラメータでフォント情報を生成します。フォント情報の詳細は「[](#フォント)」を
参照してください。上記は簡潔な記述で柔軟なフォント情報の生成を可能にするためのもので、 
`params` として渡されるパラメータ数に応じて以下のことをします。

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

　各パラメータの意味は以下の通りです。詳細は [$@ 節](#フォント)を参照してください。

* `family` ---- 使用するフォントの名前をカンマで区切って並べた文字列を指定します。
* `size` ---- フォントサイズを数値で指定します。
* `fill` ---- フォントの塗り潰しを指定します。
* `stroke` ---- フォントの輪郭を描くストロークを指定します。通常、フォントでは輪郭線は指定しません。
* `style` ---- スタイルを `:normal, :italic, :oblique` のいずれかから指定します。
* `decoration` ---- 装飾を `:none, :underline, :overline, :line-through` のいずれかから指定します。
* `weight` ---- 文字の太さを `:normal, :bold, :bolder, :lighter` のいずれか、または 100 200 300 400 500 600 700 800 900 のいずれかから指定します。
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `line-spacing` ---- パラグラフなどで複数行のテキストを描画する際の行間を数値で指定します。
* `width-spice` ---- フォントサイズとテキスト内容から描画幅を計算する際の係数を数値で指定します。
* `base` ---- フォント情報の作成においてベースとする他のフォント情報があれば指定します。

${SEE_ALSO}

* [](#フォント)

${NO_NOTES}


<!-- autolink: [make-font 関数](#function make-font) -->

${BLANK_PARAGRAPH}

#### function make-id

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{make-id}} prefix ${REST} args => result

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `prerix` ---- 文字列またはシンボルを指定します。
* `args` ---- 任意の数の文字列またはシンボルを指定します。
* `result` ---- 結果がキーワードシンボルで返ります。

${DESCRIPTION}

　`prefix` および追加の `args` を連結した ID を生成し、キーワードシンボルのかたちで
返します。

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [make-id 関数](#function make-id) -->

${BLANK_PARAGRAPH}

#### function make-label

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{make-label}} ${REST} params

<!-- stack:pop li -->

${DESCRIPTION}

　指定されたパラメータでラベル情報を生成します。ラベル情報の詳細は「[](#ラベル)」を参照して
ください。上記は簡潔な記述で柔軟なラベル情報の生成を可能にするためのもので、 `params` として
渡されるパラメータ数に応じて以下のことをします。

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

* `text` ---- ラベルのテキストを文字列かキーワードシンボルで指定します。
* `position` ---- ラベルを表示する位置を `:above :below :left :right` のいずれかで指定します。直線やコネクタでは無視されます。
* `offset` ---- ラベルの表示位置を微調整する `(x y)` 値を指定します。
* `font` ---- ラベルの描画に使用するフォントを指定します。

${SEE_ALSO}

* [](#ラベル)

${NO_NOTES}


<!-- autolink: [make-label 関数](#function make-label) -->

${BLANK_PARAGRAPH}

#### function make-link

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{make-link}} ${REST} params

<!-- stack:pop li -->

${DESCRIPTION}

　指定されたパラメータでリンク情報を生成します。リンク情報の詳細は「[](#リンク)」を参照して
ください。上記は簡潔な記述で柔軟なリンク情報の生成を可能にするためのもので、 `params` として
渡されるパラメータ数に応じて以下のことをします。

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

* `url` ---- リンク先の URL を文字列で指定します。  |
* `target` ---- `a` タグにおける `target` をキーワード `:replace :self :parent :top :blank` のいずれかで指定します。省略可能です。

${SEE_ALSO}

* [](#リンク)

${NO_NOTES}


<!-- autolink: [make-link 関数](#function make-link) -->

${BLANK_PARAGRAPH}

#### function make-point

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{make-point}} x y ${OPTIONAL} type => result

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `x` ---- x 座標を数値で指定します。
* `y` ---- y 座標を数値で指定します。
* `type` ---- `:relative` または `:absolute` を指定します。省略した場合のデフォルト値は `:relative` です。
* `result` ---- 座標値が返ります。

${DESCRIPTION}

　座標値を生成します。 `type` に `absolute` を指定すると絶対座標に、それ以外の場合は
相対座標になります。絶対座標は画像全体をキャンバスとしてその左上を `'(0 0)` とする座標
で、相対座標は「現在のキャンバス」をの左上を `'(0 0)` とする座標です。

${SEE_ALSO}

* [](#座標と位置)
* [](#サブキャンバス)

${NO_NOTES}


<!-- autolink: [make-point 関数](#function make-point) -->

${BLANK_PARAGRAPH}

#### function make-stroke

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{make-stroke}} ${REST} params

<!-- stack:pop li -->

${DESCRIPTION}

　指定されたパラメータでストローク情報を生成します。ストローク情報の詳細は「[](#ストローク)」を
参照してください。上記は簡潔な記述で柔軟なストローク情報の生成を可能にするためのもので、 
`params` として渡されるパラメータ数に応じて以下のことをします。

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

　各パラメータの意味は以下の通りです。詳細は [$@ 節](#ストローク)を参照してください。

* `color` ---- 線の色を指定します。色の指定方法については [$@ 節](#色の指定)を、色の名前については [$@ 節](#色の名前)を参照してください。
* `width` ---- 線の幅を数値で指定します。
* `opacity` ---- 線の不透明度を 0.0 ～ 1.0 の数値で指定します。省略した場合のデフォルト値は 1.0 です。
* `linecap` ---- 線の両端の形状を `:butt, :round, :square` から指定します。
* `linejoin` ---- 線が折れ曲ってできる角の形状を `:miter, :round, :bevel` から指定します。
* `miterlimit` ---- `linejoin` が `:miter` の場合の、結合される線の太さに対する結合部の長さの比率を数値で指定します。デフォルト値は 4 です。
* `dasharray` ---- 点線や破線を描画したい場合に、繰り返される線の長さと間隔の長さをリストで指定します。デフォルト値は nil で、直線になります。
* `dashoffset` ---- `dasharray` を指定する場合に、線の開始を `dasharray` のどこから始めるかのオフセットを数値で指定します。
* `url` ---- パターンやグラデーションを使用する場合、その ID をキーワードシンボルで指定します。詳細は [$@ 節](#ストロークにおけるパターンとグラデーションの指定)を参照してください。
* `base` ---- ストローク情報の作成においてベースとする他のストローク情報があれば指定します。

${SEE_ALSO}

* [](#ストローク)

${NO_NOTES}


<!-- autolink: [make-stroke 関数](#function make-stroke) -->

${BLANK_PARAGRAPH}

#### function make-uml-keyword

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{make-uml-keyword}} ${REST} params


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [make-uml-keyword 関数](#function make-uml-keyword) -->

${BLANK_PARAGRAPH}

#### function make-uml-multiplicity

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{make-uml-multiplicity}} ${REST} params


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [make-uml-multiplicity 関数](#function make-uml-multiplicity) -->

${BLANK_PARAGRAPH}

#### function make-uml-role

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{make-uml-role}} ${REST} params


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [make-uml-role 関数](#function make-uml-role) -->

${BLANK_PARAGRAPH}

#### function make-uml-transition-spec

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{make-uml-transition-spec}} ${REST} params


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [make-uml-transition-spec 関数](#function make-uml-transition-spec) -->

${BLANK_PARAGRAPH}

#### macro memo

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{memo}} position text ${KEY} pivot width height crease align valign margin font fill fill2 stroke link rotate layer id filter contents

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `text` ---- 内部に描画するテキストを文字列で指定します。改行は "~%" で表現します。
* `pivot` ---- 基準点がメモのどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `width` ---- 幅を自動決定せず、明示的に指定したい場合に数値で指定します。
* `height` ---- 高さを自動決定せず、明示的に指定したい場合に数値で指定します。
* `crease` ---- 右下の折り目のサイズを数値で指定します。省略した場合のデフォルト値は 20 です。
* `align` ---- テキストの水平方向のアライメントを `:left :center :right` のいずれかで指定します。省略した場合のデフォルト値は `:center` です。
* `valign` ---- テキストの垂直方向のアライメントを `:top :center :bottom` のいずれかで指定します。省略した場合のデフォルト値は `:center` です。
* `margin` ---- テキスト描画における「余白」のサイズです。省略した場合のデフォルト値は 10 です。
* `font` ---- フォントを指定します。
* `fill` ---- 内部の塗り潰しを指定します。
* `fill2` ---- 折り目部分の塗り潰しを指定します。
* `stroke` ---- 外枠を描画するストロークを指定します
* `link` ---- リンクにする場合、リンク先を指定します。
* `rotate` ---- 全体を回転させたい場合に、その角度を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。

${DESCRIPTION}

　メモを描画します。複数のメモでスタイルを統一したい場合、with-memo-options マクロを
使うことができます。

${SEE_ALSO}

* メモ
* with-memo-options マクロ

${NO_NOTES}


<!-- autolink: [memo マクロ](#macro memo) -->

${BLANK_PARAGRAPH}

#### macro paragraph

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{paragraph}} position text ${KEY} align valign rotate font link layer id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画するパラグラフの基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `text` ---- 描画するパラグラフを文字列で指定します。改行は "~%" で表現します。
* `align` ---- パラグラフの水平方向のアライメントを `:left :center :right` のいずれかで指定します。
* `valign` ---- パラグラフの垂直方向のアライメントを `:top :center :bottom` のいずれかで指定します。
* `font` ---- フォントを指定します。
* `link` ---- リンクにする場合、リンク先を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します

${DESCRIPTION}

　複数行に渡るパラグラフを描画します。単一行のテキストであれば text マクロを、テキストボックス
を使いたい場合は textbox マクロを使うことができます。

${SEE_ALSO}

* パラグラフ
* テキスト
* テキストボックス

${NO_NOTES}


<!-- autolink: [paragraph マクロ](#macro paragraph) -->

${BLANK_PARAGRAPH}

#### macro parallelogram

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{parallelogram}} position width height direction offset ${KEY} pivot fill stroke rotate link layer id filter contents

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `width` ---- 幅を数値で指定します。
* `height` ---- 高さを数値で指定します。
* `direction` ---- 平行四辺形の「向き」を `:h` または `:v` で指定します。
* `offset` ---- 平行四辺形の形状に関するオフセット値を数値で指定します。
* `pivot` ---- 基準点が平行四辺形のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 外枠を描画するストロークを指定します。
* `rotate` ---- 全体を回転させたい場合に、その角度を指定します。
* `link` ---- リンクにする場合、リンク先を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します。
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します。
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します。
* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。

${DESCRIPTION}

　平行四辺形を描画します。複数の基本要素でスタイルを統一したい場合、with-options マクロを
使うことができます。

${SEE_ALSO}

* 平行四辺形

${NO_NOTES}


<!-- autolink: [parallelogram マクロ](#macro parallelogram) -->

${BLANK_PARAGRAPH}

#### macro path

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{path}} data ${KEY} fill stroke layer filter id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `data` ---- パスを構成するデータを指定します。詳細は [$@ 節](#パス)を参照してください。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 線を描画するストロークを指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します。
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します。
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します。

${DESCRIPTION}

　パスを描画します。

${SEE_ALSO}

* パス

${NO_NOTES}


<!-- autolink: [path マクロ](#macro path) -->

${BLANK_PARAGRAPH}

#### macro person

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{person}} position size ${KEY} pivot fill stroke label link rotate layer filter id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `size` ---- 幅を数値で指定します。高さは自動的にこの 2 倍になります。
* `pivot` ---- 基準点が人物のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 外枠を描画するストロークを指定します。
* `label` ---- ラベルを付ける場合は指定します。
* `link` ---- リンクにする場合、リンク先を指定します。
* `rotate` ---- 全体を回転させたい場合に、その角度を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します。
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します。
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します。

${DESCRIPTION}

　人物を描画します。複数の人物でスタイルを統一したい場合、with-person-options マクロを
使うことができます。

${SEE_ALSO}

* 人物
* with-person-options マクロ

${NO_NOTES}


<!-- autolink: [person マクロ](#macro person) -->

${BLANK_PARAGRAPH}

#### function point*

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{point*}} pt n => result

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `pt` ---- 対象の座標値を指定します。
* `n` ---- 対象の座標値を指定します。
* `result` ---- 結果の座標値が返ります。

${DESCRIPTION}

　`pt`を `n` 倍した座標値を返します。function pt* と同じことをします。

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [point* 関数](#function point*) -->

${BLANK_PARAGRAPH}

#### function point+

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{point+}} pt1 pt2 => result

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `pt1` ---- 対象の座標値を指定します。
* `pt2` ---- 対象の座標値を指定します。
* `result` ---- 結果の座標値が返ります。

${DESCRIPTION}

　2 つの座標値を足しあわせます。どちらかまたは両方が絶対座標の場合、結果も絶対座標に
なります。function pt+ と同じことをします。

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [point+ 関数](#function point+) -->

${BLANK_PARAGRAPH}

#### function point-

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{point-}} pt1 pt2 => result

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `pt1` ---- 対象の座標値を指定します。
* `pt2` ---- 対象の座標値を指定します。
* `result` ---- 結果の座標値が返ります。

${DESCRIPTION}

　`pt1` から `pt2` を引きます。どちらかまたは両方が絶対座標の場合、結果も絶対座標に
なります。function pt- と同じことをします。

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [point- 関数](#function point-) -->

${BLANK_PARAGRAPH}

#### function point-absolute-p

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{point-absolute-p}} pt => result

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `pt` ---- 対象オブジェクトを指定します。
* `result` ---- `T` または `NIL` が返ります。

${DESCRIPTION}

　`pt` が絶対座標を示す座標値かどうかを調べます。

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [point-absolute-p 関数](#function point-absolute-p) -->

${BLANK_PARAGRAPH}

#### function point-distance

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{point-distance}} pt1 pt2 => result

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `pt1` ---- 対象の座標値を指定します。
* `pt2` ---- 対象の座標値を指定します。
* `result` ---- 結果の値が返ります。

${DESCRIPTION}

　`pt1` と `pt2` の間の距離を計算して返します。この時、これらの座標値が絶対座標であるか
相対座標であるかは考慮されないので注意が必要です。

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [point-distance 関数](#function point-distance) -->

${BLANK_PARAGRAPH}

#### function point-p

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{point-p}} pt => result

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `pt` ---- 対象オブジェクトを指定します。
* `result` ---- `T` または `NIL` が返ります。

${DESCRIPTION}

　`pt` が座標値かどうかを調べます。

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [point-p 関数](#function point-p) -->

${BLANK_PARAGRAPH}

#### function point-relative-p

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{point-relative-p}} pt => result

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `pt` ---- 対象オブジェクトを指定します。
* `result` ---- `T` または `NIL` が返ります。

${DESCRIPTION}

　`pt` が相対座標を示す座標値かどうかを調べます。

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [point-relative-p 関数](#function point-relative-p) -->

${BLANK_PARAGRAPH}

#### function point-x

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{point-x}} pt => result
* (setf (${{B}{point-x}} pt) val)

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `pt` ---- 対象の座標値を指定します。
* `result` ---- `pt` の x 座標が返ります。
* `val` ---- `pt` に設定する x 座標値を指定します。

${DESCRIPTION}

　座標値の x 軸の値を取得または設定します。

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [point-x 関数](#function point-x) -->

${BLANK_PARAGRAPH}

#### function point-y

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{point-y}} pt
* (setf ${{B}{point-y}} pt) val)

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `pt` ---- 対象の座標値を指定します。
* `result` ---- `pt` の y 座標が返ります。
* `val` ---- `pt` に設定する y 座標値を指定します。

${DESCRIPTION}

　座標値の y 軸の値を取得または設定します。

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [point-y 関数](#function point-y) -->

${BLANK_PARAGRAPH}

#### function point/

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{point/}} pt n

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `pt` ---- 対象の座標値を指定します。
* `n` ---- 対象の座標値を指定します。
* `result` ---- 結果の座標値が返ります。

${DESCRIPTION}

　`pt`を `1/n` 倍した座標値を返します。function pt/ と同じことをします。

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [point/ 関数](#function point/) -->

${BLANK_PARAGRAPH}

#### function point/x+

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{point/x+}} pt x => result

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `pt` ---- 対象の座標値を指定します。
* `x` ---- 加算する x 軸の値を指定します。
* `result` ---- 結果の座標値が返ります。

${DESCRIPTION}

　`pt` に `(x 0)` を足した座標値を返します。function x+ と同じことをします。

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [point/x+ 関数](#function point/x+) -->

${BLANK_PARAGRAPH}

#### function point/xy+

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{point/xy+}} pt x y => result

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `pt` ---- 対象の座標値を指定します。
* `x` ---- 加算する x 軸の値を指定します。
* `y` ---- 加算する y 軸の値を指定します。
* `result` ---- 結果の座標値が返ります。

${DESCRIPTION}

　`pt` に `(x y)` を足した座標値を返します。function xy+ と同じことをします。

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [point/xy+ 関数](#function point/xy+) -->

${BLANK_PARAGRAPH}

#### function point/y+

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{point/y+}} pt y => result

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `pt` ---- 対象の座標値を指定します。
* `y` ---- 加算する y 軸の値を指定します。
* `result` ---- 結果の座標値が返ります。

${DESCRIPTION}

　`pt` に `(0 y)` を足した座標値を返します。function y+ と同じことをします。

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [point/y+ 関数](#function point/y+) -->

${BLANK_PARAGRAPH}

#### macro polygon

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{polygon}} points ${KEY} fill stroke link layer filter id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `points` ---- 多角形を構成する点のリストを指定します。詳細は「[](#座標と位置)」を参照してください。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 線を描画するストロークを指定します。
* `link` ---- リンクにする場合、リンク先を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します。
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します。
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します。

${DESCRIPTION}

　多角形を描画します。複数の基本要素でスタイルを統一したい場合、with-options マクロを
使うことができます。

${SEE_ALSO}

* 多角形
* 正多角形

${NO_NOTES}


<!-- autolink: [polygon マクロ](#macro polygon) -->

${BLANK_PARAGRAPH}

#### function pt*

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{pt*}} pt n => result

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `pt` ---- 対象の座標値を指定します。
* `n` ---- 対象の座標値を指定します。
* `result` ---- 結果の座標値が返ります。

${DESCRIPTION}

　`pt`を `n` 倍した座標値を返します。function point* と同じことをします。

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [pt* 関数](#function pt*) -->

${BLANK_PARAGRAPH}

#### function pt+

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{pt+}} pt1 pt2 => result

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `pt1` ---- 対象の座標値を指定します。
* `pt2` ---- 対象の座標値を指定します。
* `result` ---- 結果の座標値が返ります。

${DESCRIPTION}

　2 つの座標値を足しあわせます。どちらかまたは両方が絶対座標の場合、結果も絶対座標に
なります。function point+ と同じことをします。

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [pt+ 関数](#function pt+) -->

${BLANK_PARAGRAPH}

#### function pt-

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{pt-}} pt1 pt2 => result

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `pt1` ---- 対象の座標値を指定します。
* `pt2` ---- 対象の座標値を指定します。
* `result` ---- 結果の座標値が返ります。

${DESCRIPTION}

　`pt1` から `pt2` を引きます。どちらかまたは両方が絶対座標の場合、結果も絶対座標に
なります。function point- と同じことをします。

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [pt- 関数](#function pt-) -->

${BLANK_PARAGRAPH}

#### function pt/

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{pt/}} pt n => result

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `pt` ---- 対象の座標値を指定します。
* `n` ---- 対象の座標値を指定します。
* `result` ---- 結果の座標値が返ります。

${DESCRIPTION}

　`pt`を `1/n` 倍した座標値を返します。function point/ と同じことをします。

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [pt/ 関数](#function pt/) -->

${BLANK_PARAGRAPH}

#### macro raw-svg

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{raw-svg}} svgdata ${KEY} (layer nil)

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `svgdata` ---- 挿入する SVG コード片を文字列で指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します

${DESCRIPTION}

　SVG 図面の中に任意の SVG コードを挿入します。

${SEE_ALSO}

* 生の SVG コード片の挿入

${NO_NOTES}


<!-- autolink: [raw-svg マクロ](#macro raw-svg) -->

${BLANK_PARAGRAPH}

#### macro rect

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{rect}} position width height ${KEY} pivot rx ry fill stroke rotate link layer id filter contents

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `width` ---- 幅を数値で指定します。
* `height` ---- 高さを数値で指定します。
* `pivot` ---- 基準点が四角形のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `rx, ry` ---- 角を丸くしたい場合に、角の x 半径（rx）と y 半径（ry）を数値で指定します。rx と ry のどちらかだけを指定すると、もう一方も同じであると見なされます。省略した場合のデフォルト値は 0（つまり角を丸くしない）です。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 外枠を描画するストロークを指定します。
* `rotate` ---- 全体を回転させたい場合に、その角度を指定します。
* `link` ---- リンクにする場合、リンク先を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します。
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します。
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します。
* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。

${DESCRIPTION}

　四角形を描画します。複数の基本要素でスタイルを統一したい場合、with-options マクロを
使うことができます。

${SEE_ALSO}

* 四角形

${NO_NOTES}


<!-- autolink: [rect マクロ](#macro rect) -->

${BLANK_PARAGRAPH}

#### macro register-theme

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{register-theme}} (name ${OPTIONAL} base) ${REST} settings

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `name` ---- 作成するテーマの ID をキーワードシンボルで指定します。
* `base` ---- ベースにするテーマがある場合はその ID をキーワードシンボルで指定します。
* `settings` ---- テーマに登録する設定を指定します（[$@ 節](#新しいテーマの作成)参照）。

${DESCRIPTION}

　新しいテーマを作成します。詳細は [$@ 節](#新しいテーマの作成)および
[$@ 節](#テーマのカスタマイズ)を参照してください。

${SEE_ALSO}

* [](#新しいテーマの作成)
* [](#テーマのカスタマイズ)

${NO_NOTES}


<!-- autolink: [register-theme マクロ](#macro register-theme) -->

${BLANK_PARAGRAPH}

#### macro regular-polygon

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{regular-polygon}} position n size ${KEY} pivot fill stroke link layer id filter contents

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `n` ---- 正Ｎ角形を描く場合の N を指定します。現在、3 4 5 6 8 10 12 が使用できます。
* `size` ---- ベースとなる正円の半径を数値で指定します。
* `pivot` ---- 基準点が正円のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 円を描画するストロークを指定します。
* `rotate` ---- 全体を回転させたい場合に、その角度を指定します。
* `link` ---- リンクにする場合、リンク先を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します。
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します。
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します。
* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。

${DESCRIPTION}

　正多角形を描画します。複数の基本要素でスタイルを統一したい場合、with-options マクロを
使うことができます。

${SEE_ALSO}

* 正多角形
* 多角形

${NOTES}

　regular-polygon マクロが生成する正多角形への接続点は、 `size` を半径とする正円の上に
配置されます。


<!-- autolink: [regular-polygon マクロ](#macro regular-polygon) -->

${BLANK_PARAGRAPH}

#### function repeat

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{repeat}} value count ${REST} customs => result

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `value` ---- 数値、または一引数関数を指定します。
* `count` ---- 数値を指定します。
* `customs` ---- `(n v)` 形式のリストを任意数指定します。ここで `n v` はともに整数です。
* `result` ---- 結果がリストで返ります。

${DESCRIPTION}

　`value count customs` に従って数値のリストを生成して返します。table マクロにおける
`rows` および `cols` パラメータでの使用を想定しています。

　基本的には、この関数は `count` 個の `value` からなるリストを生成します。ただし、
`customs` が指定されている場合、生成したリストを `customs` に従って変更します。
`customs` は、 `(n v)` 形式のリストの羅列であることが期待され、これによって結果の 
`n` 番目の要素が `v` に置き換えられます。

　さらに、 `value` は数値ではなく一引数関数にすることもできます。この場合、その関数は
結果リストの個々の値を生成するためにインデックスを引数としてコールされます。以下に
それぞれの例を示します。

```lisp
(repeat 50 4)                 ; => '(50 50 50 50)
(repeat 40 5 '(0 80) '(3 60)) ; => '(80 40 40 60 40)
(repeat #'identity 10)        ; => '(0 1 2 3 4 5 6 7 8 9)
(repeat (lambda (i)
           (* 10 (1+ i))) 10)  ; => '(10 20 30 40 50 60 70 80 90 100)
```

${SEE_ALSO}

* table マクロ

${NO_NOTES}


<!-- autolink: [repeat 関数](#function repeat) -->

${BLANK_PARAGRAPH}

#### function rgb

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{rgb}} r g b => result

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `r` ---- 0 ～ 255 の整数値または 0.0 ～ 1.0 の浮動小数点値
* `g` ---- 0 ～ 255 の整数値または 0.0 ～ 1.0 の浮動小数点値
* `b` ---- 0 ～ 255 の整数値または 0.0 ～ 1.0 の浮動小数点値
* `result` ---- 結果が `"#RRGGBB"` 形式の文字列で返ります。

${DESCRIPTION}

　`r g b` の 3 値から色指定の文字列を生成指定返します。

${SEE_ALSO}

* [](#色の指定)

${NO_NOTES}


<!-- autolink: [rgb 関数](#function rgb) -->

${BLANK_PARAGRAPH}

#### macro table

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{table}} position rows cols ${KEY} pivot font fills stroke texts layer id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `rows` ---- 行数と各行の高さを数値のリストで指定します。リストの長さが行数、リスト要素の数値が行の高さです。[repeat 関数](#function repeat)が役に立つかもしれません。
* `cols` ---- 列数と各列の幅を数値のリストで指定します。リストの長さが列数、リスト要素の数値が列の幅です。[repeat 関数](#function repeat)が役に立つかもしれません。
* `pivot` ---- 基準点がテーブルのどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `stroke` ---- 罫線を描画する線を指定します。
* `fills` ---- 表、行、列、またはセル個別の塗り潰しを指定します。詳細は後述します。
* `font` ---- 表内でテキストを描画する際に使用するフォントを指定します。省略した場合、 `*default-table-font*, *default-font*` の順でデフォルトフォントが使用されます。また、 `texts` パラメータ指定の中でセル毎に個別にフォントを指定することもできます。
* `texts` ---- 表内の各セルに設定するテキストをリストで指定します。正確には、行のリストを連ねたリストで指定します。テキスト情報はキーワードなどのシンボル、数値、文字列を指定できますが、アライメントやフォント情報を指定する場合は テキスト情報自体をリストにする必要があります。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードで指定します。

${DESCRIPTION}

　テーブルを描画します。複数のテーブルでスタイルを統一したい場合、with-table-options マクロを
使うことができます。

${SEE_ALSO}

* テーブル
* with-table-options マクロ
* with-table-cell マクロ
* with-table-range マクロ

${NOTES}

　`fills` パラメータは、位置を示すキーワードとフィル情報の２つの値を繰り返すリストで指定して
ください。位置は、表全体であれば `:rc` 、列や行全体を指定する場合は `:rN` や `:cM` を指定
します。ここで、 `N,M` は行や列の番号を示す整数です（上または左から０で始まります）。 
`:rN-M` といった範囲指定も可能です。単独のセルを指定する場合、同じ要領で `:rNcM` と指定して
ください。 `:rK-LcN-M` 形式の範囲指定も可能です。 `fills` パラメータ全体が省略された場合、
表の背景は塗り潰されません。


<!-- autolink: [table マクロ](#macro table) -->

${BLANK_PARAGRAPH}

#### macro text

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{text}} position text ${KEY} align font link layer id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画するテキストの基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `text` ---- 描画するテキストを文字列で指定します。
* `align` ---- テキストのアライメントを `:left :center :right` のいずれかで指定します。省略した場合のデフォルト値は `:left` です。
* `font` ---- フォントを指定します。
* `link` ---- リンクにする場合、リンク先を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します。
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します。

${DESCRIPTION}

　テキストを描画します。複数の基本要素でスタイルを統一したい場合、with-options マクロを
使うことができます。

${SEE_ALSO}

* テキスト

${NO_NOTES}


<!-- autolink: [text マクロ](#macro text) -->

${BLANK_PARAGRAPH}

#### macro textbox

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{textbox}} position text ${KEY} pivot width height no-frame rx ry align valign margin font fill stroke link rotate layer id filter contents

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `text` ---- 内部に描画するテキストを文字列で指定します。改行は "~%" で表現します。
* `pivot` ---- 基準点がテキストボックスのどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `width` ---- 幅を自動決定せず、明示的に指定したい場合に数値で指定します。
* `height` ---- 高さを自動決定せず、明示的に指定したい場合に数値で指定します。
* `no-frame` ---- ボックスを描画せず、テキストのみにしたい場合には `:no-frame t` と指定してください。
* `rx, ry` ---- 角を丸くしたい場合に、角の x 半径と y 半径を数値で指定します。rx と ry のどちらかだけを指定すると、もう一方も同じであると見なされます。省略した場合のデフォルト値は 0（つまり角を丸くしない）です。
* `align` ---- テキストの水平方向のアライメントを `:left :center :right` のいずれかで指定します。省略した場合のデフォルト値は `:center` です。
* `valign` ---- テキストの垂直方向のアライメントを `:top :center :bottom` のいずれかで指定します。省略した場合のデフォルト値は `:center` です。
* `margin` ---- テキスト描画における「余白」のサイズです。省略した場合のデフォルト値は 10 です。
* `font` ---- フォントを指定します。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 外枠を描画するストロークを指定します
* `link` ---- リンクにする場合、リンク先を指定します。
* `rotate` ---- 全体を回転させたい場合に、その角度を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。

${DESCRIPTION}

　テキストボックスを描画します。複数のテキストボックスでスタイルを統一したい場合、
with-textbox-options マクロを使うことができます。

${SEE_ALSO}

* テキストボックス
* with-textbox-options マクロ

${NO_NOTES}


<!-- autolink: [textbox マクロ](#macro textbox) -->

${BLANK_PARAGRAPH}

#### macro uml-action

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-action}} position text ${KEY} keyword pivot width height margin corner-r rake font fill stroke link layer filter id contents


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

* rake は t を指定するか、または数値 4 要素のリストを指定する。 `(width height x-margin y-margin)`
* `:contents t` がサポートされる。

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-action マクロ](#macro uml-action) -->

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


<!-- autolink: [uml-activity-final マクロ](#macro uml-activity-final) -->

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


<!-- autolink: [uml-activity-start マクロ](#macro uml-activity-start) -->

${BLANK_PARAGRAPH}

#### macro uml-actor

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-actor}} position name ${KEY} pivot width fill stroke link layer id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-actor マクロ](#macro uml-actor) -->

${BLANK_PARAGRAPH}

#### macro uml-aggregation

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-aggregation}} from to ${KEY} arrow keyword name style spacing role1 mult1 role2 mult2 layer id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-aggregation マクロ](#macro uml-aggregation) -->

${BLANK_PARAGRAPH}

#### macro uml-association

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-association}} from to ${KEY} arrows keyword name style spacing role1 role2 mult1 mult2 layer id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-association マクロ](#macro uml-association) -->

${BLANK_PARAGRAPH}

#### macro uml-class

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-class}} position name ${KEY} pivot width height keyword multiplicity abstract active template attributes operations responsibilities (emptybox nil emptybox-p) font fill stroke link layer id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-class マクロ](#macro uml-class) -->

${BLANK_PARAGRAPH}

#### macro uml-component

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-component}} position name ${KEY} pivot keyword width height font fill stroke link layer id contents


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-component マクロ](#macro uml-component) -->

${BLANK_PARAGRAPH}

#### macro uml-composition

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-composition}} from to ${KEY} arrow keyword name style spacing role1 mult1 role2 mult2 layer id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-composition マクロ](#macro uml-composition) -->

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


<!-- autolink: [uml-connector マクロ](#macro uml-connector) -->

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


<!-- autolink: [uml-decision マクロ](#macro uml-decision) -->

${BLANK_PARAGRAPH}

#### macro uml-dependency

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-dependency}} from to ${KEY} keyword name style spacing layer id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-dependency マクロ](#macro uml-dependency) -->

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


<!-- autolink: [uml-expansion-region マクロ](#macro uml-expansion-region) -->

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


<!-- autolink: [uml-flow マクロ](#macro uml-flow) -->

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


<!-- autolink: [uml-flow-final マクロ](#macro uml-flow-final) -->

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


<!-- autolink: [uml-fork マクロ](#macro uml-fork) -->

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


<!-- autolink: [uml-frame マクロ](#macro uml-frame) -->

${BLANK_PARAGRAPH}

#### macro uml-generalization

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-generalization}} from to ${KEY} keyword name style spacing layer id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-generalization マクロ](#macro uml-generalization) -->

${BLANK_PARAGRAPH}

#### macro uml-interface

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-interface}} position name ${KEY} pivot fill stroke link layer id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-interface マクロ](#macro uml-interface) -->

${BLANK_PARAGRAPH}

#### macro uml-interface-request

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-interface-request}} from to ${KEY} arrow-p style spacing stroke layer id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-interface-request マクロ](#macro uml-interface-request) -->

${BLANK_PARAGRAPH}

#### macro uml-interface-socket

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-interface-socket}} position from name ${KEY} style spacing stroke layer id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-interface-socket マクロ](#macro uml-interface-socket) -->

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


<!-- autolink: [uml-join マクロ](#macro uml-join) -->

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


<!-- autolink: [uml-merge マクロ](#macro uml-merge) -->

${BLANK_PARAGRAPH}

#### macro uml-node

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-node}} position name ${KEY} pivot keyword width height font fill1 fill2 stroke link layer id contents


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-node マクロ](#macro uml-node) -->

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


<!-- autolink: [uml-note マクロ](#macro uml-note) -->

${BLANK_PARAGRAPH}

#### macro uml-package

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-package}} position name ${KEY} pivot keyword width height font fill stroke link layer id contents


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-package マクロ](#macro uml-package) -->

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


<!-- autolink: [uml-partition マクロ](#macro uml-partition) -->

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


<!-- autolink: [uml-pin マクロ](#macro uml-pin) -->

${BLANK_PARAGRAPH}

#### macro uml-realization

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-realization}} from to ${KEY} keyword name style spacing layer id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-realization マクロ](#macro uml-realization) -->

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


<!-- autolink: [uml-signal マクロ](#macro uml-signal) -->

${BLANK_PARAGRAPH}

#### macro uml-state

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-state}} position text ${KEY} pivot keyword width height activities margin corner-r font fill stroke link layer id contents


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-state マクロ](#macro uml-state) -->

${BLANK_PARAGRAPH}

#### macro uml-state-begin

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-state-begin}} position ${KEY} pivot radius fill link layer id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-state-begin マクロ](#macro uml-state-begin) -->

${BLANK_PARAGRAPH}

#### macro uml-state-end

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-state-end}} position ${KEY} pivot radius ratio fill stroke link layer id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-state-end マクロ](#macro uml-state-end) -->

${BLANK_PARAGRAPH}

#### macro uml-state-history

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-state-history}} position ${KEY} pivot radius fill stroke link layer id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-state-history マクロ](#macro uml-state-history) -->

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


<!-- autolink: [uml-time-event マクロ](#macro uml-time-event) -->

${BLANK_PARAGRAPH}

#### macro uml-transition

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-transition}} from to ${KEY} spec style spacing layer id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-transition マクロ](#macro uml-transition) -->

${BLANK_PARAGRAPH}

#### macro uml-usecase

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-usecase}} position text ${KEY} pivot keyword width height font fill stroke margin link layer id


<!-- stack:pop li -->

${DESCRIPTION}

　${{TODO}{まだ記述されていません。}}

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [uml-usecase マクロ](#macro uml-usecase) -->

${BLANK_PARAGRAPH}

#### macro use

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{use}} ref position ${KEY} pivot link rotate layer id contents debug

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `ref` ---- 再使用する定義の ID をキーワードシンボルで指定します。
* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `pivot` ---- 基準点が描画矩形のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `link` ---- リンクにする場合、リンク先を指定します。
* `rotate` ---- 全体を回転させたい場合に、その角度を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します
* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。
* `debug` ---- 補助線を描画する場合、 `t` または色名を指定します

${DESCRIPTION}

　defgroup マクロで作成した定義 `ref` を使用します。

${SEE_ALSO}

* [](#定義と再使用)
* defgroup マクロ

${NO_NOTES}


<!-- autolink: [use マクロ](#macro use) -->

${BLANK_PARAGRAPH}

#### macro with-balloon-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-balloon-options}} (${KEY} round align valign margin font fill stroke filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　balloon  マクロで描画される吹き出しのデフォルトオプションを変更します。キーワードパラメータ
群の説明は balloon マクロを参照してください。

${SEE_ALSO}

* 吹き出し
* balloon マクロ

${NO_NOTES}


<!-- autolink: [with-balloon-options マクロ](#macro with-balloon-options) -->

${BLANK_PARAGRAPH}

#### macro with-block-arrow-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-block-arrow-options}} (${KEY} length size margin fill stroke filter layer) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　block-arrow1 マクロおよび block-arrow2 マクロで描画されるブロック矢印のデフォルト
オプションを変更します。キーワードパラメータ群の説明は block-arrow1 マクロを参照して
ください。

${SEE_ALSO}

* ブロック矢印
* block-arrow1 マクロ
* block-arrow2 マクロ

${NO_NOTES}


<!-- autolink: [with-block-arrow-options マクロ](#macro with-block-arrow-options) -->

${BLANK_PARAGRAPH}

#### macro with-brace-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-brace-options}} (${KEY} font stroke filter layer) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　brace マクロで描画される波括弧のデフォルトオプションを変更します。キーワードパラメータ
群の説明は brace マクロを参照してください。

${SEE_ALSO}

* 波括弧
* brace マクロ

${NO_NOTES}


<!-- autolink: [with-brace-options マクロ](#macro with-brace-options) -->

${BLANK_PARAGRAPH}

#### macro with-canvas

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-canvas}} (sym-center sym-width sym-height) canvas ${BODY} body

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `sym-center` ---- `canvas` の center 値を参照するための変数名を指定します。
* `sym-width` ---- `canvas` の幅を参照するための変数名を指定します。
* `sym-height` ---- `canvas` の高さを参照するための変数名を指定します。
* `canvas` ---- 対象のキャンバスを指定します。
* `body` ---- 実行するコードを指定します。

${DESCRIPTION}

　`canvas` の中心、幅、および高さを変数で直接参照できるかのようなレキシカル環境を確立し、
コード `body` を実行します。コード `body` 内では、 `sym-center sym-width sym-height` 
それぞれで指定した名前の変数で値の取得が可能です。

${NO_SEE_ALSO}

${NOTES}

　with-canvas マクロは非推奨となりました。今後は with-current-canvas マクロを使用して
ください。


<!-- autolink: [with-canvas マクロ](#macro with-canvas) -->

${BLANK_PARAGRAPH}

#### macro with-cross-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-cross-options}} (${KEY} fill stroke filter layer) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　cross マクロで描画される十字のデフォルトオプションを変更します。キーワードパラメータ
群の説明は cross マクロを参照してください。

${SEE_ALSO}

* 十字
* cross マクロ

${NO_NOTES}


<!-- autolink: [with-cross-options マクロ](#macro with-cross-options) -->

${BLANK_PARAGRAPH}

#### macro with-cube-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-cube-options}} (${KEY} depth align valign margin font fill fill2 stroke filter layer) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　cube マクロで描画されるキューブのデフォルトオプションを変更します。キーワードパラメータ
群の説明は cube マクロを参照してください。

${SEE_ALSO}

* キューブ
* cube マクロ

${NO_NOTES}


<!-- autolink: [with-cube-options マクロ](#macro with-cube-options) -->

${BLANK_PARAGRAPH}

#### macro with-current-canvas

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-current-canvas}} (${REST} vars) ${BODY} body

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `vars` ---- `canvas` における各種の値を参照するための変数を指定します。
* `body` ---- 実行するコードを指定します。

${DESCRIPTION}

　現在のキャンバスの中心、幅、高さなど各種の値を直接参照できるかのようなレキシカル
環境を確立し、コード `body` を実行します。使用例は [$@ 章](#サブキャンバス)を参照して
ください。

${SEE_ALSO}

* [](#サブキャンバス)

${NO_NOTES}


<!-- autolink: [with-current-canvas マクロ](#macro with-current-canvas) -->

${BLANK_PARAGRAPH}

#### macro with-cylinder-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-cylinder-options}} (${KEY} depth align valign margin font fill stroke filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　cylinder マクロで描画される円柱のデフォルトオプションを変更します。キーワードパラメータ
群の説明は cylinder マクロを参照してください。

${SEE_ALSO}

* 円柱
* cylinder マクロ

${NO_NOTES}


<!-- autolink: [with-cylinder-options マクロ](#macro with-cylinder-options) -->

${BLANK_PARAGRAPH}

#### macro with-document-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-document-options}} (${KEY} align valign margin font fill stroke filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　document マクロで描画されるドキュメントのデフォルトオプションを変更します。キーワード
パラメータ群の説明は document マクロを参照してください。

${SEE_ALSO}

* ドキュメント
* document マクロ

${NO_NOTES}


<!-- autolink: [with-document-options マクロ](#macro with-document-options) -->

${BLANK_PARAGRAPH}

#### macro with-endmark-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-endmark-options}} (${KEY} type size fill end1 end2) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　終端マークのデフォルトオプションを変更します。キーワードパラメータ群の説明は 
[$@ 節](#終端マーク)を参照してください。

${SEE_ALSO}

* [](#終端マーク)

${NO_NOTES}


<!-- autolink: [with-endmark-options マクロ](#macro with-endmark-options) -->

${BLANK_PARAGRAPH}

#### macro with-explosion-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-explosion-options}} (${KEY} font fill stroke filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　explosion1 マクロおよび explosion2 マクロで描画される爆発のデフォルトオプションを変更
します。キーワードパラメータ群の説明は explosion1 マクロを参照してください。

${SEE_ALSO}

* 爆発
* explosion1 マクロ
* explosion2 マクロ

${NO_NOTES}


<!-- autolink: [with-explosion-options マクロ](#macro with-explosion-options) -->

${BLANK_PARAGRAPH}

#### macro with-folder-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-folder-options}} (${KEY} tab-width tab-height align valign margin font fill stroke filter layer) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　folder マクロで描画されるフォルダのデフォルトオプションを変更します。キーワードパラメータ
群の説明は folder マクロを参照してください。

${SEE_ALSO}

* フォルダ
* folder マクロ

${NO_NOTES}


<!-- autolink: [with-folder-options マクロ](#macro with-folder-options) -->

${BLANK_PARAGRAPH}

#### macro with-label-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-label-options}} (${KEY} position offset font) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　ラベルのデフォルトオプションを変更します。キーワードパラメータ群の説明は [$@ 節](#ラベル)を
参照してください。

${SEE_ALSO}

* [](#ラベル)

${NO_NOTES}


<!-- autolink: [with-label-options マクロ](#macro with-label-options) -->

${BLANK_PARAGRAPH}

#### macro with-memo-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-memo-options}} (${KEY} crease align valign margin font fill fill2 stroke filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　memo マクロで描画されるメモのデフォルトオプションを変更します。キーワードパラメータ
群の説明は memo マクロを参照してください。

${SEE_ALSO}

* メモ
* memo マクロ

${NO_NOTES}


<!-- autolink: [with-memo-options マクロ](#macro with-memo-options) -->

${BLANK_PARAGRAPH}

#### macro with-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-options}} (${KEY} fill stroke font filter layer) ${BODY} body

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 外枠を描画するストロークを指定します。
* `font` ---- フォントを指定します。
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します。

${DESCRIPTION}

　ストローク、塗り潰し、フォントなどのデフォルト設定を変更します。

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-options マクロ](#macro with-options) -->

${BLANK_PARAGRAPH}

#### macro with-person-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-person-options}} (${KEY} fill stroke layer filter) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　person マクロで描画される人物のデフォルトオプションを変更します。キーワードパラメータ
群の説明は person マクロを参照してください。

${SEE_ALSO}

* 人物
* person マクロ

${NO_NOTES}


<!-- autolink: [with-person-options マクロ](#macro with-person-options) -->

${BLANK_PARAGRAPH}

#### macro with-point

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-point}} (sym-x sym-y) pt ${BODY} body

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `sym-x` ---- `pt` の x 値を参照するための変数名を指定します。
* `sym-y` ---- `pt` の y 値を参照するための変数名を指定します。
* `pt` ---- 対象の座標値を指定します。
* `body` ---- 実行するコードを指定します。

${DESCRIPTION}

　`pt` の x および y の値を変数で直接参照できるかのようなレキシカル環境を確立し、
コード `body` を実行します。コード `body` 内では、 `sym-x sym-y` それぞれで指定した
名前の変数で値の取得と設定が可能です。

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [with-point マクロ](#macro with-point) -->

${BLANK_PARAGRAPH}

#### macro with-subcanvas

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-subcanvas}} (top-left width height ${KEY} debug) ${BODY} body

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `top-left` ---- 作成するサブキャンバスの（現在のキャンバスから見た）左上の座標を指定します。
* `width` ---- 作成するサブキャンバスの幅を指定します。
* `height` ---- 作成するサブキャンバスの高さ指定します。
* `debug` ---- `T` または色名を示すキーワードシンボルを指定するとキャンバスの領域を明示する補助線が描画されます。
* `body` ---- 作成したサブキャンバス内部に描画するコードを指定します。

${DESCRIPTION}

　任意の位置とサイズでサブキャンバスを確立し、その内部に描画を行ないます。既存の図形要素の
内部をサブキャンバスとした描画をしたい場合、with-subcanvas-of マクロを使用してください。

${SEE_ALSO}

* [](#サブキャンバス)
* with-subcanvas-of マクロ

${NO_NOTES}


<!-- autolink: [with-subcanvas マクロ](#macro with-subcanvas) -->

${BLANK_PARAGRAPH}

#### macro with-subcanvas-of

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-subcanvas-of}} (id) ${BODY} body

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `id` ---- 対象となる図形要素の ID をキーワードシンボルで指定します。
* `body` ---- 作成したサブキャンバス内部に描画するコードを指定します。

${DESCRIPTION}

　既存の図形要素の内部をサブキャンバスとして、その内部に描画を行ないます。任意の領域を
サブキャンバスとした描画をしたい場合、with-subcanvas マクロを使用してください。

${SEE_ALSO}

* [](#サブキャンバス)
* with-subcanvas マクロ

${NO_NOTES}


<!-- autolink: [with-subcanvas-of マクロ](#macro with-subcanvas-of) -->

${BLANK_PARAGRAPH}

#### macro with-table-cell

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-table-cell}} (id r c) ${BODY} body

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `id` ---- 対象となるテーブルの ID をキーワードシンボルで指定します。
* `r` ---- 対象となるテーブルセルの（０から始まる）行番号を数値で指定します。
* `c` ---- 対象となるテーブルセルの（０から始まる）列番号を数値で指定します。
* `body` ---- 対象となるテーブルセル内で行なう描画コードを記述します。

${DESCRIPTION}

　テーブルの ID とセルの行・列番号を指定して該当するセル内をサブキャンバスとした
描画を行ないます。

${SEE_ALSO}

* [](#with-table-cell を使ったセル内描画)

${NO_NOTES}


<!-- autolink: [with-table-cell マクロ](#macro with-table-cell) -->

${BLANK_PARAGRAPH}

#### macro with-table-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-table-options}} (${KEY} font fill stroke layer) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　table マクロで描画されるテーブルのデフォルトオプションを変更します。キーワードパラメータ
群の説明は table マクロを参照してください。

${SEE_ALSO}

* テーブル
* table マクロ

${NO_NOTES}


<!-- autolink: [with-table-options マクロ](#macro with-table-options) -->

${BLANK_PARAGRAPH}

#### macro with-table-range

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-table-range}} (id range) ${BODY} body

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `id` ---- 対象となるテーブルの ID をキーワードシンボルで指定します。
* `range` ---- 対象となるテーブル内の部分領域をキーワードシンボルで指定します。これは table マクロにおける `fills` パラメータの指定方法と同様です。
* `body` ---- 対象となるテーブルセル内で行なう描画コードを記述します。

${DESCRIPTION}

　テーブルの一部の領域をサブキャンバスとした描画を行ないます。

${SEE_ALSO}

* [](#with-table-range を使った範囲取得)

${NO_NOTES}


<!-- autolink: [with-table-range マクロ](#macro with-table-range) -->

${BLANK_PARAGRAPH}

#### macro with-textbox-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-textbox-options}} (${KEY} rx ry align valign margin font fill stroke filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　textbox マクロで描画されるテキストボックスのデフォルトオプションを変更します。キーワード
パラメータ群の説明は textbox マクロを参照してください。

${SEE_ALSO}

* テキストボックス
* textbox マクロ

${NO_NOTES}


<!-- autolink: [with-textbox-options マクロ](#macro with-textbox-options) -->

${BLANK_PARAGRAPH}

#### macro with-theme

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-theme}} (name) ${BODY} body

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `name` ---- 使用するテーマの ID をキーワードシンボルで指定します。
* `body` ---- `name` で指定したテーマを使用して描画を行なうコードを記述します。

${DESCRIPTION}

　テーマを指定した描画を行ないます。詳細は [$@ 章](#テーマ)を参照してください。

${SEE_ALSO}

* [](#テーマ)

${NO_NOTES}


<!-- autolink: [with-theme マクロ](#macro with-theme) -->

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


<!-- autolink: [with-uml-action-options マクロ](#macro with-uml-action-options) -->

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


<!-- autolink: [with-uml-activity-final-options マクロ](#macro with-uml-activity-final-options) -->

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


<!-- autolink: [with-uml-activity-start-options マクロ](#macro with-uml-activity-start-options) -->

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


<!-- autolink: [with-uml-connector-options マクロ](#macro with-uml-connector-options) -->

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


<!-- autolink: [with-uml-decision-merge-options マクロ](#macro with-uml-decision-merge-options) -->

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


<!-- autolink: [with-uml-expansion-region-options マクロ](#macro with-uml-expansion-region-options) -->

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


<!-- autolink: [with-uml-flow-final-options マクロ](#macro with-uml-flow-final-options) -->

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


<!-- autolink: [with-uml-flow-options マクロ](#macro with-uml-flow-options) -->

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


<!-- autolink: [with-uml-fork-join-options マクロ](#macro with-uml-fork-join-options) -->

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


<!-- autolink: [with-uml-frame-options マクロ](#macro with-uml-frame-options) -->

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


<!-- autolink: [with-uml-note-options マクロ](#macro with-uml-note-options) -->

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


<!-- autolink: [with-uml-partition-lane マクロ](#macro with-uml-partition-lane) -->

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


<!-- autolink: [with-uml-partition-options マクロ](#macro with-uml-partition-options) -->

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


<!-- autolink: [with-uml-pin-options マクロ](#macro with-uml-pin-options) -->

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


<!-- autolink: [with-uml-signal-options マクロ](#macro with-uml-signal-options) -->

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


<!-- autolink: [with-uml-time-event-options マクロ](#macro with-uml-time-event-options) -->

${BLANK_PARAGRAPH}

#### function x+

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{x+}} pt x => result

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `pt` ---- 対象の座標値を指定します。
* `x` ---- 加算する x 軸の値を指定します。
* `result` ---- 結果の座標値が返ります。

${DESCRIPTION}

　`pt` に `(x 0)` を足した座標値を返します。function point/x+ と同じことをします。

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [x+ 関数](#function x+) -->

${BLANK_PARAGRAPH}

#### function xy+

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{xy+}} pt x y => result

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `pt` ---- 対象の座標値を指定します。
* `x` ---- 加算する x 軸の値を指定します。
* `y` ---- 加算する y 軸の値を指定します。
* `result` ---- 結果の座標値が返ります。

${DESCRIPTION}

　`pt` に `(x y)` を足した座標値を返します。function xy+ と同じことをします。

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [xy+ 関数](#function xy+) -->

${BLANK_PARAGRAPH}

#### function y+

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{y+}} pt y => result

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `pt` ---- 対象の座標値を指定します。
* `y` ---- 加算する y 軸の値を指定します。
* `result` ---- 結果の座標値が返ります。

${DESCRIPTION}

　`pt` に `(0 y)` を足した座標値を返します。function point/y+ と同じことをします。

${NO_SEE_ALSO}

${NO_NOTES}


<!-- autolink: [y+ 関数](#function y+) -->

${BLANK_PARAGRAPH}

