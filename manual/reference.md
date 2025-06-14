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

　図形要素同士を接続します。スタイルを統一したい場合、with-connector-options マクロを
使うことができます。

${SEE_ALSO}

* コネクタ
* 終端マーク
* with-connector-options マクロ

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
* `x` ---- x 座標を数値で指定します。SVG の pattern タグにおける x 属性として使用されます。省略した場合のデフォルト値は 0 です。
* `y` ---- y 座標を数値で指定します。SVG の pattern タグにおける y 属性として使用されます。省略した場合のデフォルト値は 0 です。
* `width` ---- 幅を数値で指定します。
* `height` ---- 高さを数値で指定します。
* `href` ---- SVG の pattern タグにおける xlink:href 属性として使用されます。
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
* `size` ---- 終端マークのサイズを指定します。 `:small :medium :large :xlarge` のいずれか、または数値を指定します。ここで、 `:small` は `10.0` 、 `:medium` は `15.0` 、 `:large` は `20.0` 、 `:xlarge` は `30.0` にそれぞれ相当します。
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

* ${{B}{make-stroke}} ${KEY} color width opacity linecap linejoin miterlimit dasharray dashoffset url base

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

#### function make-uml-class-attribute

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{make-uml-class-attribute}} ${REST} params

<!-- stack:pop li -->

${DESCRIPTION}

　指定されたパラメータでクラス属性情報を生成します。上記は簡潔な記述で柔軟なクラス属性情報
の生成を可能にするためのもので、 `params` として渡されるパラメータ数に応じて以下のことを
します。

* パラメータ数が 1 の場合
    * `:<<STEREOTYPE>>`  形式のキーワードシンボルがの場合、ステレオタイプ情報を作成して返します
    * キーワードシンボル `:etc` の場合、文字列 `...` を表示する省略情報を作成して返します
    * 上記のいずれでもないキーワードシンボルまたは文字列 `param` の場合、 `(make-uml-class-attribute :none param)` を返します
    * クラス属性情報オブジェクト `obj` の場合、それをそのまま返します
    * リスト lst が渡された場合、 `(apply #'make-uml-class-attribute lst)` を返します
    * 上記のいずれでもない prm の場合、 `(make-uml-class-attribute :none param)` を返します
* パラメータ数が 2 以上の場合
    * 後述します

　パラメータ数が 2 以上の場合、make-uml-class-attribute 関数は実質的に以下の関数であるかのように
振舞います。

<!-- stack:push li class='syntax' -->

* ${{B}{make-uml-class-attribute}} visibility name ${KEY} type multiplicity default property scope

<!-- stack:pop li -->

　各パラメータの意味は以下の通りです。

* `visibility` ---- 可視性を `:none :public :private :protected :package :derived` のいずれかで指定します。
* `name` ---- 属性の名称を文字列またはキーワードシンボルで指定します。
* `type` ---- 型を文字列またはキーワードシンボルで指定します。
* `multiplicity` ---- 多重度を整数値または文字列で指定します。
* `default` ---- 属性のデフォルト値を数値、文字列またはキーワードシンボルで指定します。
* `property` ---- 属性のプロパティ値を文字列またはキーワードシンボルで指定します。
* `scope` ---- 属性のスコープを `:instance :class` のいずれかで指定します。

${SEE_ALSO}

* make-uml-class-operation 関数

${NOTES}

　通常、この関数を明示的に使用する必要はありません。 `uml-class` マクロの `:attributes` に
指定されたリストの要素は内部でこの関数に渡されるため、 `'(:private "m_data" :type :string)` 
などといった記述でクラス属性情報を指定できます。


<!-- autolink: [make-uml-class-attribute 関数](#function make-uml-class-attribute) -->

${BLANK_PARAGRAPH}

#### function make-uml-class-operation

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{make-uml-class-operation}} ${REST} params

<!-- stack:pop li -->

${DESCRIPTION}

　指定されたパラメータでクラス操作情報を生成します。上記は簡潔な記述で柔軟なクラス操作情報
の生成を可能にするためのもので、 `params` として渡されるパラメータ数に応じて以下のことを
します。

* パラメータ数が 1 の場合
    * `:<<STEREOTYPE>>`  形式のキーワードシンボルがの場合、ステレオタイプ情報を作成して返します
    * キーワードシンボル `:etc` の場合、文字列 `...` を表示する省略情報を作成して返します
    * 上記のいずれでもないキーワードシンボルまたは文字列 `param` の場合、 `(make-uml-class-operation :none param)` を返します
    * クラス操作情報オブジェクト `obj` の場合、それをそのまま返します
    * リスト `lst` が渡された場合、 `(apply #'make-uml-class-operation lst)` を返します
    * 上記のいずれでもない prm の場合、 `(make-uml-class-operation :none param)` を返します
* パラメータ数が 2 以上の場合
    * 後述します

　パラメータ数が 2 以上の場合、make-uml-class-operation 関数は実質的に以下の関数であるかのように
振舞います。

<!-- stack:push li class='syntax' -->

* ${{B}{make-uml-class-operation}} visibility name ${KEY} abstract parameters type property scope

<!-- stack:pop li -->

　各パラメータの意味は以下の通りです。

* `visibility` ---- 可視性を `:none :public :private :protected :package` のいずれかで指定します。
* `name` ---- 操作の名称を文字列またはキーワードシンボルで指定します。
* `abstract` ---- 抽象操作か否かを真偽値で指定します。 `:scope` が `:class` の場合に真を指定するとエラーになります。
* `parameters` ---- パラメータ情報をリストで指定します。このリストの各要素は `make-uml-class-operation-param` 関数に渡されます。詳細は [$@ 節](#function make-uml-class-operation-param) を参照してください。
* `type` ---- 型を文字列またはキーワードシンボルで指定します。
* `property` ---- 操作のプロパティ値を文字列またはキーワードシンボルで指定します。
* `scope` ---- 操作のスコープを `:instance :class` のいずれかで指定します。

${SEE_ALSO}

* make-uml-class-operation-param 関数
* make-uml-class-attribute 関数

${NOTES}

　通常、この関数を明示的に使用する必要はありません。 `uml-class` マクロの `:operations` に
指定されたリストの要素は内部でこの関数に渡されるため、 `'(:public "DoSomething" :type :integer)` 
などといった記述でクラス操作情報を指定できます。


<!-- autolink: [make-uml-class-operation 関数](#function make-uml-class-operation) -->

${BLANK_PARAGRAPH}

#### function make-uml-class-operation-param

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{make-uml-class-operation-param}} ${REST} params

<!-- stack:pop li -->

${DESCRIPTION}

　指定されたパラメータでクラス操作パラメータ情報を生成します。上記は簡潔な記述で柔軟なクラス操作パラメータ情報
の生成を可能にするためのもので、 `params` として渡されるパラメータ数に応じて以下のことを
します。

* パラメータ数が 1 の場合
    * キーワードシンボル `:etc` の場合、文字列 `...` を表示する省略情報を作成して返します
    * 上記に該当しないキーワードシンボルまたは文字列 `param` の場合、 `param` を名前とするクラス操作パラメータ情報を作成して返します
    * クラス操作パラメータ情報オブジェクト `obj` の場合、それをそのまま返します
    * リスト `lst` が渡された場合、 `(apply #'make-uml-class-operation-param lst)` を返します
    * 上記のいずれでもない `prm` の場合、 `prm` を名前とするクラス操作パラメータ情報を作成して返します
* パラメータ数が 2 以上の場合
    * 後述します

　パラメータ数が 2 以上の場合、make-uml-class-operation-param 関数は実質的に以下の関数であるかのように
振舞います。

<!-- stack:push li class='syntax' -->

* ${{B}{make-uml-class-operation-param}} name ${KEY} io type default

<!-- stack:pop li -->

　各パラメータの意味は以下の通りです。

* `name` ---- 操作パラメータの名称を文字列またはキーワードシンボルで指定します。
* `io` ---- 入出力に関する指定を `:in :out :inout` のいずれかで 指定します。省略可能です。
* `type` ---- 型を文字列またはキーワードシンボルで指定します。
* `default` ---- パラメータのデフォルト値を数値、文字列またはキーワードシンボルで指定します。

${SEE_ALSO}

* make-uml-class-operation 関数

${NO_NOTES}


<!-- autolink: [make-uml-class-operation-param 関数](#function make-uml-class-operation-param) -->

${BLANK_PARAGRAPH}

#### function make-uml-flow-spec

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{make-uml-flow-spec}} ${REST} params

<!-- stack:pop li -->

${DESCRIPTION}

　指定されたパラメータでフロー仕様情報を生成します。上記は簡潔な記述で柔軟なフロー仕様情報
の生成を可能にするためのもので、 `params` として渡されるパラメータ数に応じて以下のことを
します。

* パラメータ数が 1 の場合
    * フロー仕様情報が渡された場合、それをそのまま返します
    * リスト lst が渡された場合、 `(apply #'make-uml-flow-spec lst)` を返します
    * 上記のいずれでもない prm の場合、 `(make-uml-flow-spec :guard prm)` を返します
* パラメータ数が 2 以上の場合
    * 後述します

　パラメータ数が 2 以上の場合、make-uml-flow-spec 関数は実質的に以下の関数であるかのように
振舞います。

<!-- stack:push li class='syntax' -->

* ${{B}{make-uml-flow-spec}} ${KEY} guard action offset font

<!-- stack:pop li -->

　各パラメータの意味は以下の通りです。

* `guard` ---- ガード条件をキーワードシンボルまたは文字列で指定します。
* `action` ---- 実行アクションをキーワードシンボルまたは文字列で指定します。
* `offset` ---- 描画位置調整に使うオフセット情報を座標値で指定します。
* `font` ---- フォントを指定します。

${SEE_ALSO}

* [$$](#uml-flow-spec)

${NOTES}

　通常、この関数を明示的に使用する必要はありません。uml-flow マクロの 
`:spec` に指定されたパラメータは内部でこの関数に渡されるため、 
`:spec '(:guard :when-idle :action :"OnIdle()"` といった記述でフロー仕様情報
を指定できます。


<!-- autolink: [make-uml-flow-spec 関数](#function make-uml-flow-spec) -->

${BLANK_PARAGRAPH}

#### function make-uml-icon-setting

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{make-uml-icon-setting}} ${REST} params

<!-- stack:pop li -->

${DESCRIPTION}

　指定されたパラメータでアイコン設定情報を生成します。上記は簡潔な記述で柔軟なアイコン設定情報
の生成を可能にするためのもので、 `params` として渡されるパラメータ数に応じて以下のことを
します。

* パラメータ数が 1 の場合
    * アイコン設定情報が渡された場合、それをそのまま返します
    * 数値 N が渡された場合、 `(make-uml-icon-setting :size N)` を返します
    * キーワードシンボル kwd が渡された場合、 `(make-uml-icon-setting :pivot kwd)` を返します
    * リスト lst が渡された場合、 `(apply #'make-uml-icon-setting lst)` を返します
    * 上記のいずれでもない prm の場合、 `(make-uml-icon-setting :size prm)` を返します
* パラメータ数が 2 以上の場合
    * 後述します

　パラメータ数が 2 以上の場合、make-uml-icon-setting 関数は実質的に以下の関数であるかのように
振舞います。

<!-- stack:push li class='syntax' -->

* ${{B}{make-uml-icon-setting}} ${KEY} fill stroke size pivot offset base

<!-- stack:pop li -->

　各パラメータの意味は以下の通りです。

* `fill` ---- アイコン内部の塗り潰しを指定します。
* `stroke` ---- アイコンの線を描画するストロークを指定します。
* `size` ---- アイコンのサイズを数値で指定します。デフォルト値は 14 です。
* `pivot` ---- アイコンの描画位置を `:TL :TC :TR :CL :CC :CR :BL :BC :BR` のいずれかで指定します。デフォルト値は `:TR` です。
* `offset` ---- アイコンの描画位置調整に使うオフセット情報を座標値で指定します。デフォルト値は `pivot` によって変化します。
* `base` ---- アイコン設定情報の作成においてベースとする他のアイコン設定情報があれば指定します。

${SEE_ALSO}

* [$$](#uml-icon-setting)

${NOTES}

　通常、この関数を明示的に使用する必要はありません。各種マクロの `:icon` などに指定
されたパラメータは内部でこの関数に渡されるため、 `:icon '(:fill :cyan :stroke :blue)` 
といった記述でアイコン設定情報を指定できます。


<!-- autolink: [make-uml-icon-setting 関数](#function make-uml-icon-setting) -->

${BLANK_PARAGRAPH}

#### function make-uml-multiplicity

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{make-uml-multiplicity}} ${REST} params

<!-- stack:pop li -->

${DESCRIPTION}

　指定されたパラメータで多重度情報を生成します。上記は簡潔な記述で柔軟な多重度情報の生成を
可能にするためのもので、 `params` として渡されるパラメータ数に応じて以下のことをします。

* パラメータ数が 1 の場合
    * ドットリストを使った多重度指定（ car / cdr それぞれが `:*` または整数値） `cons` の場合、 `(make-uml-multiplicity :min (car param) :max (cdr param))` を返します。
    * 多重度情報が渡された場合、それをそのまま返します。
    * リスト lst が渡された場合、 `(apply #'make-uml-multiplicity lst)` を返します。
    * 上記のいずれでもない prm の場合、 `(make-uml-multiplicity :min prm :max prm)` を返します。
* パラメータ数が 2 以上の場合
    * 後述します。

　パラメータ数が 2 以上の場合、make-uml-multiplicity 関数は実質的に以下の関数であるかのように
振舞います。

<!-- stack:push li class='syntax' -->

* ${{B}{make-uml-multiplicity}} ${KEY} min max offset font

<!-- stack:pop li -->

　各パラメータの意味は以下の通りです。

* `min` ---- 多重度の最小値を指定します。整数または `:*` です。
* `max` ---- 多重度の最大値を指定します。整数または `:*` です。
* `offset` ---- 描画位置調整のためのオフセット情報を `(x y)` 形式で指定します。
* `font` ---- フォントを指定します。

${SEE_ALSO}

* [$$](#uml-multiplicity-info)
* [$$](#uml-aggregation)
* [$$](#uml-association)
* [$$](#uml-composition)

${NOTES}

　通常、この関数を明示的に使用する必要はありません。uml-assciation マクロの `:mult1` などに
指定されたパラメータは内部でこの関数に渡されるため、 `:mult1 1` や `:mult1 '(0 . 5)` 、あるいは 
`:mult1 '(:min 5 :max 100 :offset (-5 3))` といった記述で多重度情報を指定できます。


<!-- autolink: [make-uml-multiplicity 関数](#function make-uml-multiplicity) -->

${BLANK_PARAGRAPH}

#### function make-uml-role

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{make-uml-role}} ${REST} params

<!-- stack:pop li -->

${DESCRIPTION}

　指定されたパラメータでロール情報を生成します。上記は簡潔な記述で柔軟なロール情報の生成を
可能にするためのもので、 `params` として渡されるパラメータ数に応じて以下のことをします。

* パラメータ数が 1 の場合
    * ロール情報が渡された場合、それをそのまま返します
    * リスト lst が渡された場合、 `(apply #'make-uml-role lst)` を返します
    * 上記のいずれでもない prm の場合、 `(make-uml-role prm :offset '(0 0))` を返します
* パラメータ数が 2 以上の場合
    * 後述します

　パラメータ数が 2 以上の場合、make-uml-role 関数は実質的に以下の関数であるかのように振舞います。

<!-- stack:push li class='syntax' -->

* ${{B}{make-uml-role}} name ${KEY} offset font

<!-- stack:pop li -->

　各パラメータの意味は以下の通りです。

* `name` ---- ロールの名前をキーワードシンボルまたは文字列で指定します。
* `offset` ---- 描画位置調整のためのオフセット情報を `(x y)` 形式で指定します。
* `font` ---- フォントを指定します。

${SEE_ALSO}

* [$$](#uml-role-info)
* [$$](#uml-aggregation)
* [$$](#uml-association)
* [$$](#uml-composition)

${NOTES}

　通常、この関数を明示的に使用する必要はありません。uml-assciation マクロの `:role1` などに
指定されたパラメータは内部でこの関数に渡されるため、 `:role1 "target"` あるいは 
`:role1 '("target" :offset (-5 3))` といった記述でロール情報を指定できます。


<!-- autolink: [make-uml-role 関数](#function make-uml-role) -->

${BLANK_PARAGRAPH}

#### function make-uml-stereotype

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{make-uml-stereotype}} ${REST} params

<!-- stack:pop li -->

${DESCRIPTION}

　指定されたパラメータでステレオタイプ情報を生成します。上記は簡潔な記述で柔軟なステレオタイプ情報
の生成を可能にするためのもので、 `params` として渡されるパラメータ数に応じて以下のことを
します。

* パラメータ数が 1 の場合
    * ステレオタイプ情報が渡された場合、それをそのまま返します
    * リスト lst が渡された場合、 `(apply #'make-uml-stereotype lst)` を返します
    * 上記のいずれでもない prm の場合、 `(make-uml-stereotype prm :font nil)` を返します
* パラメータ数が 2 以上の場合
    * 後述します

　パラメータ数が 2 以上の場合、make-uml-stereotype 関数は実質的に以下の関数であるかのように
振舞います。

<!-- stack:push li class='syntax' -->

* ${{B}{make-uml-stereotype}} name ${KEY} font

<!-- stack:pop li -->

　各パラメータの意味は以下の通りです。

* `name` ---- ステレオタイプの名前をキーワードシンボルまたは文字列で指定します。
* `font` ---- フォントを指定します。

${SEE_ALSO}

* [$$](#uml-stereotype-info)

${NOTES}

　通常、この関数を明示的に使用する必要はありません。各種マクロの `:stereotype` などに指定
されたパラメータは内部でこの関数に渡されるため、 `:stereotype "concurrent"` あるいは 
`:stereotype '("concurrent" :font 9)` といった記述でステレオタイプ情報を指定できます。


<!-- autolink: [make-uml-stereotype 関数](#function make-uml-stereotype) -->

${BLANK_PARAGRAPH}

#### function make-uml-transition-spec

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{make-uml-transition-spec}} ${REST} params

<!-- stack:pop li -->

${DESCRIPTION}

　指定されたパラメータで遷移仕様情報を生成します。上記は簡潔な記述で柔軟な遷移仕様情報
の生成を可能にするためのもので、 `params` として渡されるパラメータ数に応じて以下のことを
します。

* パラメータ数が 1 の場合
    * 遷移仕様情報が渡された場合、それをそのまま返します
    * リスト lst が渡された場合、 `(apply #'make-uml-transition-spec lst)` を返します
    * 上記のいずれでもない prm の場合、 `(make-uml-transition-spec :trigger prm)` を返します
* パラメータ数が 2 以上の場合
    * 後述します

　パラメータ数が 2 以上の場合、make-uml-transition-spec 関数は実質的に以下の関数であるかのように
振舞います。

<!-- stack:push li class='syntax' -->

* ${{B}{make-uml-transition-spec}} ${KEY} trigger guard action offset font

<!-- stack:pop li -->

　各パラメータの意味は以下の通りです。

* `trigger` ---- トリガー条件をキーワードシンボルまたは文字列で指定します。
* `guard` ---- ガード条件をキーワードシンボルまたは文字列で指定します。
* `action` ---- 実行アクションをキーワードシンボルまたは文字列で指定します。
* `offset` ---- 描画位置調整に使うオフセット情報を座標値で指定します。
* `font` ---- フォントを指定します。

${SEE_ALSO}

* [$$](#uml-transition-spec)

${NOTES}

　通常、この関数を明示的に使用する必要はありません。uml-transition マクロの 
`:spec` に指定されたパラメータは内部でこの関数に渡されるため、 
`:spec '(:trigger :on-click :action :"OnClick()"` といった記述で遷移仕様情報
を指定できます。


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

#### function sandbox-start

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{sandbox-start}} file-spec

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `file-spec` ---- 保存するファイルを指定します。

${DESCRIPTION}

　`file-spec` で指定した名前のファイルを使用してサンドボックスモードを開始します。

${SEE_ALSO}

* sandbox-stop 関数

${NO_NOTES}


<!-- autolink: [sandbox-start 関数](#function sandbox-start) -->

${BLANK_PARAGRAPH}

#### function sandbox-stop

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{sandbox-stop}}

<!-- stack:pop li -->

${DESCRIPTION}

　サンドボックスモードを終了します。

${SEE_ALSO}

* sandbox-start 関数

${NO_NOTES}


<!-- autolink: [sandbox-stop 関数](#function sandbox-stop) -->

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

* ${{B}{uml-action}} position name ${KEY} stereotype keyword pivot width height margin corner-r rake font fill stroke icon link layer filter id contents

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `name` ---- アクションの名前を文字列で指定します。
* `stereotype` ---- ステレオタイプを明示する場合はステレオタイプ情報を指定します。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `keyword` ---- キーワードを明示する場合はステレオタイプ情報を指定します。 `stereotype` が指定されている場合は無視されます。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `pivot` ---- `position` で指定した基準点が図形要素のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `width` ---- 幅を自動決定せず、明示的に指定したい場合に数値で指定します。
* `height` ---- 高さを自動決定せず、明示的に指定したい場合に数値で指定します。
* `margin` ---- テキスト描画における「余白」のサイズです。省略した場合のデフォルト値は 5 です。
* `corner-r` ---- 角の丸みの半径を数値で指定します。省略した場合のデフォルト値は 6 です。
* `rake` ---- レーキアイコンを表示するか否かを真偽値で指定します。
* `font` ---- フォントを指定します。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 外枠を描画するストロークを指定します
* `icon` ---- アイコンの描画設定を変更する場合は指定します。 [$@ 節](#function make-uml-icon-setting) を参照してください。
* `link` ---- リンクにする場合、リンク先を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します
* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。 `:contents t` とすることも可能です。詳細は後述します。

${DESCRIPTION}

　UML アクティビティ図におけるアクションを描画します。スタイルを統一したい場合、
with-uml-action-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#アクティビティ図)
* [$$](#uml-action)
* with-uml-action-options マクロ

${NOTES}

* uml-action では `:contents` パラメータを指定した場合、 `name` で指定されたテキストを（中央でなく）上部に描画します。しかし、（ `:contents` パラメータでなく）with-subcanvas-of マクロを使う場合、テキストは中央に描画されてしまいます。これを避けるためには、 `:contents t` を指定してください。これはテキストを上端付近に描画させる効果だけを持ちます。


<!-- autolink: [uml-action マクロ](#macro uml-action) -->

${BLANK_PARAGRAPH}

#### macro uml-activity-final

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-activity-final}} position ${KEY} radius ratio pivot fill stroke link layer filter id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `radius` ---- 終了アクションの半径を数値で指定します。省略した場合のデフォルト値は 15 です。
* `ratio` ---- 内部の（塗り潰される）円の半径を `radius` に対する比で指定します。省略した場合のデフォルト値は 0.6 です。
* `pivot` ---- `position` で指定した基準点が図形要素のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `fill` ---- 外側の円と内側の円の間を塗り潰す色を指定します。省略した場合のデフォルト値は `:white` です。
* `stroke` ---- 外側の円を描画するストロークを指定します。内側の（塗り潰される）円の色としても使用されます。省略した場合のデフォルト値は `:black` です。
* `link` ---- リンクにする場合、リンク先を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します

${DESCRIPTION}

　UML アクティビティ図における終了アクションを描画します。スタイルを統一したい場合、
with-uml-activity-final-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#アクティビティ図)
* [$$](#uml-activity-final)
* with-uml-activity-final-options マクロ

${NO_NOTES}


<!-- autolink: [uml-activity-final マクロ](#macro uml-activity-final) -->

${BLANK_PARAGRAPH}

#### macro uml-activity-start

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-activity-start}} position ${KEY} radius pivot fill link layer filter id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `radius` ---- 開始アクションの半径を数値で指定します。省略した場合のデフォルト値は 10 です。
* `pivot` ---- `position` で指定した基準点が図形要素のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `fill` ---- 円を塗り潰す色を指定します。省略した場合のデフォルト値は `:black` です。
* `link` ---- リンクにする場合、リンク先を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します

${DESCRIPTION}

　UML アクティビティ図における開始アクションを描画します。スタイルを統一したい場合、
with-uml-activity-start-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#アクティビティ図)
* [$$](#uml-activity-start)
* with-uml-activity-start-options マクロ

${NO_NOTES}


<!-- autolink: [uml-activity-start マクロ](#macro uml-activity-start) -->

${BLANK_PARAGRAPH}

#### macro uml-actor

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-actor}} position name ${KEY} pivot size fill stroke link layer filter id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `name` ---- アクターの名前をラベル形式で指定します。
* `pivot` ---- `position` で指定した基準点が図形要素のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `size` ---- 大きさを明示的に指定したい場合に、高さを数値で指定します。幅は高さから自動的に決定されます。省略した場合のデフォルト値は 40 です。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 線を描画するストロークを指定します
* `link` ---- リンクにする場合、リンク先を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します

${DESCRIPTION}

　UML ユースケース図におけるアクターを描画します。スタイルを統一したい場合、
with-uml-actor-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#ユースケース図)
* [$$](#uml-actor)
* with-uml-actor-options マクロ

${NO_NOTES}


<!-- autolink: [uml-actor マクロ](#macro uml-actor) -->

${BLANK_PARAGRAPH}

#### macro uml-aggregation

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-aggregation}} from to ${KEY} arrow stereotype keyword name style spacing role1 role2 mult1 mult2 filter layer id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `from` ---- 接続元となる要素の ID をキーワードで指定します。point 値も指定できます。
* `to` ---- 接続先となる要素の ID をキーワードで指定します。point 値も指定できます。
* `arrow` ---- 集約関連の終端に矢印を描画する場合は `t` を指定します
* `stereotype` ---- ステレオタイプを明示する場合はステレオタイプ情報を指定します。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `keyword` ---- キーワードを明示する場合はステレオタイプ情報を指定します。 `stereotype` が指定されている場合は無視されます。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `name` ---- 集約関連に名前をつける場合、ラベル形式で指定します。
* `style` ---- 接続線の引き方を指定します。詳細は [$@ 節](#コネクタ)を参照してください。
* `spacing` ---- 2 回以上折れ曲がる接続線における「自由な線分」の位置を調整するためのパラメータです。詳細は [$@ 節](#コネクタ)を参照してください。
* `role1` ---- `from` 側にロールを明示する場合はロール情報を指定します。詳細は [$@ 節](#uml-role-info) を参照してください。
* `role2` ---- `to` 側にロールを明示する場合はロール情報を指定します。詳細は [$@ 節](#uml-role-info) を参照してください。
* `mult1` ---- `from` 側に多重度を明示する場合は多重度情報を指定します。詳細は [$@ 節](#uml-multiplicity-info) を参照してください。
* `mult2` ---- `to` 側に多重度を明示する場合は多重度情報を指定します。詳細は [$@ 節](#uml-multiplicity-info) を参照してください。
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します

${DESCRIPTION}

　UML クラス図における集約関連を描画します。スタイルを統一したい場合、
with-uml-aggregation-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#クラス図)
* [$$](#uml-aggregation)
* with-uml-aggregation-options マクロ

${NO_NOTES}


<!-- autolink: [uml-aggregation マクロ](#macro uml-aggregation) -->

${BLANK_PARAGRAPH}

#### macro uml-artifact

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-artifact}} position name ${KEY} pivot stereotype keyword width height font fill stroke margin link layer filter id contents

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `name` ---- 生成物の名前を文字列で指定します。
* `pivot` ---- `position` で指定した基準点が図形要素のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `stereotype` ---- ステレオタイプを明示する場合はステレオタイプ情報を指定します。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `keyword` ---- キーワードを明示する場合はステレオタイプ情報を指定します。 `stereotype` が指定されている場合は無視されます。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `width` ---- 幅を自動決定せず、明示的に指定したい場合に数値で指定します。
* `height` ---- 高さを自動決定せず、明示的に指定したい場合に数値で指定します。
* `font` ---- フォントを指定します。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 外枠を描画するストロークを指定します
* `icon` ---- アイコンの描画設定を変更する場合は指定します。 [$@ 節](#function make-uml-icon-setting) を参照してください。
* `margin` ---- テキストの余白を指定します。幅や高さを明示的に指定した場合は無視されます。
* `link` ---- リンクにする場合、リンク先を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します
* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。 `:contents t` とすることも可能です。詳細は後述します。

${DESCRIPTION}

　UML 配置図における生成物を描画します。スタイルを統一したい場合、
with-uml-artifact-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#配置図)
* [$$](#uml-artifact)
* with-uml-artifact-options マクロ

${NO_NOTES}


<!-- autolink: [uml-artifact マクロ](#macro uml-artifact) -->

${BLANK_PARAGRAPH}

#### macro uml-association

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{umL-association}} from to ${KEY} arrows stereotype keyword name style spacing role1 role2 mult1 mult2 filter layer id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `from` ---- 接続元となる要素の ID をキーワードで指定します。point 値も指定できます。
* `to` ---- 接続先となる要素の ID をキーワードで指定します。point 値も指定できます。
* `arrow` ---- 関連の終端に矢印を描画する場合は 1 を、両端に矢印を描画する場合は 2 を指定します。
* `stereotype` ---- ステレオタイプを明示する場合はステレオタイプ情報を指定します。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `keyword` ---- キーワードを明示する場合はステレオタイプ情報を指定します。 `stereotype` が指定されている場合は無視されます。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `name` ---- 関連に名前をつける場合、ラベル形式で指定します。
* `style` ---- 接続線の引き方を指定します。詳細は [$@ 節](#コネクタ)を参照してください。
* `spacing` ---- 2 回以上折れ曲がる接続線における「自由な線分」の位置を調整するためのパラメータです。詳細は [$@ 節](#コネクタ)を参照してください。
* `role1` ---- `from` 側にロールを明示する場合はロール情報を指定します。詳細は [$@ 節](#uml-role-info) を参照してください。
* `role2` ---- `to` 側にロールを明示する場合はロール情報を指定します。詳細は [$@ 節](#uml-role-info) を参照してください。
* `mult1` ---- `from` 側に多重度を明示する場合は多重度情報を指定します。詳細は [$@ 節](#uml-multiplicity-info) を参照してください。
* `mult2` ---- `to` 側に多重度を明示する場合は多重度情報を指定します。詳細は [$@ 節](#uml-multiplicity-info) を参照してください。
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します。
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します。

${DESCRIPTION}

　クラス図を始めとする各種の UML ダイアグラムにおける関連を描画します。スタイルを統一
したい場合、with-uml-association-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#クラス図)
* [$$](#uml-association)
* with-uml-association-options マクロ

${NO_NOTES}


<!-- autolink: [uml-association マクロ](#macro uml-association) -->

${BLANK_PARAGRAPH}

#### macro uml-class

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-class}} position name ${KEY} pivot width height stereotype keyword multiplicity abstract active template attributes operations responsibilities emptybox font fill stroke link filter layer id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `name` ---- クラスの名前を文字列で指定します。
* `pivot` ---- `position` で指定した基準点が図形要素のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `width` ---- 幅を自動決定せず、明示的に指定したい場合に数値で指定します。
* `height` ---- 高さを自動決定せず、明示的に指定したい場合に数値で指定します。
* `stereotype` ---- ステレオタイプを明示する場合はステレオタイプ情報を指定します。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `keyword` ---- キーワードを明示する場合はステレオタイプ情報を指定します。 `stereotype` が指定されている場合は無視されます。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `multiplicity` ---- 多重度を数値または文字列で指定します。
* `abstract` ---- 抽象クラスであることを明示する場合は `t` を指定します。
* `active` ---- アクティブクラスであることを明示する場合は `t` を指定します。
* `template` ---- クラステンプレートの場合、そのテンプレート引数を文字列で指定します。
* `attributes` ---- クラスの属性を明示する場合、クラス属性情報のリストを指定します。指定方法については [$@ 節](#uml-class)を参照してください。
* `operations` ---- クラスの操作を明示する場合、クラス操作情報のリストを指定します。指定方法については [$@ 節](#uml-class)を参照してください。
* `responsibilities` ---- クラスの責務を明示する場合、文字列で指定します。改行コードを含むことで複数行を記述できます。
* `emptybox` ---- 属性、操作、責務で空になるボックスを描画する場合は `t` を指定します。
* `font` ---- フォントを指定します。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 外枠を描画するストロークを指定します
* `link` ---- リンクにする場合、リンク先を指定します。
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します

${DESCRIPTION}

　UML クラス図などで使用されるクラスを描画します。スタイルを統一したい場合、
with-uml-class-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#クラス図)
* [$$](#uml-class)
* with-uml-class-options マクロ

${NO_NOTES}


<!-- autolink: [uml-class マクロ](#macro uml-class) -->

${BLANK_PARAGRAPH}

#### macro uml-component

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-component}} position name ${KEY} pivot stereotype keyword width height font fill stroke icon margin link layer filter id contents

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `name` ---- コンポーネントの名前を文字列で指定します。
* `pivot` ---- `position` で指定した基準点が図形要素のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `stereotype` ---- ステレオタイプを明示する場合はステレオタイプ情報を指定します。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `keyword` ---- キーワードを明示する場合はステレオタイプ情報を指定します。 `stereotype` が指定されている場合は無視されます。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `width` ---- 幅を自動決定せず、明示的に指定したい場合に数値で指定します。
* `height` ---- 高さを自動決定せず、明示的に指定したい場合に数値で指定します。
* `font` ---- フォントを指定します。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 外枠を描画するストロークを指定します
* `icon` ---- アイコンの描画設定を変更する場合は指定します。 [$@ 節](#function make-uml-icon-setting) を参照してください。
* `margin` ---- テキストの余白を指定します。幅や高さを明示的に指定した場合は無視されます。
* `link` ---- リンクにする場合、リンク先を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します
* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。 `:contents t` とすることも可能です。詳細は後述します。

${DESCRIPTION}

　UML コンポーネント図におけるコンポーネントを描画します。スタイルを統一したい場合、
with-uml-component-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#コンポーネント図)
* [$$](#uml-component)
* with-uml-component-options マクロ

${NO_NOTES}


<!-- autolink: [uml-component マクロ](#macro uml-component) -->

${BLANK_PARAGRAPH}

#### macro uml-composition

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-composition}} from to ${KEY} arrow stereotype keyword name style spacing role1 role2 mult1 mult2 filter layer id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `from` ---- 接続元となる要素の ID をキーワードで指定します。point 値も指定できます。
* `to` ---- 接続先となる要素の ID をキーワードで指定します。point 値も指定できます。
* `arrow` ---- コンポジションの終端に矢印を描画する場合は `t` を指定します
* `stereotype` ---- ステレオタイプを明示する場合はステレオタイプ情報を指定します。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `keyword` ---- キーワードを明示する場合はステレオタイプ情報を指定します。 `stereotype` が指定されている場合は無視されます。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `name` ---- コンポジションに名前をつける場合、ラベル形式で指定します。
* `style` ---- 接続線の引き方を指定します。詳細は [$@ 節](#コネクタ)を参照してください。
* `spacing` ---- 2 回以上折れ曲がる接続線における「自由な線分」の位置を調整するためのパラメータです。詳細は [$@ 節](#コネクタ)を参照してください。
* `role1` ---- `from` 側にロールを明示する場合はロール情報を指定します。詳細は [$@ 節](#uml-role-info) を参照してください。
* `role2` ---- `to` 側にロールを明示する場合はロール情報を指定します。詳細は [$@ 節](#uml-role-info) を参照してください。
* `mult1` ---- `from` 側に多重度を明示する場合は多重度情報を指定します。詳細は [$@ 節](#uml-multiplicity-info) を参照してください。
* `mult2` ---- `to` 側に多重度を明示する場合は多重度情報を指定します。詳細は [$@ 節](#uml-multiplicity-info) を参照してください。
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します

${DESCRIPTION}

　UML クラス図におけるコンポジション関連を描画します。スタイルを統一したい場合、
with-uml-composition-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#クラス図)
* [$$](#uml-composition)
* with-uml-composition-options マクロ

${NO_NOTES}


<!-- autolink: [uml-composition マクロ](#macro uml-composition) -->

${BLANK_PARAGRAPH}

#### macro uml-connector

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-connector}} position1 position2 id ${KEY} pivot1 pivot2 name size fill stroke font filter layer

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position1` ---- ひとつめの描画要素の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `position2` ---- ふたつめの描画要素の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `id` ---- 付与するID をキーワードシンボルで指定します。
* `pivot1` ---- `position1` で指定した基準点が図形要素のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `pivot2` ---- `position2` で指定した基準点が図形要素のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `name` ---- 描画されるコネクタ内部に表示される（１文字の）名前を `:A` などのキーワードシンボルで指定します。
* `size` ---- 描画されるコネクタのサイズを数値で指定します。省略した場合のデフォルト値は20です。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 外枠を描画するストロークを指定します
* `font` ---- フォントを指定します。
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します

${DESCRIPTION}

　UML アクティビティ図におけるコネクタを描画します。スタイルを統一したい場合、
with-uml-connector-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#アクティビティ図)
* [$$](#uml-connector)
* with-uml-connector-options マクロ

${NOTES}

* uml-connector は（フローなどのコネクタでの）接続に使用する前提のため、 `:id` パラメータは省略できません。
* `:name` に２文字以上からなるキーワードシンボルを指定した場合、最初の１文字だけが使用されます。
* `:name` を省略した場合、 `id` の「最後の１文字」が使用されます。


<!-- autolink: [uml-connector マクロ](#macro uml-connector) -->

${BLANK_PARAGRAPH}

#### macro uml-decision

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-decision}} position ${KEY} pivot text width height margin font fill stroke link layer filter id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `pivot` ---- `position` で指定した基準点が図形要素のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `text` ---- 分岐に関する情報をテキストで指定します。
* `width` ---- 幅を自動決定せず、明示的に指定したい場合に数値で指定します。
* `height` ---- 高さを自動決定せず、明示的に指定したい場合に数値で指定します。
* `margin` ---- テキスト描画における「余白」のサイズです。省略した場合のデフォルト値は 5 です。
* `font` ---- フォントを指定します。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 外枠を描画するストロークを指定します
* `link` ---- リンクにする場合、リンク先を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します

${DESCRIPTION}

　UML アクティビティ図におけるデジジョン（条件による分岐）を描画します。スタイルを
統一したい場合、with-uml-decision-merge-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#アクティビティ図)
* [$$](#uml-decision)
* [$$](#uml-merge)
* with-uml-decision-merge-options マクロ

${NO_NOTES}


<!-- autolink: [uml-decision マクロ](#macro uml-decision) -->

${BLANK_PARAGRAPH}

#### macro uml-dependency

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-dependency}} from to ${KEY} stereotype keyword name style spacing filter layer id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `from` ---- 接続元となる要素の ID をキーワードで指定します。point 値も指定できます。
* `to` ---- 接続先となる要素の ID をキーワードで指定します。point 値も指定できます。
* `stereotype` ---- ステレオタイプを明示する場合はステレオタイプ情報を指定します。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `keyword` ---- キーワードを明示する場合はステレオタイプ情報を指定します。 `stereotype` が指定されている場合は無視されます。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `name` ---- 依存関係に名前をつける場合、ラベル形式で指定します。
* `style` ---- 接続線の引き方を指定します。詳細は [$@ 節](#コネクタ)を参照してください。
* `spacing` ---- 2 回以上折れ曲がる接続線における「自由な線分」の位置を調整するためのパラメータです。詳細は [$@ 節](#コネクタ)を参照してください。
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します。
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します。

${DESCRIPTION}

　クラス図を始めとする各種の UML ダイアグラムにおける依存関係を描画します。スタイルを統一
したい場合、with-uml-dependency-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#クラス図)
* [$$](#uml-dependency)
* with-uml-dependency-options マクロ

${NO_NOTES}


<!-- autolink: [uml-dependency マクロ](#macro uml-dependency) -->

${BLANK_PARAGRAPH}

#### macro uml-expansion-region

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-expansion-region}} position width height ${KEY} pivot stereotype keyword offset corner-r fill stroke link layer filter id contents

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `width` ---- 幅を数値で指定します。
* `height` ---- 高さを数値で指定します。
* `pivot` ---- `position` で指定した基準点が図形要素のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `stereotype` ---- ステレオタイプを明示する場合はステレオタイプ情報を指定します。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `keyword` ---- キーワードを明示する場合はステレオタイプ情報を指定します。 `stereotype` が指定されている場合は無視されます。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `offset` ---- キーワードの表示位置を調整するためのオフセットを座標値で指定します。
* `corner-r` ---- 四隅の角の丸みの大きさを数値で指定します。デフォルト値は 10 です。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 外枠を描画するストロークを指定します
* `link` ---- リンクにする場合、リンク先を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します
* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。

${DESCRIPTION}

　UML アクティビティ図における拡張領域を描画します。スタイルを統一したい場合、
with-uml-expansion-region-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#アクティビティ図)
* [$$](#uml-expansion-region)
* with-uml-expansion-region-options マクロ

${NOTES}

　キーワードはデフォルト値で拡張領域の左上に表示されます。この位置を調整したい場合、
`:offset '(20 3)` といった要領で移動させたいオフセット値を指定してください。 


<!-- autolink: [uml-expansion-region マクロ](#macro uml-expansion-region) -->

${BLANK_PARAGRAPH}

#### macro uml-flow

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-flow}} from to ${KEY} stereotype keyword spec style spacing filter layer id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `from` ---- 接続元となる要素の ID をキーワードで指定します。point 値も指定できます。
* `to` ---- 接続先となる要素の ID をキーワードで指定します。point 値も指定できます。
* `stereotype` ---- ステレオタイプを明示する場合はステレオタイプ情報を指定します。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `keyword` ---- キーワードを明示する場合はステレオタイプ情報を指定します。 `stereotype` が指定されている場合は無視されます。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `spec` ---- フローにガード条件やアクションを明示する場合に指定します。詳細は後述します。
* `style` ---- 接続線の引き方を指定します。詳細は [$@ 節](#コネクタ)を参照してください。
* `spacing` ---- 2 回以上折れ曲がる接続線における「自由な線分」の位置を調整するためのパラメータです。詳細は [$@ 節](#コネクタ)を参照してください。
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します。
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します。

${DESCRIPTION}

　UML アクティビティ図におけるフローを描画します。スタイルを統一したい場合、
with-uml-flow-options マクロを使うことができます。

　`spec` パラメータに指定するフロー仕様は、 `:spec '(:guard "v = 666" :action "foo()")` と
いった要領で指定します。リスト内に名前付きパラメータを並べる形式で、以下のパラメータが指定
できます。

<!-- stack:push li class='syntax' -->

* ${KEY} guard action offset font

<!-- stack:pop li -->

　`:guard :action` はそれぞれガード条件とアクションで、それぞれ文字列で指定します。これに
よって、先ほどの例で言えば `[v = 666]/foo()` といったフロー仕様が表示されることになります。 
`:offset` は表示位置の調整を `(x y)` 形式で指定します。 `:font` はフォントの指定が必要な
場合に使用します。フォントの指定を省略した場合、with-uml-flow-options マクロで指定されて
いるフォントが使用されます。

${SEE_ALSO}

* [$$](#アクティビティ図)
* [$$](#uml-flow)
* with-uml-flow-options マクロ

${NO_NOTES}


<!-- autolink: [uml-flow マクロ](#macro uml-flow) -->

${BLANK_PARAGRAPH}

#### macro uml-flow-final

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-flow-final}} position ${KEY} pivot radius fill stroke link layer filter id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `pivot` ---- `position` で指定した基準点が図形要素のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `radius` ---- 大きさを半径で指定します。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 線を描画するストロークを指定します
* `link` ---- リンクにする場合、リンク先を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します

${DESCRIPTION}

　UML アクティビティ図におけるフロー終了を描画します。スタイルを統一したい場合、
with-uml-flow-final-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#アクティビティ図)
* [$$](#uml-flow-final)
* with-uml-flow-final-options マクロ

${NO_NOTES}


<!-- autolink: [uml-flow-final マクロ](#macro uml-flow-final) -->

${BLANK_PARAGRAPH}

#### macro uml-fork

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-fork}} position direction ${KEY} pivot width length fill link layer filter id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `direction` ---- 向きを `:h` か `:v` で指定します。横方向に流れるフローで使用する場合は `:h` 、縦方向なら `:v` です。
* `pivot` ---- `position` で指定した基準点が図形要素のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `width` ---- 幅を数値で指定します。省略した場合のデフォルト値は 10 です。
* `length` ---- 長さを数値で指定します。省略した場合のデフォルト値は 40 です。
* `fill` ---- 内部の塗り潰しを指定します。
* `link` ---- リンクにする場合、リンク先を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します

${DESCRIPTION}

　UML アクティビティ図におけるフォークを描画します。スタイルを統一したい場合、
with-uml-fork-join-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#アクティビティ図)
* [$$](#uml-fork)
* [$$](#uml-join)
* with-uml-fork-join-options マクロ

${NO_NOTES}


<!-- autolink: [uml-fork マクロ](#macro uml-fork) -->

${BLANK_PARAGRAPH}

#### macro uml-frame

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-frame}} position width height title ${KEY} pivot margin fragments font fill stroke link layer filter id contents

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `width` ---- 幅を数値で指定します。
* `height` ---- 高さを数値で指定します。
* `title` ---- 名前をキーワードまたは文字列で指定します。内部的にパラグラフとして処理されるため、改行を含む複数行の文字列が使用できます。
* `pivot` ---- `position` で指定した基準点が図形要素のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `margin` ---- フレームの左上に表示される名前部分の余白の大きさを数値で指定します。
* `fragments` ---- フレーム内部を点線で複数の領域に区切る場合、各領域の高さをリストで与えます。区切られた個々の領域には with-uml-frame-fragment マクロを使ってアクセスできます。
* `font` ---- フォントを指定します。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 外枠を描画するストロークを指定します
* `link` ---- リンクにする場合、リンク先を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します
* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。

${DESCRIPTION}

　各種の UML ダイアグラムで使用されるフレームを描画します。スタイルを統一
したい場合、with-uml-frame-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#uml-frame)
* with-uml-frame-fragment マクロ
* with-uml-frame-options マクロ

${NO_NOTES}


<!-- autolink: [uml-frame マクロ](#macro uml-frame) -->

${BLANK_PARAGRAPH}

#### macro uml-generalization

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-generalization}} from to ${KEY} stereotype keyword name style spacing filter layer id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `from` ---- 接続元となる要素の ID をキーワードで指定します。point 値も指定できます。
* `to` ---- 接続先となる要素の ID をキーワードで指定します。point 値も指定できます。
* `stereotype` ---- ステレオタイプを明示する場合はステレオタイプ情報を指定します。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `keyword` ---- キーワードを明示する場合はステレオタイプ情報を指定します。 `stereotype` が指定されている場合は無視されます。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `name` ---- 汎化関係に名前をつける場合、ラベル形式で指定します。
* `style` ---- 接続線の引き方を指定します。詳細は [$@ 節](#コネクタ)を参照してください。
* `spacing` ---- 2 回以上折れ曲がる接続線における「自由な線分」の位置を調整するためのパラメータです。詳細は [$@ 節](#コネクタ)を参照してください。
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します。
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します。

${DESCRIPTION}

　クラス図を始めとする各種の UML ダイアグラムにおける汎化関係を描画します。スタイルを統一
したい場合、with-uml-generalization-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#クラス図)
* [$$](#uml-generalization)
* with-uml-generalization-options マクロ

${NO_NOTES}


<!-- autolink: [uml-generalization マクロ](#macro uml-generalization) -->

${BLANK_PARAGRAPH}

#### macro uml-interface

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-interface}} position name ${KEY} pivot fill stroke link filter layer id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `name` ---- インターフェース名をラベル形式で指定します。明示する必要が無い場合、 `nil` を指定してください。
* `pivot` ---- `position` で指定した基準点が図形要素のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 外枠を描画するストロークを指定します
* `link` ---- リンクにする場合、リンク先を指定します。
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します

${DESCRIPTION}

　UML クラス図などで使用されるインターフェースを描画します。スタイルを統一したい場合、
with-uml-interface-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#クラス図)
* [$$](#uml-interface)
* [$$](#uml-interface-request)
* with-uml-interface-options マクロ

${NO_NOTES}


<!-- autolink: [uml-interface マクロ](#macro uml-interface) -->

${BLANK_PARAGRAPH}

#### macro uml-interface-request

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-interface-request}} from to ${KEY} arrow-p name style spacing stroke filter layer id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `from` ---- インターフェースを要求する側の図形要素の ID をキーワードで指定します。任意の座標を指定することも可能です。
* `to` ---- 要求されるインターフェースの ID をキーワードで指定します。任意の座標を指定することも可能です。
* `arrow-p` ---- 描画されるソケットから対象のインターフェースに依存関係の矢印を描画するか否かを真偽値で指定します。省略した場合のデフォルト値は nil です。
* `name` ---- 要求するインターフェースの名前をラベル形式で指定します。インターフェース側で明示してある場合は指定する必要はありません。
* `style` ---- 接続線のスタイルを指定します。詳細は [$@ 節](#コネクタ)を参照してください。
* `spacing` ---- 接続線が折れ曲がる場合の調整を指定します。詳細は [$@ 節](#コネクタ)を参照してください。
* `stroke` ---- 線を描画するストロークを指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します

${DESCRIPTION}

　UML クラス図などで使用されるインターフェース要求を描画します。 `to` には uml-interface で
描画されたインターフェースの ID を指定することが想定されています。スタイルを統一したい場合、
with-uml-interface-request-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#クラス図)
* [$$](#uml-interface)
* [$$](#uml-interface-request)
* with-uml-interface-request-options マクロ

${NOTES}

　このマクロは２種類の使い方が想定されています。ひとつは uml-interface によるインターフェース
アイコンに接続する方法で、この場合は `to` パラメータにインターフェースの ID をキーワードで
指定します。この場合、名前はインターフェース側で明示してあることが想定されるため、 `name` 
パラメータは無視されます。

　もうひとつはソケットを単独で表記する方法で、この場合は `to` パラメータにソケットを描画する
位置を座標値で指定します。この場合、依存関係の矢印を使う意味は無いため、 `arrow-p` は無視され
ます。


<!-- autolink: [uml-interface-request マクロ](#macro uml-interface-request) -->

${BLANK_PARAGRAPH}

#### macro uml-join

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-join}} position direction ${KEY} pivot spec width length fill link filter layer id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `direction` ---- 向きを `:h` か `:v` で指定します。横方向に流れるフローで使用する場合は `:h` 、縦方向なら `:v` です。
* `pivot` ---- `position` で指定した基準点が図形要素のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `spec` ---- ジョイン仕様の指定をする場合、そのテキストをラベル形式で指定します。指定方法は [$@ 節](#ラベル)を参照してください。
* `width` ---- 幅を数値で指定します。省略した場合のデフォルト値は 10 です。
* `length` ---- 長さを数値で指定します。省略した場合のデフォルト値は 40 です。
* `fill` ---- 内部の塗り潰しを指定します。
* `link` ---- リンクにする場合、リンク先を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します

${DESCRIPTION}

　UML アクティビティ図におけるジョインを描画します。スタイルを統一したい場合、
with-uml-fork-join-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#アクティビティ図)
* [$$](#uml-fork)
* [$$](#uml-join)
* with-uml-fork-join-options マクロ

${NO_NOTES}


<!-- autolink: [uml-join マクロ](#macro uml-join) -->

${BLANK_PARAGRAPH}

#### macro uml-merge

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-merge}} position ${KEY} pivot width height font fill stroke link layer filter id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `pivot` ---- `position` で指定した基準点が図形要素のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `width` ---- 幅を自動決定せず、明示的に指定したい場合に数値で指定します。
* `height` ---- 高さを自動決定せず、明示的に指定したい場合に数値で指定します。
* `font` ---- フォントを指定します。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 外枠を描画するストロークを指定します
* `link` ---- リンクにする場合、リンク先を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します

${DESCRIPTION}

　UML アクティビティ図におけるマージを描画します。スタイルを統一したい場合、
with-uml-decision-merge-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#アクティビティ図)
* [$$](#uml-decision)
* [$$](#uml-merge)
* with-uml-decision-merge-options マクロ

${NO_NOTES}


<!-- autolink: [uml-merge マクロ](#macro uml-merge) -->

${BLANK_PARAGRAPH}

#### macro uml-node

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-node}} position name ${KEY} pivot stereotype keyword width height depth margin font fill1 fill2 stroke link layer id contents

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `name` ---- ノードの名前を文字列で指定します。
* `pivot` ---- `position` で指定した基準点が図形要素のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `stereotype` ---- ステレオタイプを明示する場合はステレオタイプ情報を指定します。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `keyword` ---- キーワードを明示する場合はステレオタイプ情報を指定します。 `stereotype` が指定されている場合は無視されます。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `width` ---- 幅を自動決定せず、明示的に指定したい場合に数値で指定します。
* `height` ---- 高さを自動決定せず、明示的に指定したい場合に数値で指定します。
* `depth` ---- 上面および側面の大きさを明示的に指定したい場合に数値で指定します。
* `margin` ---- テキスト描画における「余白」のサイズです。省略した場合のデフォルト値は 5 です。
* `font` ---- フォントを指定します。
* `fill1` ---- 前面内部の塗り潰しを指定します。
* `fill2` ---- 側面・上面部分の内部の塗り潰しを指定します。
* `stroke` ---- 外枠を描画するストロークを指定します
* `link` ---- リンクにする場合、リンク先を指定します。
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します
* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。 `:contents t` とすることも可能です。詳細は後述します。

${DESCRIPTION}

　UML 配置図などにおけるノードを描画します。スタイルを統一したい場合、
with-uml-node-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#配置図)
* uml-node マクロ
* with-uml-node-options マクロ

${NOTES}

* uml-node では `:contents` パラメータを指定した場合、 `name` で指定されたテキストを（中央でなく）上部に描画します。しかし、（ `:contents` パラメータでなく）with-subcanvas-of マクロを使う場合、テキストは中央に描画されてしまいます。これを避けるためには、 `:contents t` を指定してください。これはテキストを上端付近に描画させる効果だけを持ちます。


<!-- autolink: [uml-node マクロ](#macro uml-node) -->

${BLANK_PARAGRAPH}

#### macro uml-note

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-note}} position width height text ${KEY} pivot stereotype keyword targets margin crease font fill stroke link layer filter id contents

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `width` ---- 幅を数値で指定します。
* `height` ---- 高さを数値で指定します。
* `text` ---- 内部に配置するテキストを指定します。改行を含む複数行の文字列が使用できます。
* `pivot` ---- `position` で指定した基準点が図形要素のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `stereotype` ---- ステレオタイプを明示する場合はステレオタイプ情報を指定します。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `keyword` ---- キーワードを明示する場合はステレオタイプ情報を指定します。 `stereotype` が指定されている場合は無視されます。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `targets` ---- 点線で接続する対象を指定します。対象はその ID をキーワードシンボルで指定するか、接続点の具体的な座標値です。対象が単一の場合はそのまま指定可能しますが、複数の対象に（複数の点線で）接続する場合、リストにして指定してください。
* `margin` ---- 余白の大きさを指定します。省略した場合のデフォルト値は10です。
* `crease` ---- 右上部分の折り返しの大きさを変更したい場合、その大きさを数値で指定します。省略した場合のデフォルト値は20です。
* `font` ---- フォントを指定します。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 外枠を描画するストロークを指定します
* `link` ---- リンクにする場合、リンク先を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します
* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。

${DESCRIPTION}

　各種の UML ダイアグラムで使用されるノートを描画します。スタイルを統一
したい場合、with-uml-note-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#uml-note)
* with-uml-note-options マクロ

${NOTES}

　ノートから `targets` に引かれる点線は、 `stroke` で指定したストロークの `dasharray` を
差し替えたものが使用されます。これはデフォルトで `'(3 3)` ですが、uml-note マクロではこれを
個別に指定することはできません。 `dasharray` を変更したい場合、with-uml-note-options マクロ
を使用してください。


<!-- autolink: [uml-note マクロ](#macro uml-note) -->

${BLANK_PARAGRAPH}

#### macro uml-package

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-package}} position name ${KEY} pivot stereotype keyword width height font fill stroke margin link layer filter id contents

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `name` ---- アクションの名前を文字列で指定します。
* `pivot` ---- `position` で指定した基準点が図形要素のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `stereotype` ---- ステレオタイプを明示する場合はステレオタイプ情報を指定します。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `keyword` ---- キーワードを明示する場合はステレオタイプ情報を指定します。 `stereotype` が指定されている場合は無視されます。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `width` ---- 幅を自動決定せず、明示的に指定したい場合に数値で指定します。
* `height` ---- 高さを自動決定せず、明示的に指定したい場合に数値で指定します。
* `font` ---- フォントを指定します。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 外枠を描画するストロークを指定します
* `margin` ---- テキストの余白を指定します。幅や高さを明示的に指定した場合は無視されます。
* `link` ---- リンクにする場合、リンク先を指定します。
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します
* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。 `:contents t` とすることも可能です。詳細は後述します。

${DESCRIPTION}

　UML パッケージなどにおけるパッケージを描画します。スタイルを統一したい場合、
with-uml-package-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#パッケージ図)
* uml-package マクロ
* with-uml-package-options マクロ

${NOTES}

* uml-package では `:contents` パラメータを指定した場合、 `name` で指定されたテキストを（中央でなく）上部に描画します。しかし、（ `:contents` パラメータでなく）with-subcanvas-of マクロを使う場合、テキストは中央に描画されてしまいます。これを避けるためには、 `:contents t` を指定してください。これはテキストを上端付近に描画させる効果だけを持ちます。


<!-- autolink: [uml-package マクロ](#macro uml-package) -->

${BLANK_PARAGRAPH}

#### macro uml-partition

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-partition}} position rows cols ${KEY} pivot lines header fills stroke font layer id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `rows` ---- 各レーンの名前と高さの情報をリストで与えるか、または全体の高さを数値で与えます。詳細は [$@ 節](#uml-partition)を参照してください。
* `cols` ---- 各レーンの名前と幅の情報をリストで与えるか、または全体の幅を数値で与えます。詳細は [$@ 節](#uml-partition)を参照してください。
* `pivot` ---- `position` で指定した基準点が図形要素のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `lines` ---- 枠線の描画の仕方を `:min :mid :max` のいずれかで指定します。それぞれのサンプルは [$@](F#uml-partition における lines パラメータのサンプル) を参照してください。
* `header` ---- ヘッダ部分の幅または高さを数値で指定します。省略した場合のデフォルト値は 30 です。
* `fills` ---- パーティションをテーブルとして見た場合の、各セルの背景色を table マクロの `fills` パラメータの要領で指定します。詳細は [$@ 節](#macro table)を参照してください。
* `stroke` ---- 外枠を描画するストロークを指定します
* `font` ---- フォントを指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します

${DESCRIPTION}

　UML アクティビティ図におけるパーティションを描画します。各レーン内部の描画には
with-uml-partition-lane マクロを使用してください。また、スタイルを統一したい場合、
with-uml-action-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#アクティビティ図)
* [$$](#uml-partition)
* with-uml-partition-lane マクロ
* with-uml-partition-options マクロ

${NO_NOTES}


<!-- autolink: [uml-partition マクロ](#macro uml-partition) -->

${BLANK_PARAGRAPH}

#### macro uml-pin

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-pin}} target position name ${KEY} offset multi size fill stroke font filter layer id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `target` ---- ピンを追加する対象の ID をキーワードシンボルで指定します。
* `position` ---- 対象にポートを追加する位置をキーワードシンボルで指定します。詳細は後述します。
* `name` ---- ピンの名前を文字列またはキーワードシンボルで指定します。
* `offset` ---- 名前の描画位置を調整するためのオフセット値を `(x y)` 形式で指定します。
* `multi` ---- 拡張領域などに渡すマルチピンを使用する場合は `t` を指定します。
* `size` ---- 描画されるポートのサイズを指定します。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 外枠を描画するストロークを指定します
* `font` ---- フォントを指定します。
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します

${DESCRIPTION}

　UML アクティビティ図におけるピンを描画します。スタイルを統一したい場合、
with-uml-pin-options マクロを使うことができます。

　`position` はコネクタにおける接続位置の指定と同じ要領で `:L` などと指定します
（[$@ 節](#コネクタ)参照）。uml-pin においてはさらに `'(:L1 5)` などとして微調整が
可能です。

${SEE_ALSO}

* [$$](#アクティビティ図)
* [$$](#uml-pin)
* with-uml-pin-options マクロ

${NO_NOTES}


<!-- autolink: [uml-pin マクロ](#macro uml-pin) -->

${BLANK_PARAGRAPH}

#### macro uml-port

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-port}} position ${KEY} name size fill stroke layer filter id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `name` ---- ポート名を文字列またはキーワードシンボルで指定します。
* `size` ---- 描画されるポートのサイズを指定します。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 外枠を描画するストロークを指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します

${DESCRIPTION}

　UML コンポーネント図などにおけるポートを描画します。スタイルを統一したい場合、
with-uml-port-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#コンポーネント図)
* [$$](#uml-port)
* with-uml-port-options マクロ

${NO_NOTES}


<!-- autolink: [uml-port マクロ](#macro uml-port) -->

${BLANK_PARAGRAPH}

#### macro uml-realization

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-realization}} from to ${KEY} stereotype keyword name style spacing filter layer id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `from` ---- 接続元となる要素の ID をキーワードで指定します。point 値も指定できます。
* `to` ---- 接続先となる要素の ID をキーワードで指定します。point 値も指定できます。
* `stereotype` ---- ステレオタイプを明示する場合はステレオタイプ情報を指定します。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `keyword` ---- キーワードを明示する場合はステレオタイプ情報を指定します。 `stereotype` が指定されている場合は無視されます。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `name` ---- 実現関係に名前をつける場合、ラベル形式で指定します。
* `style` ---- 接続線の引き方を指定します。詳細は [$@ 節](#コネクタ)を参照してください。
* `spacing` ---- 2 回以上折れ曲がる接続線における「自由な線分」の位置を調整するためのパラメータです。詳細は [$@ 節](#コネクタ)を参照してください。
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します。
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します。

${DESCRIPTION}

　クラス図を始めとする各種の UML ダイアグラムにおける実現関係を描画します。スタイルを統一
したい場合、with-uml-realization-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#クラス図)
* [$$](#uml-realization)
* with-uml-realization-options マクロ

${NO_NOTES}


<!-- autolink: [uml-realization マクロ](#macro uml-realization) -->

${BLANK_PARAGRAPH}

#### macro uml-signal

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-signal}} position type text ${KEY} pivot stereotype keyword width height direction depth font fill stroke margin link filter layer id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `type` ---- シグナルの送信か受信かに応じて `:send` または `:receive` のいずれかを指定します。
* `text` ---- 送信／受信アクションの名前を文字列で指定します。
* `pivot` ---- `position` で指定した基準点が図形要素のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `stereotype` ---- ステレオタイプを明示する場合はステレオタイプ情報を指定します。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `keyword` ---- キーワードを明示する場合はステレオタイプ情報を指定します。 `stereotype` が指定されている場合は無視されます。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `width` ---- 幅を自動決定せず、明示的に指定したい場合に数値で指定します。
* `height` ---- 高さを自動決定せず、明示的に指定したい場合に数値で指定します。
* `direction` ---- 送信／受信を示す ＜ や Σ を描画する方向を `:left` または `:right` で指定します。
* `depth` ---- 送信／受信を示す ＜ や Σ の大きさを数値で指定します。デフォルト値は 15 です。
* `font` ---- フォントを指定します。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 外枠を描画するストロークを指定します
* `margin` ---- テキストの余白を指定します。幅や高さを明示的に指定した場合は無視されます。
* `link` ---- リンクにする場合、リンク先を指定します。
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します

${DESCRIPTION}

　UML アクティビティ図におけるシグナルの送信／受信アクションを描画します。スタイルを統一
したい場合、with-uml-signal-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#アクティビティ図)
* [$$](#uml-signal)
* with-uml-signal-options マクロ

${NO_NOTES}


<!-- autolink: [uml-signal マクロ](#macro uml-signal) -->

${BLANK_PARAGRAPH}

#### macro uml-state

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-state}} position text ${KEY} pivot stereotype keyword width height activities margin corner-r font fill stroke link layer filter id contents

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `text` ---- 状態名を文字列で指定します。
* `pivot` ---- `position` で指定した基準点が図形要素のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `stereotype` ---- ステレオタイプを明示する場合はステレオタイプ情報を指定します。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `keyword` ---- キーワードを明示する場合はステレオタイプ情報を指定します。 `stereotype` が指定されている場合は無視されます。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `width` ---- 幅を自動決定せず、明示的に指定したい場合に数値で指定します。
* `height` ---- 高さを自動決定せず、明示的に指定したい場合に数値で指定します。
* `activities` ---- 状態にアクティビティの一覧を含める場合、遷移指定のリストを記述します。このリストの個々の要素は、uml-transition における `:spec` パラメータに指定するものと同じ形式です。[$@ 節](#macro uml-transition) を参照してください。
* `margin` ---- テキスト描画における「余白」のサイズです。省略した場合のデフォルト値は 5 です。
* `corner-r` ---- 角の丸みの半径を数値で指定します。省略した場合のデフォルト値は 6 です。
* `font` ---- フォントを指定します。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 外枠を描画するストロークを指定します
* `link` ---- リンクにする場合、リンク先を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します
* `contents` ---- 内部をサブキャンバスとした描画をしたい場合、その内容を指定します。 `:contents t` とすることも可能です。詳細は後述します。

${DESCRIPTION}

　UML 状態マシン図における状態を描画します。スタイルを統一したい場合、
with-uml-state-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#状態マシン図)
* [$$](#uml-state)
* with-uml-state-options マクロ

${NOTES}

* uml-state では `:contents` パラメータを指定した場合、 `tesxt` で指定されたテキストを（中央でなく）上部に描画します。しかし、（ `:contents` パラメータでなく）with-subcanvas-of マクロを使う場合、テキストは中央に描画されてしまいます。これを避けるためには、 `:contents t` を指定してください。これはテキストを上端付近に描画させる効果だけを持ちます。


<!-- autolink: [uml-state マクロ](#macro uml-state) -->

${BLANK_PARAGRAPH}

#### macro uml-state-begin

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-state-begin}} position ${KEY} radius pivot fill link layer filter id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `radius` ---- 開始状態の半径を数値で指定します。省略した場合のデフォルト値は 10 です。
* `pivot` ---- `position` で指定した基準点が図形要素のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `fill` ---- 円を塗り潰す色を指定します。省略した場合のデフォルト値は `:black` です。
* `link` ---- リンクにする場合、リンク先を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します

${DESCRIPTION}

　UML 状態マシン図における開始状態を描画します。スタイルを統一したい場合、
with-uml-state-begin-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#状態マシン図)
* [$$](#uml-state-begin)
* with-uml-state-begin-options マクロ

${NO_NOTES}


<!-- autolink: [uml-state-begin マクロ](#macro uml-state-begin) -->

${BLANK_PARAGRAPH}

#### macro uml-state-end

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-state-end}} position ${KEY} pivot radius ratio fill stroke link layer filter id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `pivot` ---- `position` で指定した基準点が図形要素のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `radius` ---- 終了状態の半径を数値で指定します。省略した場合のデフォルト値は 15 です。
* `ratio` ---- 内部の（塗り潰される）円の半径を `radius` に対する比で指定します。省略した場合のデフォルト値は 0.6 です。
* `fill` ---- 外側の円と内側の円の間を塗り潰す色を指定します。省略した場合のデフォルト値は `:white` です。
* `stroke` ---- 外側の円を描画するストロークを指定します。内側の（塗り潰される）円の色としても使用されます。省略した場合のデフォルト値は `:black` です。
* `link` ---- リンクにする場合、リンク先を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します

${DESCRIPTION}

　UML 状態マシン図における終了状態を描画します。スタイルを統一したい場合、
with-uml-state-end-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#状態マシン図)
* [$$](#uml-state-end)
* with-uml-state-end-options マクロ

${NO_NOTES}


<!-- autolink: [uml-state-end マクロ](#macro uml-state-end) -->

${BLANK_PARAGRAPH}

#### macro uml-state-history

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-state-history}} position ${KEY} pivot radius fill stroke link layer filter id

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `pivot` ---- `position` で指定した基準点が図形要素のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `radius` ---- 半径を数値で指定します。省略した場合のデフォルト値は 15 です。
* `fill` ---- 内側の塗り潰しを指定します。省略した場合のデフォルト値は `:white` です。
* `stroke` ---- 線を描画するストロークを指定します。省略した場合のデフォルト値は `:black` です。
* `link` ---- リンクにする場合、リンク先を指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します

<!-- stack:pop li -->

${DESCRIPTION}

　UML 状態マシン図におけるヒストリアイコンを描画します。スタイルを統一したい場合、
with-uml-state-history-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#状態マシン図)
* [$$](#uml-state-history)
* with-uml-state-history-options マクロ

${NO_NOTES}


<!-- autolink: [uml-state-history マクロ](#macro uml-state-history) -->

${BLANK_PARAGRAPH}

#### macro uml-time-event

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-time-event}} position ${KEY} label pivot width height fill stroke link filter layer id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `label` ---- イベント名をラベル形式で指定します。
* `pivot` ---- `position` で指定した基準点が図形要素のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `width` ---- 幅を自動決定せず、明示的に指定したい場合に数値で指定します。
* `height` ---- 高さを自動決定せず、明示的に指定したい場合に数値で指定します。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 外枠を描画するストロークを指定します
* `link` ---- リンクにする場合、リンク先を指定します。
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します

${DESCRIPTION}

　UML アクティビティ図におけるタイムイベントを描画します。スタイルを統一
したい場合、with-uml-time-event-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#アクティビティ図)
* [$$](#uml-time-event)
* with-uml-time-event-options マクロ

${NO_NOTES}


<!-- autolink: [uml-time-event マクロ](#macro uml-time-event) -->

${BLANK_PARAGRAPH}

#### macro uml-transition

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-transition}} from to ${KEY} stereotype keyword spec style spacing filter layer id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `from` ---- 接続元となる要素の ID をキーワードで指定します。point 値も指定できます。
* `to` ---- 接続先となる要素の ID をキーワードで指定します。point 値も指定できます。
* `stereotype` ---- ステレオタイプを明示する場合はステレオタイプ情報を指定します。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `keyword` ---- キーワードを明示する場合はステレオタイプ情報を指定します。 `stereotype` が指定されている場合は無視されます。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `spec` ---- 遷移にトリガーやガード条件、アクションを明示する場合に指定します。詳細は後述します。
* `style` ---- 接続線の引き方を指定します。詳細は [$@ 節](#コネクタ)を参照してください。
* `spacing` ---- 2 回以上折れ曲がる接続線における「自由な線分」の位置を調整するためのパラメータです。詳細は [$@ 節](#コネクタ)を参照してください。
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します。
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します。
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します。

${DESCRIPTION}

　UML 状態マシン図における遷移を描画します。スタイルを統一したい場合、
with-uml-transition-options マクロを使うことができます。

　`spec` に指定する遷移仕様は、 `:spec '(:trigger "event" :guard "v = 666" :action "foo()")` と
いった要領で指定します。リスト内に名前付きパラメータを並べる形式で、以下のパラメータが指定
できます。

<!-- stack:push li class='syntax' -->

* ${KEY} trigger guard action offset font

<!-- stack:pop li -->

　`:trigger :guard :action` はそれぞれトリガーとガード条件、アクションで、それぞれ文字列で
指定します。これによって、先ほどの例で言えば `event[v = 666]/foo()` といった仕様が表示される
ことになります。 `:offset` は表示位置の調整を `(x y)` 形式で指定します。 `:font` はフォント
の指定が必要な場合に使用します。フォントの指定を省略した場合、with-uml-transition-options マクロ
で指定されているフォントが使用されます。

${SEE_ALSO}

* [$$](#状態マシン図)
* [$$](#uml-transition)
* with-uml-transition-options マクロ

${NO_NOTES}


<!-- autolink: [uml-transition マクロ](#macro uml-transition) -->

${BLANK_PARAGRAPH}

#### macro uml-usecase

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{uml-usecase}} position text ${KEY} pivot stereotype keyword width height font fill stroke margin link filter layer id

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `position` ---- 描画の基準点を指定します。詳細は「[](#座標と位置)」を参照してください。
* `text` ---- ユースケースのテキストを指定します。改行を含むことで複数行を記述できます。
* `pivot` ---- `position` で指定した基準点が図形要素のどこにくるように描画するかを指定します。詳細は「[](#座標と位置)」を参照してください。
* `stereotype` ---- ステレオタイプを明示する場合はステレオタイプ情報を指定します。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `keyword` ---- キーワードを明示する場合はステレオタイプ情報を指定します。 `stereotype` が指定されている場合は無視されます。詳細は [$@ 節](#uml-stereotype-info) を参照してください。
* `width` ---- 幅を自動決定せず、明示的に指定したい場合に数値で指定します。
* `height` ---- 高さを自動決定せず、明示的に指定したい場合に数値で指定します。
* `font` ---- フォントを指定します。
* `fill` ---- 内部の塗り潰しを指定します。
* `stroke` ---- 外枠を描画するストロークを指定します
* `margin` ---- テキストの周囲に確保する余白を数値で指定します。デフォルト値は 20 です。
* `link` ---- リンクにする場合、リンク先を指定します。
* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します

${DESCRIPTION}

　UML ユースケース図におけるユースケースを描画します。スタイルを統一したい場合、
with-uml-action-options マクロを使うことができます。

${SEE_ALSO}

* [$$](#ユースケース図)
* [$$](#uml-usecase)
* with-uml-usecase-options マクロ

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

#### macro with-clipping-current-canvas

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-clipping-current-canvas}} ${BODY} body

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `body` ---- クリッピングの対象となる描画コードを記述します。

${DESCRIPTION}

　現在のキャンバスを使ってクリッピングを行ないます。

${SEE_ALSO}

* クリッピング
* with-clipping-use マクロ

${NO_NOTES}


<!-- autolink: [with-clipping-current-canvas マクロ](#macro with-clipping-current-canvas) -->

${BLANK_PARAGRAPH}

#### macro with-clipping-use

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-clipping-use}} (id) ${BODY} body

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `id` ---- クリッピングパスとして扱う既出の図形要素の ID をキーワードシンボルで指定します。
* `body` ---- クリッピングの対象となる描画コードを記述します。

${DESCRIPTION}

　既出の図形要素の ID を指定し、その描画パスを使ってクリッピングを行ないます。

${SEE_ALSO}

* クリッピング
* with-clipping-current-canvas マクロ

${NOTES}

　`id` で指定する図形要素の ID は、その図形要素の記述において明示的に指定されていなければ
なりません。たとえばコネクタなどでは `$1.id` といった記述で ID が明示的に指定されていない
図形要素を指定できますが、 with-clipping-use マクロではそれはできません
{{fn: これは、省略時に自動的に付与される ID は SVG コード上には現れないからです。しかし、この挙動は \
将来変更される可能性があります。}}。


<!-- autolink: [with-clipping-use マクロ](#macro with-clipping-use) -->

${BLANK_PARAGRAPH}

#### macro with-connector-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-connector-options}} (${KEY} style spacing stroke filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　connect マクロで描画されるコネクタのデフォルトオプションを変更します。キーワードパラメータ
群の説明は connect マクロを参照してください。

${SEE_ALSO}

* コネクタ
* connector マクロ

${NO_NOTES}


<!-- autolink: [with-connector-options マクロ](#macro with-connector-options) -->

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

#### macro with-paragraph-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-paragraph-options}} (${KEY} align valign font layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　paragraph マクロで描画されるドキュメントのデフォルトオプションを変更します。キーワード
パラメータ群の説明は paragraph マクロを参照してください。

${SEE_ALSO}

* ドキュメント
* paragraph マクロ

${NO_NOTES}


<!-- autolink: [with-paragraph-options マクロ](#macro with-paragraph-options) -->

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

* ${{B}{with-uml-action-options}} (${KEY} font fill stroke icon width height corner-r margin filter layer) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　uml-action マクロで描画される図形要素のデフォルトオプションを変更します。キーワード
パラメータ群の説明は uml-action マクロを参照してください。

　なお、ここで指定する width および height はデフォルトの最低サイズとして使用されます。

${SEE_ALSO}

* uml-action マクロ

${NO_NOTES}


<!-- autolink: [with-uml-action-options マクロ](#macro with-uml-action-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-activity-final-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-activity-final-options}} (${KEY} radius ratio fill stroke filter layer) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　uml-activity-final マクロで描画される図形要素のデフォルトオプションを変更します。
キーワードパラメータ群の説明は uml-activity-final マクロを参照してください。

${SEE_ALSO}

* uml-activity-final マクロ

${NO_NOTES}


<!-- autolink: [with-uml-activity-final-options マクロ](#macro with-uml-activity-final-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-activity-start-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-activity-start-options}} (${KEY} radius fill filter layer) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　uml-activity-start マクロで描画される図形要素のデフォルトオプションを変更します。
キーワードパラメータ群の説明は uml-activity-start マクロを参照してください。

${SEE_ALSO}

* uml-activity-start マクロ

${NO_NOTES}


<!-- autolink: [with-uml-activity-start-options マクロ](#macro with-uml-activity-start-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-actor-options

<!-- stack:push li class='syntax' -->
${SYNTAX}
t
* ${{B}{with-uml-actor-options}} (${KEY} fill stroke size width-ratio head-ratio shoulder-position thigh-position filter layer) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　uml-actor マクロで描画される図形要素のデフォルトオプションを変更します。キーワード
パラメータ群の説明は uml-actor マクロを参照してください。uml-actor マクロに現れない
パラメータについては後述します。

${SEE_ALSO}

* uml-actor マクロ

${NOTES}

　uml-actor マクロのパラメータに対応しないものとして、以下があります。これらはアクター
のアイコン形状を制御するものです。

* `width-ratio` : `size` で指定するサイズに対して描画される人型の幅を決定するための係数です。デフォルト値は 0.620 です。
* `head-ratio` : 頭部の円の大きさを高さに対する比で決定するための係数です。デフォルト値は 0.4 です。
* `shoulder-position` : 「肩」の位置（つまり横棒で描画される腕の高さ）を決定するための係数です。デフォルト値は 0.5 です。
* `thigh-position` : 「腰」の位置（つまり斜めの棒で描画される二本の脚が胴体から分岐する位置）を決定するための係数です。デフォルト値は 0.7 です。


<!-- autolink: [with-uml-actor-options マクロ](#macro with-uml-actor-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-aggregation-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-aggregation-options}} (${KEY} stroke arrow-size font filter layer) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　uml-aggregation マクロで描画される図形要素のデフォルトオプションを変更します。キーワード
パラメータ群の説明は uml-aggregation マクロを参照してください。

${SEE_ALSO}

* uml-aggregation マクロ

${NO_NOTES}


<!-- autolink: [with-uml-aggregation-options マクロ](#macro with-uml-aggregation-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-artifact-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-artifact-options}} (${KEY} font fill stroke icon width height margin filter layer) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　uml-artifact マクロで描画される図形要素のデフォルトオプションを変更します。
キーワードパラメータ群の説明は uml-artifact マクロを参照してください。uml-artifact マクロ
に現れないパラメータについては後述します。

　なお、ここで指定する width および height はデフォルトの最低サイズとして使用されます。

${SEE_ALSO}

* uml-artifact マクロ

${NO_NOTES}


<!-- autolink: [with-uml-artifact-options マクロ](#macro with-uml-artifact-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-association-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-association-options}} (${KEY} stroke arrow-size font filter layer) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　uml-association マクロで描画される図形要素のデフォルトオプションを変更します。キーワード
パラメータ群の説明は uml-association マクロを参照してください。

${SEE_ALSO}

* uml-association マクロ

${NO_NOTES}


<!-- autolink: [with-uml-association-options マクロ](#macro with-uml-association-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-class-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-class-options}} (${KEY} font fill stroke filter layer width height name-margin margin draw-emptybox) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　uml-class マクロで描画される図形要素のデフォルトオプションを変更します。キーワード
パラメータ群の説明は uml-class マクロを参照してください。

${SEE_ALSO}

* uml-class マクロ

${NO_NOTES}


<!-- autolink: [with-uml-class-options マクロ](#macro with-uml-class-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-component-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-component-options}} (${KEY} font fill stroke icon width height margin filter layer) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　uml-component マクロで描画される図形要素のデフォルトオプションを変更します。
キーワードパラメータ群の説明は uml-component マクロを参照してください。uml-component マクロ
に現れないパラメータについては後述します。

　なお、ここで指定する width および height はデフォルトの最低サイズとして使用されます。

${SEE_ALSO}

* uml-component マクロ

${NO_NOTES}


<!-- autolink: [with-uml-component-options マクロ](#macro with-uml-component-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-composition-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-composition-options}} (${KEY} stroke arrow-size font filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　uml-composition マクロで描画される図形要素のデフォルトオプションを変更します。
キーワードパラメータ群の説明は uml-composition マクロを参照してください。

${SEE_ALSO}

* uml-composition マクロ

${NO_NOTES}


<!-- autolink: [with-uml-composition-options マクロ](#macro with-uml-composition-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-connector-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-connector-options}} (${KEY} font fill stroke size filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　uml-connector マクロで描画される図形要素のデフォルトオプションを変更します。キーワード
パラメータ群の説明は uml-connector マクロを参照してください。

${SEE_ALSO}

* uml-connector マクロ

${NO_NOTES}


<!-- autolink: [with-uml-connector-options マクロ](#macro with-uml-connector-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-decision-merge-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-decision-merge-options}} (${KEY} font fill stroke width height margin filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　uml-decision マクロおよび uml-merge マクロで描画される図形要素のデフォルトオプションを
変更します。キーワードパラメータ群の説明はそれぞれのマクロを参照してください。

${SEE_ALSO}

* uml-decision マクロ
* uml-merge マクロ

${NO_NOTES}


<!-- autolink: [with-uml-decision-merge-options マクロ](#macro with-uml-decision-merge-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-dependency-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-dependency-options}} (${KEY} stroke arrow-size font filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　uml-dependency マクロで描画される図形要素のデフォルトオプションを変更します。キーワード
パラメータ群の説明は uml-dependency マクロを参照してください。

${SEE_ALSO}

* uml-dependency マクロ

${NO_NOTES}


<!-- autolink: [with-uml-dependency-options マクロ](#macro with-uml-dependency-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-expansion-region-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-expansion-region-options}} (${KEY} fill stroke corner-r filter layer) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　uml-expansion-region マクロで描画される図形要素のデフォルトオプションを変更します。
キーワードパラメータ群の説明は uml-expansion-region マクロを参照してください。

${SEE_ALSO}

* uml-expansion-region マクロ

${NO_NOTES}


<!-- autolink: [with-uml-expansion-region-options マクロ](#macro with-uml-expansion-region-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-flow-final-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-flow-final-options}} (${KEY} radius fill stroke filter layer) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　uml-flow-final マクロで描画される図形要素のデフォルトオプションを変更します。
キーワードパラメータ群の説明は uml-flow-final マクロを参照してください。

${SEE_ALSO}

* uml-flow-final マクロ

${NO_NOTES}


<!-- autolink: [with-uml-flow-final-options マクロ](#macro with-uml-flow-final-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-flow-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-flow-options}} (${KEY} stroke arrow-size font filter layer) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　uml-flow マクロで描画される図形要素のデフォルトオプションを変更します。キーワード
パラメータ群の説明は uml-flow マクロを参照してください。

${SEE_ALSO}

* uml-flow マクロ

${NO_NOTES}


<!-- autolink: [with-uml-flow-options マクロ](#macro with-uml-flow-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-fork-join-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-fork-join-options}} (${KEY} fill width length filter layer) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　uml-fork マクロおよび uml-join マクロで描画される図形要素のデフォルトオプションを
変更します。キーワードパラメータ群の説明はそれぞれのマクロを参照してください。

${SEE_ALSO}

* uml-fork マクロ
* uml-join マクロ

${NO_NOTES}


<!-- autolink: [with-uml-fork-join-options マクロ](#macro with-uml-fork-join-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-frame-fragment

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-frame-fragment}} (id index) ${BODY} body

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `id` ---- 対象となる uml-frame の ID を指定します。
* `index` ---- 対象となる領域のインデックスを上から順に０から始まる整数で指定します。
* `body` ---- 対象となるサブキャンバス内で行なう描画コードを記述します。

${DESCRIPTION}

　uml-frame の指定された領域（マトリクス状のパーティションの場合はセル）を
サブキャンバスとした描画を行ないます。

${SEE_ALSO}

* [$$](#uml-frame)
* uml-frame マクロ

${NOTES}

　`index` で指定できる値は、uml-frame の `:fragments` で指定した要素数が上限となります。
たとえば `:fragments '(50 40)` とした場合、点線が２本引かれて３つの領域に分割されるため、 
`0 1 2` が指定できることになります。


<!-- autolink: [with-uml-frame-fragment マクロ](#macro with-uml-frame-fragment) -->

${BLANK_PARAGRAPH}

#### macro with-uml-frame-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-frame-options}} (${KEY} font fill stroke margin fragment-dasharray filter layer) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　uml-frame マクロで描画される図形要素のデフォルトオプションを変更します。キーワード
パラメータ群の説明は uml-frame マクロを参照してください。ただし、以下はこのマクロで
のみ指定可能です。

* `fragment-dasharray` ---- `:fragments` パラメータで領域を区切る際に描画される点線の仕様を指定します。。詳細は [$@ 節](#ストローク)を参照してください。

${SEE_ALSO}

* uml-frame マクロ

${NO_NOTES}


<!-- autolink: [with-uml-frame-options マクロ](#macro with-uml-frame-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-generalization-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-generalization-options}} (${KEY} stroke fill arrow-size font filter layer) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　uml-generalization マクロで描画される図形要素のデフォルトオプションを変更します。
キーワードパラメータ群の説明は uml-generalization マクロを参照してください。

${SEE_ALSO}

* uml-generalization マクロ

${NO_NOTES}


<!-- autolink: [with-uml-generalization-options マクロ](#macro with-uml-generalization-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-interface-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-interface-options}} (${KEY} radius fill stroke filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　uml-interface マクロで描画される図形要素のデフォルトオプションを変更します。キーワード
パラメータ群の説明は uml-interface マクロを参照してください。

${SEE_ALSO}

* uml-interface マクロ

${NO_NOTES}


<!-- autolink: [with-uml-interface-options マクロ](#macro with-uml-interface-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-interface-request-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-interface-request-options}} (${KEY} stroke filter layer font degree dasharray arrow-size) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　uml-interface-request マクロで描画される図形要素のデフォルトオプションを変更します。キーワード
パラメータ群の説明は uml-interface-request マクロを参照してください。ただし、以下はこのマクロで
のみ指定可能です。

* `font` ---- `name` で指定するラベルのフォント指定を省略した場合に使用されるデフォルト設定です。
* `degree` ---- ソケットの円弧の角度です。
* `dasharray` ---- 依存関係矢印を描画する場合の点線を指定する dasharray パラメータです。
* `arrow-size` ---- 依存関係矢印を描画する場合の矢印のサイズです。

${SEE_ALSO}

* uml-interface-request マクロ

${NO_NOTES}


<!-- autolink: [with-uml-interface-request-options マクロ](#macro with-uml-interface-request-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-node-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-node-options}} (${KEY} font fill1 fill2 stroke width height depth margin filter layer) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　uml-node マクロで描画される図形要素のデフォルトオプションを変更します。キーワード
パラメータ群の説明は uml-node マクロを参照してください。

${SEE_ALSO}

* uml-node マクロ

${NO_NOTES}


<!-- autolink: [with-uml-node-options マクロ](#macro with-uml-node-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-note-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-note-options}} (${KEY} margin crease font fill stroke filter layer dasharray) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　uml-note マクロで描画される図形要素のデフォルトオプションを変更します。キーワード
パラメータ群の説明は uml-note マクロを参照してください。ただし、以下はこのマクロで
のみ指定可能です。

* `dasharray` ---- uml-note からその `targets` に引かれる点線の仕様を指定します。詳細は [$@ 節](#ストローク)を参照してください。

${SEE_ALSO}

* uml-note マクロ

${NO_NOTES}


<!-- autolink: [with-uml-note-options マクロ](#macro with-uml-note-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-package-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-package-options}} (${KEY} font fill stroke width height tab-width tab-height tab-margin margin filter layer) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　uml-package マクロで描画される図形要素のデフォルトオプションを変更します。キーワード
パラメータ群の説明は uml-package マクロを参照してください。

${SEE_ALSO}

* uml-package マクロ

${NO_NOTES}


<!-- autolink: [with-uml-package-options マクロ](#macro with-uml-package-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-partition-lane

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-partition-lane}} (id name ${OPTIONAL} name2) ${BODY} body

<!-- stack:pop li -->

${ARGS_AND_VALS}

* `id` ---- 対象となる uml-partition の ID を指定します。
* `name` ---- 対象となるレーンの名前を文字列かキーワードシンボルで指定します。マトリクス状のパーティションの場合、行の名前を指定してください。
* `name2` ---- マトリクス状のパーティションの場合、列の名前を文字列かキーワードシンボルで指定します。
* `body` ---- 対象となるサブキャンバス内で行なう描画コードを記述します。

${DESCRIPTION}

　uml-partition の指定されたレーン（マトリクス状のパーティションの場合はセル）を
サブキャンバスとした描画を行ないます。

${SEE_ALSO}

* [$$](#uml-partition)
* uml-partition マクロ

${NO_NOTES}


<!-- autolink: [with-uml-partition-lane マクロ](#macro with-uml-partition-lane) -->

${BLANK_PARAGRAPH}

#### macro with-uml-partition-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-partition-options}} (${KEY} font fill stroke lines header layer) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　uml-partition マクロで描画される図形要素のデフォルトオプションを変更します。キーワード
パラメータ群の説明は uml-partition マクロを参照してください。

${SEE_ALSO}

* uml-partition マクロ

${NO_NOTES}


<!-- autolink: [with-uml-partition-options マクロ](#macro with-uml-partition-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-pin-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-pin-options}} (${KEY} font fill stroke size filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　uml-pin マクロで描画される図形要素のデフォルトオプションを変更します。キーワード
パラメータ群の説明は uml-pin マクロを参照してください。

${SEE_ALSO}

* uml-pin マクロ

${NO_NOTES}


<!-- autolink: [with-uml-pin-options マクロ](#macro with-uml-pin-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-port-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-port-options}} (${KEY} fill stroke size filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　uml-port マクロで描画される図形要素のデフォルトオプションを変更します。キーワード
パラメータ群の説明は uml-port マクロを参照してください。

${SEE_ALSO}

* uml-port マクロ

${NO_NOTES}


<!-- autolink: [with-uml-port-options マクロ](#macro with-uml-port-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-realization-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-realization-options}} (${KEY} stroke fill arrow-size font filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　uml-realization マクロで描画される図形要素のデフォルトオプションを変更します。キーワード
パラメータ群の説明は uml-realization マクロを参照してください。

${SEE_ALSO}

* uml-realization マクロ

${NO_NOTES}


<!-- autolink: [with-uml-realization-options マクロ](#macro with-uml-realization-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-signal-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-signal-options}} (${KEY} font fill stroke width height direction depth margin filter layer) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　uml-signal マクロで描画される図形要素のデフォルトオプションを変更します。キーワード
パラメータ群の説明は uml-signal マクロを参照してください。

${SEE_ALSO}

* uml-signal マクロ

${NO_NOTES}


<!-- autolink: [with-uml-signal-options マクロ](#macro with-uml-signal-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-state-begin-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-state-begin-options}} (${KEY} radius fill filter layer) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　uml-state-begin マクロで描画される図形要素のデフォルトオプションを変更します。キーワード
パラメータ群の説明は uml-state-begin マクロを参照してください。

${SEE_ALSO}

* uml-state-begin マクロ

${NO_NOTES}


<!-- autolink: [with-uml-state-begin-options マクロ](#macro with-uml-state-begin-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-state-end-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-state-end-options}} (${KEY} radius ratio fill stroke filter layer) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　uml-state-end マクロで描画される図形要素のデフォルトオプションを変更します。キーワード
パラメータ群の説明は uml-state-end マクロを参照してください。

${SEE_ALSO}

* uml-state-end マクロ

${NO_NOTES}


<!-- autolink: [with-uml-state-end-options マクロ](#macro with-uml-state-end-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-state-history-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-state-history-options}} (${KEY} radius fill stroke filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　uml-state-history マクロで描画される図形要素のデフォルトオプションを変更します。キーワード
パラメータ群の説明は uml-state-history マクロを参照してください。

${SEE_ALSO}

* uml-state-history マクロ

${NO_NOTES}


<!-- autolink: [with-uml-state-history-options マクロ](#macro with-uml-state-history-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-state-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-state-options}} (${KEY} font fill stroke width height corner-r margin filter layer) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　uml-state マクロで描画される図形要素のデフォルトオプションを変更します。キーワード
パラメータ群の説明は uml-state マクロを参照してください。

　なお、ここで指定する width および height はデフォルトの最低サイズとして使用されます。

${SEE_ALSO}

* uml-state マクロ

${NO_NOTES}


<!-- autolink: [with-uml-state-options マクロ](#macro with-uml-state-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-time-event-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-time-event-options}} (${KEY} fill stroke width height filter layer) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　uml-time-event マクロで描画される図形要素のデフォルトオプションを変更します。キーワード
パラメータ群の説明は uml-time-event マクロを参照してください。

${SEE_ALSO}

* uml-time-event マクロ

${NO_NOTES}


<!-- autolink: [with-uml-time-event-options マクロ](#macro with-uml-time-event-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-transition-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-transition-options}} (${KEY} stroke arrow-size font filter layer) ${BODY} body

<!-- stack:pop li -->

${DESCRIPTION}

　uml-transition マクロで描画される図形要素のデフォルトオプションを変更します。キーワード
パラメータ群の説明は uml-transition マクロを参照してください。

${SEE_ALSO}

* uml-transition マクロ

${NO_NOTES}


<!-- autolink: [with-uml-transition-options マクロ](#macro with-uml-transition-options) -->

${BLANK_PARAGRAPH}

#### macro with-uml-usecase-options

<!-- stack:push li class='syntax' -->
${SYNTAX}

* ${{B}{with-uml-usecase-options}} (${KEY} font fill stroke margin filter layer) ${BODY} body


<!-- stack:pop li -->

${DESCRIPTION}

　uml-usecase マクロで描画される図形要素のデフォルトオプションを変更します。キーワード
パラメータ群の説明は uml-usecase マクロを参照してください。

${SEE_ALSO}

* uml-usecase マクロ

${NO_NOTES}


<!-- autolink: [with-uml-usecase-options マクロ](#macro with-uml-usecase-options) -->

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

