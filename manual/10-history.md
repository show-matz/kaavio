
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
    * ENHANCE : `obj.center` に対する `obj.cc` などの簡略記法を導入（[$@ 節](#座標と位置)参照）
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
* __2026/03/20 - version 0.032__
    * DOCUMENT : マニュアル（この文書）に CSS を埋め込む方式に変更
* __2026/05/19 - version 0.033__
    * ENHANCE : [$$](#パイプ)を追加
* __2026/05/22 - version 0.034__
    * ENHANCE : [$$](#星型)を追加
    * ENHANCE : [$$](#禁止マーク)を追加

${BLANK_PARAGRAPH}

