# kaavio

　kaavio は、テキストベースの作図ツールです。テキスト形式のデータファイルを入力として、
SVG 形式の画像ファイルを生成します。

## インストール

　kaavio は Common Lisp で書かれています。使用には Common Lisp 処理系が必要に
なります。

　このプロジェクトを git clone して、ASDF から `src/kaavio.asd` を利用できるよう
に設定してください。kaavio をロードすれば、あとは `kaavio:diagram` マクロで始まる
図面データを実行することで SVG 図面を生成することができます。図面データ作成の詳細に
ついては `manual/manual.html` を参照してください。

## 実行可能バイナリの作成

　kaavio は実行可能バイナリの作成を想定して作られています。kaavio が Common Lisp 
処理系で使えるように設定されている環境であれば、 `misc/` ディレクトリ配下で `make` 
と言うだけで実行可能バイナリを作成できます。SBCL だけがインストールされている環境であれば、
`make bare-build` と言うことでも実行可能バイナリを作成できます。ただし、これらの方法は現状 
SBCL でしか試しておらず、他の処理系では `misc/` 配下のファイルを少し変更する必要があると
思います。

## sandbox mode

　`--sandbox` を指定して 実行可能バイナリを起動すれば、sandbox mode を利用することが
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

