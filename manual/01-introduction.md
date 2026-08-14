
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

