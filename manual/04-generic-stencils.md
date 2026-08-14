
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
<!-- define: HASH_STAR       = '[](#星型)' -->
<!-- define: HASH_CROSS      = '[](#十字)' -->
<!-- define: HASH_PIPE       = '[](#パイプ)' -->
<!-- define: HASH_BLOCKARROW = '[](#ブロック矢印)' -->
<!-- define: HASH_PROHIBITION = '[](#禁止マーク)' -->
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
    (defgroup (w h :star-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (star (y+ canvas.center -10) 5 80 80 "star" 
                                       :stroke :brown :fill :khaki)
      (text `(,(/ w 2) ,(- h 5)) "星型" :align :center))
    (defgroup (w h :cross-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (cross (y+ canvas.center -10) (- canvas.width 30) (- canvas.height 30) 20 
                                       :stroke :purple :fill :plum)
      (text `(,(/ w 2) ,(- h 5)) "十字" :align :center))
    (defgroup (w h :pipe-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (pipe '(50 40) :h 80 :stroke :black :fill :lightgray :label '("pipe" :offset (0 18)))
      (text `(,(/ w 2) ,(- h 5)) "パイプ" :align :center))
    (defgroup (w h :blockarrow-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (block-arrow1 '(0 40) '(100 40) 20 :margin 5 :stroke :brown :fill :burlywood)
      (text `(,(/ w 2) ,(- h 5)) "ブロック矢印" :align :center))
    (defgroup (w h :prohibit-grp)
      (rect canvas.center canvas.width canvas.height :stroke :none :fill bgclr)
      (prohibition '(50 40) 60 :stroke :red :fill :pink)
      (text `(,(/ w 2) ,(- h 5)) "禁止マーク" :align :center))
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
    (use :star-grp       '(720 180) :link "${HASH_STAR}")
    (use :cross-grp      '( 70 300) :link "${HASH_CROSS}")
    (use :pipe-grp       '(200 300) :link "${HASH_PIPE}")
    (use :blockarrow-grp '(330 300) :link "${HASH_BLOCKARROW}")
    (use :prohibit-grp   '(460 300) :link "${HASH_PROHIBITION}")
    (use :brace-grp      '(590 300) :link "${HASH_BRACE}")
    (use :table-grp      '(720 300) :link "${HASH_TABLE}")))
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

### 星型
<!-- autolink: [$$](#星型) -->

<!-- snippet: STAR-SAMPLE
(diagram (300 150)
  (grid)
  (star '( 80 75) 7 100 100 "star1" :stroke :brown :fill :khaki)
  (star '(220 75) 5 120 120 "star2" :stroke :navy  :fill :azure :tilt 10))
-->

　star マクロにより、星型を描画できます。

```kaavio
<!-- expand: STAR-SAMPLE -->
```
Figure. 星型のサンプル

　上記サンプルのソースは以下の通りです。パラメータの詳細については star マクロを参照して
ください。

```lisp
<!-- expand: STAR-SAMPLE -->
```

${BLANK_PARAGRAPH}

　図の中で星型のスタイルを統一する作業を簡単にするために、with-star-options マクロが
用意されています。これを以下のように使用することで、複数の星型のスタイルを一箇所で指定
することができます。

<!-- snippet: WITH-STAR-OPTIONS-SAMPLE
(diagram (300 100)
  (grid)
  (drop-shadow)
  (with-star-options (:stroke :brown :fill :khaki :filter :drop-shadow)
    (star '( 80 50) 6 80 80 "star1")
    (star '(220 50) 5 80 80 "star2")))
-->

```lisp
<!-- expand: WITH-STAR-OPTIONS-SAMPLE -->
```

```kaavio
<!-- expand: WITH-STAR-OPTIONS-SAMPLE -->
```
Figure. with-star-options のサンプル

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

### パイプ
<!-- autolink: [$$](#パイプ) -->

<!-- snippet: PIPE-SAMPLE
(diagram (300 150)
  (grid)
  (with-options (:fill :skyblue
                 :stroke '(:color :navy :width 2))
    (pipe '( 50 75) :v  90 :width 30 :label "pipe1")
    (pipe '(180 75) :h 150 :depth 16 :label '("pipe2" :offset (0 18)))))
-->

　pipe マクロにより、縦方向または横方向に伸びる細長いパイプを描画できます。

```kaavio
<!-- expand: PIPE-SAMPLE -->
```
Figure. パイプのサンプル

　上記サンプルのソースは以下の通りです。パラメータの詳細については pipe マクロを参照して
ください。

```lisp
<!-- expand: PIPE-SAMPLE -->
```

${BLANK_PARAGRAPH}

　図の中でパイプのスタイルを統一する作業を簡単にするために、with-pipe-options マクロが
用意されています。これを以下のように使用することで、複数のパイプのスタイルを一箇所で指定
することができます。

<!-- snippet: WITH-PIPE-OPTIONS-SAMPLE
(diagram (300 100)
  (grid)
  (with-pipe-options (:stroke :orangered3 :fill :thistle1)
    (pipe '( 50 50) :v  40 :label "pipe1")
    (pipe '(180 50) :h 150 :label '("pipe2" :offset (0 18)))))
-->

```lisp
<!-- expand: WITH-PIPE-OPTIONS-SAMPLE -->
```

```kaavio
<!-- expand: WITH-PIPE-OPTIONS-SAMPLE -->
```
Figure. with-pipe-options のサンプル

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

### 禁止マーク
<!-- autolink: [$$](#禁止マーク) -->

<!-- snippet: PROHIBITION-SAMPLE
(diagram (300 150)
  (grid)
  (drop-shadow)
  (prohibition '( 80 60) 100 :stroke :red     :fill :pink   :filter :drop-shadow)
  (prohibition '(220 60) 100 :stroke :crimson :fill :salmon :label "with label."))
-->

　prohibition マクロにより、禁止マークを描画できます。

```kaavio
<!-- expand: PROHIBITION-SAMPLE -->
```
Figure. 禁止マークのサンプル

　上記サンプルのソースは以下の通りです。パラメータの詳細については prohibition マクロを参照して
ください。

```lisp
<!-- expand: PROHIBITION-SAMPLE -->
```

${BLANK_PARAGRAPH}

　図の中で禁止マークのスタイルを統一する作業を簡単にするために、with-prohibition-options マクロが
用意されています。これを以下のように使用することで、複数の禁止マークのスタイルを一箇所で指定
することができます。

<!-- snippet: WITH-PROHIBITION-OPTIONS-SAMPLE
(diagram (300 110)
  (grid)
  (drop-shadow)
  (with-prohibition-options (:fill :pink
                             :stroke '(:color :crimson :width 2))
    (prohibition '( 80 40) 60 :filter :drop-shadow)
    (prohibition '(220 40) 60 :label "with label.")))
-->

```lisp
<!-- expand: WITH-PROHIBITION-OPTIONS-SAMPLE -->
```

```kaavio
<!-- expand: WITH-PROHIBITION-OPTIONS-SAMPLE -->
```
Figure. with-prohibition-options のサンプル

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

