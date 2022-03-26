<!-- define: APPNAME = diagram -->
<!-- define: BLANK_PARAGRAPH = '　　' -->

<!-- title:${APPNAME} readme -->    
<!-- style:./default.css -->			
<!-- config:term-link-in-header -->			

<!-- filter:diagram = bash ./diagram.sh %in %out -->
<!-- filter:plantuml = bash ./plantuml.sh %in %out -->

<!-- config:write-comment -->			
<!-- config:header-numbering 2 4 -->			

# README - ${APPNAME}

　この文書は、 **${APPNAME}** のためのマニュアル文書です。


## Table of contents

<!-- embed:toc 2 4 -->

${BLANK_PARAGRAPH}

## ${APPNAME} とは

　${APPNAME} は、テキストベースの作図ツールです。テキスト形式で作成したデータファイルを入力として、
SVG 形式{{fn:SVG は Scalable Vector Graphics の略です。}}の画像ファイルを生成します。

　入力データの記述には Common LISP 言語を使用します。これは、diagram 自身が Common LISP で
書かれているからですが、


## 簡単なサンプル

　簡単なサンプルから始めましょう。以下のような入力を ${APPNAME} に与えると、

<!-- snippet: FIRST-SAMPLE
(create-svg (:width 300 :height 150)
  (rectangle '( 50  50) 80 60 :id :x)
  (circle    '(250 100) 40    :id :y)
  (connector :x :y :end2 :arrow))
-->

```lisp
<!-- expand: FIRST-SAMPLE -->
```

${BLANK_PARAGRAPH}


　以下のような画像が生成されます。

```diagram
<!-- expand: FIRST-SAMPLE -->
```
Figure. 簡単なサンプル


* create-svg で、幅 300、高さ 150 の画像を作成します
* rectangle で、左上から (50, 50) の位置に幅 80、高さ 60 の矩形を作成し、これに :x という ID をつけます
* circle で、左上から (250, 100) の位置に半径 40 の円を作成し、これに :y という ID をつけます
* connector で、:x から :y に向かって接続線を引き、終端の形状を矢印にしています

<!-- snippet: SECOND-SAMPLE
(create-svg (:width 300 :height 200)
; (rectangle '(150 100) 300 200)
  (rectangle '( 50  50) 50 50     :id :a1)
  (circle    (point/x+ a1.center 100) 25 :id :a2)
  (rectangle (point/y+ a2.center 100) 50 50 :id :a3)
  (circle    (point/x+ a3.center 100) 25 :id :a4)
  (connector :a1 :a2 :end2 :arrow)
  (connector :a2 :a3 :end2 :arrow)
  (connector :a3 :a4 :end2 :arrow))
-->


```lisp
<!-- expand: SECOND-SAMPLE -->
```

```diagram
<!-- expand: SECOND-SAMPLE -->
```
Figure. 簡単なサンプル-2



## 線・塗り潰し・文字

### ストローク


#### make-stroke 関数

```lisp
(make-stroke 10)       ;; equal to (make-stroke :width 10)

(make-stroke :blue)    ;; equal to (make-stroke :color :blue)

(make-stroke :color :red :width 3)

(make-stroke '(:color :red :width 3))

```

```lisp
(defun make-stroke &key :color :width :opacity :linecap
                        :linejoin :miterlimit :dasharray :dashoffset)
```

Table. make-stroke 関数のパラメータ
| parameter   | description          | default 値 |
|:============|:---------------------|:===========|
| :color      |                      | :black     |
| :width      |                      | 1          |
| :opacity    |                      | nil        |
| :linecap    |                      | nil        |
| :linejoin   |                      | nil        |
| :miterlimit |                      | nil        |
| :dasharray  |                      | nil        |
| :dashoffset |                      | nil        |


${BLANK_PARAGRAPH}


```diagram
(create-svg (:width 400 :height 100)
	(with-subcanvas ('(0 0) 100 100)
	  (let ((*default-stroke* (make-stroke :color :black :width 4 :dasharray '(8 4))))
		(line '((30 20) (70 20)))
		(line '((30 40) (70 40)) :stroke '(:dashoffset 2))
		(line '((30 60) (70 60)) :stroke '(:dashoffset 4))
		(line '((30 80) (70 80)) :stroke '(:dashoffset 6))))
	(with-subcanvas ('(100 0) 100 100)
	  (let ((*default-stroke* (make-stroke :color :black :width 8)))
		(line '((30 20) (70 20)))
		(line '((30 40) (70 40)) :stroke '(:linecap   :butt))
		(line '((30 60) (70 60)) :stroke '(:linecap  :round))
		(line '((30 80) (70 80)) :stroke '(:linecap :square))))
	(with-subcanvas ('(200 0) 100 100)
	  (let ((*default-stroke* (make-stroke :color :black :width 12)))
		(line '(( 30 60) ( 45 45) ( 60 60)) :stroke '(:linejoin :miter))
		(line '(( 90 60) (105 45) (120 60)) :stroke '(:linejoin :round))
		(line '((150 60) (165 45) (180 60)) :stroke '(:linejoin :bevel)))))
```
Figure. dashoffset, linecap, linejoin のサンプル



#### ＊default-stroke＊ 変数



### フィル

### フォント



## リファレンス

### create-svg

### with-subcanvas


## 更新履歴

　更新履歴です。


--------------------------------------------------------------------------------

<!-- embed:footnotes -->

