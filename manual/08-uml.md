
## UML

　kaavio は、UML を（一部）サポートしています。

### アクティビティ図
<!-- autolink: [$$](#アクティビティ図) -->

　UML アクティビティ図の例を以下に示します。

<!-- snippet: UML-ACTIVITY-DIAGRAM-SAMPLE
(diagram (800 270)
  (grid)
  (with-theme (:uml-activity-default)
    (uml-activity-start '(30 60) :id :start)
    (uml-action   (x+ start.cc  90) "step1" :rake t :id :step1)
    (uml-note (xy+ step1.cc -30 80) 130 60
              "action with~%rake icon."
              :targets :step1 :keyword "memo")
    (uml-decision (x+ step1.cc 120)
                  :text "check~%status" :width 80 :height 40 :id :check)
    (uml-signal   (y+ check.center 100) :send
                  "signal~%error" :direction :left :id :signal)
    (uml-action (xy+ check.center 290 40) "step2"
                :id :step2 :width 400 :height 160 :contents t)
    (labels ((flow-chain (from &rest rest)
               (when rest
                 (uml-flow from (car rest))
                 (apply #'flow-chain (car rest) (cdr rest)))))
      (with-subcanvas-of (:step2)
        (uml-activity-start (x+ canvas.cl 20) :id :start2)
        (uml-fork           (x+ start2.cc 40) :h :id :fork)
        (uml-action (xy+ $1.cc 80 -30)   "sub1" :id :sub1)
        (uml-action (x+  $1.cc 120)      "sub2" :id :sub2)
        (uml-action (xy+ fork.cc 140 30) "sub3" :id :sub3)
        (uml-join           (xy+ sub2.cc 80 30) :h :id :join)
        (uml-activity-final (x+  join.cc 40) :id :final2)
        (flow-chain :start2 :fork :sub1 :sub2 :join :final2)
        (flow-chain :fork :sub3 :join))
      (uml-merge (y+ step2.bc 60) :id :merge)
      (uml-activity-final (x+ merge.center 180) :id :final)
      (flow-chain :start  :step1 :check)
      (uml-flow   :check  :signal
                  :spec '(:guard "NG" :offset ( 5 -15) :font 9))
      (uml-flow   :check  :step2 :style :RL1
                  :spec '(:guard "OK" :offset (-9   0) :font 9))
      (uml-flow   :signal :merge :style :BL)
      (flow-chain :step2  :merge :final))))
-->

```kaavio
<!-- expand: UML-ACTIVITY-DIAGRAM-SAMPLE -->
```
Figure. UML アクティビティ図の例

<!-- collapse:close -->
※上記サンプルのソースはこちら。

```lisp
<!-- expand: UML-ACTIVITY-DIAGRAM-SAMPLE -->
```
<!-- collapse:end -->

${BLANK_PARAGRAPH}

　UML アクティビティ図で使用する構成要素は以下を参照してください。

* [$$](#uml-action)
* [$$](#uml-activity-final)
* [$$](#uml-activity-start)
* [$$](#uml-connector)
* [$$](#uml-decision)
* [$$](#uml-expansion-region)
* [$$](#uml-flow)
* [$$](#uml-flow-final)
* [$$](#uml-fork)
* [$$](#uml-frame)
* [$$](#uml-join)
* [$$](#uml-merge)
* [$$](#uml-note)
* [$$](#uml-partition)
* [$$](#uml-pin)
* [$$](#uml-signal)
* [$$](#uml-time-event)

### クラス図
<!-- autolink: [$$](#クラス図) -->

　UML クラス図の例を以下に示します。

<!-- snippet: UML-CLASS-DIAGRAM-SAMPLE
(diagram (800 270)
  (grid)
  (with-theme (:uml-class-default)
    (uml-package (y+ canvas.tc 100) "GUI"
      :width 140 :height 110 :stroke :brown :fill :beige :id :gui
      :contents
      ((uml-class canvas.cc "Application" :id :app)))
    (uml-package (xy+ canvas.tc 200 100) "sys"
      :width 140 :height 110 :stroke :brown :fill :beige :id :sys
      :contents
      ((uml-class canvas.cc "Thread" :id :thread)))
    (uml-class (y+ app.cc    120) "TheApp" :id :theapp
      :width 120 :active t
      :operations
      ((:public "Run" :type :int)))
    (uml-note (xy+ theapp.cc -180 -80) 130 60
                "TheApp is~%active class."
                :fill :ivory :stroke :olive
                :targets :theapp :keyword "memo")
    (uml-class (y+ thread.cc 120) "Worker" :keyword '("thread" :font 9) :id :worker)
    (uml-generalization :theapp :app)
    (uml-generalization :worker :thread)
    (uml-aggregation :theapp :worker :arrow t)
    (uml-interface (x+ theapp.cc -170) "LogReceiver" :fill :lightgray :id :logrcv)
    (uml-class (x+ logrcv.cc -150) "Logger" :id :logger)
    (uml-interface-request :theapp :logrcv)
    (uml-association :logger :logrcv)))
-->

```kaavio
<!-- expand: UML-CLASS-DIAGRAM-SAMPLE -->
```
Figure. UML クラス図の例

<!-- collapse:close -->
※上記サンプルのソースはこちら。

```lisp
<!-- expand: UML-CLASS-DIAGRAM-SAMPLE -->
```
<!-- collapse:end -->

${BLANK_PARAGRAPH}

　UML クラス図で使用する構成要素は以下を参照してください。

* [$$](#uml-aggregation)
* [$$](#uml-association)
* [$$](#uml-class)
* [$$](#uml-composition)
* [$$](#uml-dependency)
* [$$](#uml-frame)
* [$$](#uml-generalization)
* [$$](#uml-interface)
* [$$](#uml-interface-request)
* [$$](#uml-stereotype-info)
* [$$](#uml-multiplicity-info)
* [$$](#uml-note)
* [$$](#uml-package)
* [$$](#uml-realization)
* [$$](#uml-role-info)

### コンポーネント図
<!-- autolink: [$$](#コンポーネント図) -->

　UML コンポーネント図の例を以下に示します。これは書籍「UML モデリングのエッセンス第３版」の図を
再現したものです。

<!-- snippet: UML-COMPONENT-DIAGRAM-SAMPLE
(diagram (700 400)
  (grid)
  (with-theme (:uml-component-default)
    (with-options (:font 10)
      (uml-component (xy+ canvas.tl 80 50) ": till" :id :till)
      (uml-interface (xy+ $1.cc 0 70)
                     '("sales message" :position :right :offset (5 0)) :id :sales-msg1)
      (uml-component (xy+ $1.cc 0 60) "~%: message~%queue" :id :msg-que)
      (uml-interface-request :till :sales-msg1)
      (uml-association :msg-que :sales-msg1)
      (uml-interface (xy+ msg-que.tr 50 (/ msg-que.height 4))
                     '("sales~%message" :position :below :offset (0 5)) :id :sales-msg2)
      (uml-interface-request :msg-que :sales-msg2 :style :R1L2)
      (uml-component (xy+ sales-msg2.cc 270 0) ": Sales Server"
                     :width 420 :height 150 :contents t :id :sales-svr)
      (uml-port sales-svr.CL :id :port1)
      (uml-association :port1 :sales-msg2)
      (with-subcanvas-of (:sales-svr)
        (uml-component (xy+ port1.cc 100 0)
                       "~%: transaction~%processor" :height 70 :id :tran-proc)
        (uml-interface (xy+ $1.cc  120 0) "" :id :tmp-interface)
        (uml-component (xy+ $1.cc  120 0)
                       "~%: accounting~%driver" :height 70 :id :account-drvr)
        (uml-interface-request :tran-proc :tmp-interface)
        (uml-association :account-drvr :tmp-interface)
        (uml-dependency :port1 :tran-proc))
      (uml-port (x+ sales-svr.B3 25) :id :port2)
      (uml-dependency :account-drvr :port2)
      (uml-interface (xy+ port2.cc 0 45)
                     '("receivables" :position :left :offset (-5 0)) :id :receivables)
      (uml-interface-request :port2 :receivables)
      (uml-component (xy+ receivables.cc 0 65)
                     "~%: accounting~%system" :height 70 :id :account-sys)
      (uml-association :account-sys :receivables))))
-->

```kaavio
<!-- expand: UML-COMPONENT-DIAGRAM-SAMPLE -->
```
Figure. UML コンポーネント図の例

<!-- collapse:close -->
※上記サンプルのソースはこちら。

```lisp
<!-- expand: UML-COMPONENT-DIAGRAM-SAMPLE -->
```
<!-- collapse:end -->

${BLANK_PARAGRAPH}

　UML コンポーネント図で使用する構成要素は以下を参照してください。

* [$$](#uml-component)
* [$$](#uml-dependency)
* [$$](#uml-frame)
* [$$](#uml-stereotype-info)
* [$$](#uml-note)
* [$$](#uml-package)
* [$$](#uml-port)
* [$$](#uml-interface)
* [$$](#uml-interface-request)
* [$$](#uml-association)
* [$$](#uml-realization)

### パッケージ図
<!-- autolink: [$$](#パッケージ図) -->

　UML パッケージ図の例を以下に示します。

<!-- snippet: UML-PACKAGE-DIAGRAM-SAMPLE
(diagram (700 300)
  (grid)
  (with-theme (:uml-package-default)
	(with-options (:font 10)
	  (uml-package (xy+ canvas.tl 100  60) "Application")
	  (uml-package (xy+ $1.cc     260   0) "Database~%Gateway" :id :db-gateway)
	  (uml-dependency $2.id $1.id)
	  (uml-package (xy+ $2.cc       0 160) "SQL Server~%Gateway")
	  (uml-realization $1.id :db-gateway)
	  (uml-package (xy+ $2.cc    -200   0) "Oracle~%Gateway")
	  (uml-realization $1.id :db-gateway :style :TB1)
	  (uml-package (xy+ $4.cc     200   0) "Tset Stub~%Gateway")
	  (uml-realization $1.id :db-gateway :style :TB3))))
-->

```kaavio
<!-- expand: UML-PACKAGE-DIAGRAM-SAMPLE -->
```
Figure. UML パッケージ図の例

<!-- collapse:close -->
※上記サンプルのソースはこちら。

```lisp
<!-- expand: UML-PACKAGE-DIAGRAM-SAMPLE -->
```
<!-- collapse:end -->

${BLANK_PARAGRAPH}

　UML パッケージ図で使用する構成要素は以下を参照してください。

* [$$](#uml-stereotype-info)
* [$$](#uml-dependency)
* [$$](#uml-frame)
* [$$](#uml-note)
* [$$](#uml-package)
* [$$](#uml-realization)

### 状態マシン図
<!-- autolink: [$$](#状態マシン図) -->

　UML 状態マシン図の例を以下に示します。

<!-- snippet: UML-STATEMACHINE-DIAGRAM-SAMPLE
(diagram (600 300)
  (grid)
  (with-theme (:uml-statemachine-default)
    (with-options (:font '(:width-spice 0.8))
      (uml-state (xy+ canvas.tc   0  40) "Show~%Connections" :id :s1 :width 100)
      (uml-state (xy+ $1.cc       0 160) "Enter Connection Details"
                 :id :s2 :width 560 :height 140 :contents t)
      (with-subcanvas-of (:s2)
        (uml-state-begin (x+ canvas.cl 30) :id :s3)
        (uml-state (xy+ $1.cc  90 0) "Enter phone~%number"    :height 40 :id :s4)
        (uml-state (xy+ $1.cc 180 0) "Choose shared~%or solo" :height 40 :id :s5)
        (uml-state (xy+ $1.cc 180 0) "Enter name"             :height 40 :id :s6)
        (uml-transition :s3 :s4)
        (uml-transition :s4 :s5 :style :R1L1 :spec :next)
        (uml-transition :s5 :s4 :style :L3R3 :spec :back)
        (uml-transition :s5 :s6 :style :R1L1 :spec :next)
        (uml-transition :s6 :s5 :style :L3R3 :spec :back))
      (uml-transition :s1 (x+ s2.tc -25) :style :B1T
                      :spec '(:trigger "new"    :offset (-30 -20)))
      (uml-transition (x+ s2.tc  25) :s1 :style :TB3
                      :spec '(:trigger "cancel" :offset ( 50  20)))
      (uml-transition :s6 :s1 :style :TR
                      :spec '(:trigger :save :offset (35 50))))))
-->

```kaavio
<!-- expand: UML-STATEMACHINE-DIAGRAM-SAMPLE -->
```
Figure. UML 状態マシン図の例

<!-- collapse:close -->
※上記サンプルのソースはこちら。

```lisp
<!-- expand: UML-STATEMACHINE-DIAGRAM-SAMPLE -->
```
<!-- collapse:end -->

${BLANK_PARAGRAPH}

　UML 状態マシン図で使用する構成要素は以下を参照してください。

* [$$](#uml-frame)
* [$$](#uml-note)
* [$$](#uml-state)
* [$$](#uml-state-begin)
* [$$](#uml-state-end)
* [$$](#uml-state-history)
* [$$](#uml-transition)
* [$$](#uml-transition-spec)

### ユースケース図
<!-- autolink: [$$](#ユースケース図) -->

　UML ユースケース図の例を以下に示します。

<!-- snippet: UML-USECASE-DIAGRAM-SAMPLE
(diagram (600 200)
  (grid)
  (with-theme (:uml-usecase-default)
	(with-options (:font 10)
	  (uml-actor   (xy+ canvas.tl 140  60) "user")
	  (uml-usecase (xy+ $1.cc     200   0) "make diagram." :id :case1)
	  (uml-association $2.id $1.id)
	  (uml-usecase (xy+ $2.cc      80  80) "make UML~%diagram." :id :case2)
	  (uml-generalization $1.id $3.id))))
-->

```kaavio
<!-- expand: UML-USECASE-DIAGRAM-SAMPLE -->
```
Figure. UML ユースケース図の例

<!-- collapse:close -->
※上記サンプルのソースはこちら。

```lisp
<!-- expand: UML-USECASE-DIAGRAM-SAMPLE -->
```
<!-- collapse:end -->

${BLANK_PARAGRAPH}

　UML ユースケース図で使用する構成要素は以下を参照してください。


* [$$](#uml-actor)
* [$$](#uml-association)
* [$$](#uml-frame)
* [$$](#uml-generalization)
* [$$](#uml-note)
* [$$](#uml-usecase)

### 配置図
<!-- autolink: [$$](#配置図) -->

　UML 配置図の例を以下に示します。

<!-- snippet: UML-DEPLOYMENT-DIAGRAM-SAMPLE
(diagram (600 300)
  (grid)
  (with-theme (:uml-deployment-default)
    (uml-node (xy+ canvas.tl  80  70) "BrowserClient" :width 120 :id :n1)
    (uml-node (xy+ canvas.tl 220  70) "Rich Client"   :width 120 :id :n2)
    (uml-node (xy+ canvas.tl 150 220) "Web Server"    :width 120 :id :n3)
    (uml-association :n3 :n1 :style :T1B :name '("http/internet" :offset (-30  0)))
    (uml-association :n3 :n2 :style :T3B :name '("http/LAN"      :offset ( 30 23)))
    (uml-node (xy+ n3.cc 280 -65) "Application Server" :width 160 :height 260 :contents t :id :n4)
    (uml-association :n3 :n4 :style :RL3 :name '("Java RMI/LAN" :offset (0 5)))
    (with-subcanvas-of (:n4)
      (uml-artifact (y+ canvas.tc  40) "JoveGL.exe" :height 50)
      (uml-node     (y+ $1.cc      70) "EJB Container" :width 130 :height 50 :id :n5)
      (uml-node     (y+ $1.cc      80) "Oracle DBMS"   :width 130 :height 50 :id :n6)
      (uml-association :n6 :n5 :name '("JDBC" :offset (-10 -5))))))
-->

```kaavio
<!-- expand: UML-DEPLOYMENT-DIAGRAM-SAMPLE -->
```
Figure. UML 配置図の例

<!-- collapse:close -->
※上記サンプルのソースはこちら。

```lisp
<!-- expand: UML-DEPLOYMENT-DIAGRAM-SAMPLE -->
```
<!-- collapse:end -->

${BLANK_PARAGRAPH}

　UML 配置図で使用する構成要素は以下を参照してください。

* [$$](#uml-artifact)
* [$$](#uml-association)
* [$$](#uml-dependency)
* [$$](#uml-frame)
* [$$](#uml-node)
* [$$](#uml-note)

### UML のダイアグラム要素

#### uml-action
<!-- autolink: [$$](#uml-action) -->

　uml-action は UML のアクティビティ図におけるアクションを表記するための図形要素です。
ほぼテキストボックスと同じように使えますが、リンク明示のためのレーキアイコンなどが用意
されています。

<!-- snippet: UML-ACTION-SAMPLE
(diagram (400 220)
  (grid)
  (with-theme (:uml-activity-default)
    (uml-activity-start '(50 40) :id :start)
    (uml-action (x+ start.cc 100) "step1" :id :step1)
    (uml-action (x+ step1.cc 150) "step2" :rake t :id :step2)
    (uml-action (xy+ canvas.cc -60 30) "step3" :id :step3 :width 250 :height 110
        :contents
        ((uml-action (x+ canvas.cc -70) "sub-step1")
         (uml-action (x+ canvas.cc  70) "sub-step2")
         (uml-flow $2.id $1.id)))
    (uml-activity-final (xy+ step3.cc 220 50) :id :final)
    (uml-flow :start :step1)
    (uml-flow :step1 :step2)
    (uml-flow :step2 :step3 :style :BR1)
    (uml-flow :step3 :final)))
-->

```kaavio
<!-- expand: UML-ACTION-SAMPLE -->
```
Figure. uml-action のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-ACTION-SAMPLE -->
```

　`:contents` パラメータと with-subcanvas-of マクロは通常は同じように使えますが、
uml-action では違いが存在します。 `:contents` で内部を描画する場合、uml-action は
テキストを上端付近に描画しますが、 `:contents` を使わずに with-subcanvas-of マクロを
使用した場合、テキストは中央に描画されます。この問題を解決するため、uml-action で
は `:contents t` と指定することでテキストを上端付近に描画させることができます。

　詳細は以下を参照してください。

* uml-action マクロ
* with-uml-action-options マクロ

#### uml-activity-final
<!-- autolink: [$$](#uml-activity-final) -->

　uml-activity-final は UML のアクティビティ図における終了状態を表記するための図形要素です。

<!-- snippet: UML-ACTIVITY-FINAL-SAMPLE
(diagram (200 80)
  (grid)
  (with-theme (:uml-activity-default)
    (uml-action (x+ canvas.cc -40) "action" :id :step)
    (uml-activity-final (x+ $1.cc 100) :id :final)
    (uml-flow :step :final)))
-->

```kaavio
<!-- expand: UML-ACTIVITY-FINAL-SAMPLE -->
```
Figure. uml-activity-final のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-ACTIVITY-FINAL-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-activity-final マクロ
* with-uml-activity-final-options マクロ

#### uml-activity-start
<!-- autolink: [$$](#uml-activity-start) -->

　uml-activity-start は UML のアクティビティ図における開始状態を表記するための図形要素です。

<!-- snippet: UML-ACTIVITY-START-SAMPLE
(diagram (200 80)
  (grid)
  (with-theme (:uml-activity-default)
    (uml-activity-start (x+ canvas.cc -60) :id :start)
    (uml-action (x+ $1.cc 100) "action" :id :step)
    (uml-flow :start :step)))
-->

```kaavio
<!-- expand: UML-ACTIVITY-START-SAMPLE -->
```
Figure. uml-activity-start のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-ACTIVITY-START-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-activity-start マクロ
* with-uml-activity-start-options マクロ

#### uml-actor
<!-- autolink: [$$](#uml-actor) -->

　uml-actor は UML のユースケース図におけるアクターを表記するための図形要素です。

<!-- このダイアグラムは uml-usecase のところでも使われてることに注意。 -->
<!-- snippet: UML-ACTOR-USECASE-SAMPLE
(diagram (240 80)
  (grid)
  (with-theme (:uml-usecase-default)
    (uml-actor   (x+ canvas.cl  30) "actor"   :id :actor)
    (uml-usecase (x+ canvas.cr -60) "usecase" :id :usecase :height 50)
    (uml-association :actor :usecase)))
-->

```kaavio
<!-- expand: UML-ACTOR-USECASE-SAMPLE -->
```
Figure. uml-actor のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-ACTOR-USECASE-SAMPLE -->
```

　第二パラメータで指定するアクターの名称は、ラベル形式での指定になります。

　詳細は以下を参照してください。

* uml-actor マクロ
* with-uml-actor-options マクロ

#### uml-aggregation
<!-- autolink: [$$](#uml-aggregation) -->

　uml-aggregation は UML のクラス図などにおける集約関連を表記するための図形要素です。

<!-- snippet: UML-AGGREGATION-SAMPLE
(diagram (400 80)
  (grid)
  (with-theme (:uml-class-default)
    (uml-class   (x+ canvas.cl  50) "foo" :id :foo)
    (uml-class   (x+ canvas.cr -50) "bar" :id :bar)
    (uml-aggregation :foo :bar
                     :arrow      t
                     :stereotype '("stereotype" :font 9)
                     :name       '("name" :offset (0 5))
                     :role1      "role1"
                     :role2      "role2"
                     :mult1      1
                     :mult2      '(0 . :*))))
-->

```kaavio
<!-- expand: UML-AGGREGATION-SAMPLE -->
```
Figure. uml-aggregation のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-AGGREGATION-SAMPLE -->
```

　必須パラメータは接続元／接続先のみで、それ以外の名前付きパラメータは基本的に 
uml-association と同じです。ただし、矢印は関連先に表示するか否かの指定になる
ため、（uml-association の `:arrows` と異なり）真偽値を指定する `:arrow` パラ
メータとなります。詳細は以下を参照してください。

* uml-association
* uml-aggregation マクロ
* with-uml-aggregation-options マクロ

#### uml-association
<!-- autolink: [$$](#uml-association) -->

　uml-association は UML のクラス図などにおける関連を表記するための図形要素です。

<!-- snippet: UML-ASSOCIATION-SAMPLE
(diagram (400 80)
  (grid)
  (with-theme (:uml-class-default)
    (uml-class   (x+ canvas.cl  50) "foo" :id :foo)
    (uml-class   (x+ canvas.cr -50) "bar" :id :bar)
    (uml-association :foo :bar
                     :arrows     1
                     :stereotype '("stereotype" :font 9)
                     :name       '("name" :offset (0 5))
                     :role1      "role1"
                     :role2      "role2"
                     :mult1      1
                     :mult2      '(0 . :*))))
-->

```kaavio
<!-- expand: UML-ASSOCIATION-SAMPLE -->
```
Figure. uml-association のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-ASSOCIATION-SAMPLE -->
```

　uml-association はコネクタとほぼ同じように使えますが、終端に表示する情報などを
指定可能になっています。矢印に関しては `:arrows` パラメータで 0,1,2 のいずれかを
指定します。これは、矢印無しか、接続先のみか、両端かを意味します。また、ロール情
報や多重度情報を `:role1 :role2 :mult1 :mult2` といった名前付きパラメータで指定
します。詳細は以下を参照してください。

* uml-role-info
* uml-multiplicity-info
* uml-association マクロ
* with-uml-association-options マクロ

#### uml-artifact
<!-- autolink: [$$](#uml-artifact) -->

　uml-artifact は UML の配置図における生成物を表記するための図形要素です。

<!-- snippet: UML-ARTIFACT-SAMPLE
(diagram (240 120)
  (grid)
  (drop-shadow)
  (with-theme (:uml-deployment-default)
    (uml-artifact canvas.cc "sample.exe"
                  :width 120 :height 50 :filter :drop-shadow)))
-->

```kaavio
<!-- expand: UML-ARTIFACT-SAMPLE -->
```
Figure. uml-artifact のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-ARTIFACT-SAMPLE -->
```

　上記では指定されていませんが、右上に表示されるアイコンの位置などの調整には `:icon`
パラメータを指定します（[$@ 節](#uml-icon-setting)参照）。詳細は以下を参照してください。

* uml-icon-setting
* uml-artifact マクロ
* with-uml-artifact-options マクロ

#### uml-class
<!-- autolink: [$$](#uml-class) -->

　uml-class は UML のクラス図におけるクラスを表記するための図形要素です。 クラス
名だけの単純なボックスでも、属性・操作・責務などの記述を伴ったボックスでも描画で
きます。

<!-- snippet: UML-CLASS-SAMPLE
(diagram (500 180)
  (grid)
  (with-theme (:uml-class-default)
    (uml-class (xy+ canvas.tl  60 40) "foo")
    (uml-class (x+  $1.cc     120) "abstract~%class" :abstract t)
    (uml-class (y+  $2.cc     100)   "active~%class" :active t)
    (uml-class (y+  $2.cc     100) "template~%class"
               :width 90 :height 40 :template " T ")
    (uml-class (xy+ $4.cc 60   50) "bar" :width 90
                                   :keyword '("keyword" :font 9))
    (uml-class (x+  canvas.cc 120) "class with~%additional boxes"
               :width 160
               :attributes
               ((:private "m_status" :type :uint32_t))
               :operations
               ((:public "Run" :type :int)
                (:public "Abort"))
               :responsibilities
               "blah blah blah~%blah blah blah~%blah blah blah")))
-->

```kaavio
<!-- expand: UML-CLASS-SAMPLE -->
```
Figure. uml-class のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-CLASS-SAMPLE -->
```

　必須のパラメータは座標とクラス名だけです。それ以外は名前付きパラメータで、抽象
クラスやクラステンプレートなどの指定を行ないます。属性や操作の指定は `:attributes` と 
`:operations` でクラス属性情報またはクラス操作情報のリストを指定します。これらに
ついては、uml-class-attribute および uml-class-operation を参照してください。

　詳細は以下を参照してください。

* uml-class-attribute
* uml-class-operation-param
* uml-class-operation
* uml-class マクロ
* with-uml-class-options マクロ

#### uml-class-attribute
<!-- autolink: [$$](#uml-class-attribute) -->

　uml-class-attribute は UML のクラス表記における属性欄を記述するための、可視性、
名前、型、多重度などからなる情報です。以下に例を示します。

<!-- snippet: UML-CLASS-ATTRIBUTE-SAMPLE
(diagram (300 150)
  (grid)
  (with-theme (:uml-class-default)
    (uml-class canvas.cc "foo"
               :width 160
               :attributes
               (:<<section1>>
                (:private "m_lastMsg" :type :string)
                :etc
                :<<section2>>
                (:private "m_status" :type :StatusT)
                (:private "m_data"   :type :uint32_t :multiplicity 10)
                :etc))))
-->

```kaavio
<!-- expand: UML-CLASS-ATTRIBUTE-SAMPLE -->
```
Figure. uml-class-attribute のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-CLASS-ATTRIBUTE-SAMPLE -->
```

　uml-class マクロの `:attributes` パラメータには、属性情報のボックスに記載する
情報をリストで指定します。このリストの要素は以下のいずれかです。

* キーワードを示すキーワードシンボル
    * これは文字列 `<<` で始まり、文字列 `>>` で終わる名前を持ちます
* 属性情報を表現するリスト
    * これは make-uml-class-attribute 関数に渡されます
* 省略を示すキーワードシンボル `:etc`
    * これは「ここに記載されているものが全てではない」ことを表現する `...` になります

　詳細は以下を参照してください。

* make-uml-class-attribute 関数

#### uml-class-operation
<!-- autolink: [$$](#uml-class-operation) -->

　uml-class-operation は UML のクラス表記における操作欄を記述するための、可視性、
名前、パラメータ、型などからなる情報です。以下に例を示します。

<!-- snippet: UML-CLASS-OPERATION-SAMPLE
(diagram (300 170)
  (grid)
  (with-theme (:uml-class-default)
    (uml-class canvas.cc "foo"
               :width 250
               :operations
               (:<<constructor>>
                (:public "foo")
                (:public "foo" :parameters ("foo&"))
                :<<accessors>>
                (:public "GetData" :type :uint32_t :property :frozen)
                :<<operations>>
                (:public "DoSomething" :parameters (:uint32_t) :type :boolean)
                :etc))))
-->

```kaavio
<!-- expand: UML-CLASS-OPERATION-SAMPLE -->
```
Figure. uml-class-operation のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-CLASS-OPERATION-SAMPLE -->
```

　uml-class マクロの `:operations` パラメータには、操作情報のボックスに記載する
情報をリストで指定します。このリストの要素は以下のいずれかです。

* キーワードを示すキーワードシンボル
    * これは文字列 `<<` で始まり、文字列 `>>` で終わる名前を持ちます
* 操作情報を表現するリスト
    * これは make-uml-class-operation 関数に渡されます
* 省略を示すキーワードシンボル `:etc`
    * これは「ここに記載されているものが全てではない」ことを表現する `...` になります

　詳細は以下を参照してください。

* uml-class-operation-param
* make-uml-class-operation 関数

#### uml-class-operation-param
<!-- autolink: [$$](#uml-class-operation-param) -->

　uml-class-operation-param は UML のクラス操作情報におけるパラメータを記述するための、
名前、型、デフォルト値などからなる情報です。以下に例を示します。

<!-- snippet: UML-CLASS-OPERATION-PARAM-SAMPLE
(diagram (400 100)
  (grid)
  (with-theme (:uml-class-default)
    (uml-class canvas.cc "foo"
               :width 320
               :operations
               ((:public "Sample" :type :bool
                 :parameters
                 ((:prm1 :type :string)
                  (:prm2 :type :int :default 0)
                  :etc))))))
-->

```kaavio
<!-- expand: UML-CLASS-OPERATION-PARAM-SAMPLE -->
```
Figure. uml-class-operation-param のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-CLASS-OPERATION-PARAM-SAMPLE -->
```

　make-uml-class-operation 関数の `:parameters` パラメータには、操作情報のパラメータ
部分に記載する情報をリストで指定します。このリストの要素は以下のいずれかです。

* パラメータ情報を表現するリスト
    * これは make-uml-class-operation-param 関数に渡されます
* 省略を示すキーワードシンボル `:etc`
    * これは「ここに記載されているものが全てではない」ことを表現する `...` になります

　詳細は以下を参照してください。

* uml-class-operation
* make-uml-class-operation-param 関数

#### uml-component
<!-- autolink: [$$](#uml-component) -->

　uml-component は UML のコンポーネント図などにおけるコンポーネントを表記するための
図形要素です。

<!-- snippet: UML-COMPONENT-SAMPLE
(diagram (240 120)
  (grid)
  (drop-shadow)
  (with-theme (:uml-component-default)
    (uml-component canvas.cc "somewhat.dll"
                   :width 120 :height 60 :filter :drop-shadow)))
-->

```kaavio
<!-- expand: UML-COMPONENT-SAMPLE -->
```
Figure. uml-component のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-COMPONENT-SAMPLE -->
```

　上記では指定されていませんが、右上に表示されるアイコンの位置などの調整には `:icon`
パラメータを指定します（[$@ 節](#uml-icon-setting)参照）。詳細は以下を参照してください。

* uml-icon-setting
* uml-component マクロ
* with-uml-component-options マクロ

#### uml-composition
<!-- autolink: [$$](#uml-composition) -->

　uml-composition は UML のクラス図などにおけるコンポジション関連を表記するための図形要素です。

<!-- snippet: UML-COMPOSITION-SAMPLE
(diagram (400 80)
  (grid)
  (with-theme (:uml-class-default)
    (uml-class   (x+ canvas.cl  50) "foo" :id :foo)
    (uml-class   (x+ canvas.cr -50) "bar" :id :bar)
    (uml-composition :foo :bar
                     :arrow      t
                     :stereotype '("stereotype" :font 9)
                     :name       '("name" :offset (0 5))
                     :role1      "role1"
                     :role2      "role2"
                     :mult1      1
                     :mult2      '(0 . :*))))
-->

```kaavio
<!-- expand: UML-COMPOSITION-SAMPLE -->
```
Figure. uml-composition のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-COMPOSITION-SAMPLE -->
```

　必須パラメータは接続元／接続先のみで、それ以外の名前付きパラメータは基本的に 
uml-association と同じです。ただし、矢印は関連先に表示するか否かの指定になる
ため、（uml-association の `:arrows` と異なり）真偽値を指定する `:arrow` パラ
メータとなります。詳細は以下を参照してください。

* uml-association
* uml-composition マクロ
* with-uml-composition-options マクロ

#### uml-connector
<!-- autolink: [$$](#uml-connector) -->

　uml-connector は UML のアクティビティ図における[コネクタ](#uml-connector)を表記
するための図形要素です。あまり使用されませんが、接続線がどうしても交差してしまう
場合には便利です。

<!-- snippet: UML-CONNECTOR-SAMPLE
(diagram (300 120)
  (grid)
  (with-theme (:uml-activity-default)
    (uml-action (xy+ canvas.center -80 -30) "action1" :id :step1)
    (uml-action (xy+ canvas.center  80  30) "action2" :id :step2)
    (uml-connector (x+ step1.center  130)
                   (x+ step2.center -130) :warp-a :name :A)
    (uml-flow :step1  :warp-a)
    (uml-flow :warp-a :step2)))
-->

```kaavio
<!-- expand: UML-CONNECTOR-SAMPLE -->
```
Figure. uml-connector のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-CONNECTOR-SAMPLE -->
```

　uml-connector はかなり特殊な図形要素で、２点を指定します。指定された２ケ所に同じ円形の
マーカーが描画され、uml-flow などの接続線で `from` と `to` のどちらに指定されたかで
接続先が変わります。詳細は以下を参照してください。

* uml-connector マクロ
* with-uml-connector-options マクロ

#### uml-decision
<!-- autolink: [$$](#uml-decision) -->

　uml-decision は UML のアクティビティ図における判断を表記するための図形要素です。
以下の例では左端の check status というテキストを伴った要素が uml-decision です。
テキストを指定しなければ uml-merge と同じようにひし形が描画されます。

<!-- snippet: UML-DECISION-MERGE-SAMPLE
(diagram (400 150)
  (grid)
  (drop-shadow)
  (with-theme (:uml-activity-default)
    (with-options (:filter :drop-shadow)
      (uml-decision (x+ canvas.cc -120)
                    :text "check~%status" :width 80 :height 40 :id :check)
      (uml-action   (xy+ canvas.cc 40 -40) "action1" :id :act1)
      (uml-action   (xy+ canvas.cc 40  40) "action2" :id :act2)
      (uml-merge    (x+ canvas.cc  150) :id :merge))
    (uml-flow :check :act1 :style :TL :spec "OK")
    (uml-flow :check :act2 :style :BL :spec "NG")
    (uml-flow :act1 :merge :style :RT)
    (uml-flow :act2 :merge :style :RB)))
-->

```kaavio
<!-- expand: UML-DECISION-MERGE-SAMPLE -->
```
Figure. uml-decision のサンプル

　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-DECISION-MERGE-SAMPLE -->
```

　uml-decision はテキストの有無で形状が変化します。そして、どちらの場合でも width と 
height の指定、または with-uml-decision-merge-options マクロで指定されるそれらの
デフォルト値のみでサイズを決定します。つまり、テキスト内容から（テキストボックスのように）
サイズを自動決定したりはしません。このことは、width / height のデフォルト値としてテキ
ストありの場合と無しの場合のどちらを想定した値を設定すべきか、という問題を引き起こします。
ひとまずのところ、デフォルトとしてはテキスト無しのサイズを指定しておき、テキストを指定
する場合は width / height を明示的に指定することをお勧めします。

　詳細は以下を参照してください。

* uml-merge マクロ
* uml-decision マクロ
* with-uml-decision-merge-options マクロ

#### uml-dependency
<!-- autolink: [$$](#uml-dependency) -->

　uml-dependency は UML の各種ダイアグラムにおける依存関係を表記するための図形要素です。

<!-- snippet: UML-DEPENDENCY-SAMPLE
(diagram (400 80)
  (grid)
  (with-theme (:uml-class-default)
    (uml-class   (x+ canvas.cl  50) "foo" :id :foo)
    (uml-class   (x+ canvas.cr -50) "bar" :id :bar)
    (uml-dependency :foo :bar
                    :stereotype '("stereotype" :font 9)
                    :name       '("name" :offset (0 5)))))
-->

```kaavio
<!-- expand: UML-DEPENDENCY-SAMPLE -->
```
Figure. uml-dependency のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-DEPENDENCY-SAMPLE -->
```

　必須パラメータは接続元／接続先のみで、それ以外は名前付きパラメータです。コネク
タとほぼ同じように使えます。詳細は以下を参照してください。

* uml-dependency マクロ
* with-uml-dependency-options マクロ

#### uml-expansion-region
<!-- autolink: [$$](#uml-expansion-region) -->

　uml-expansion-region は UML のアクティビティ図における拡張領域を表記するための
図形要素です。

<!-- snippet: UML-EXPANSION-REGION-SAMPLE
(diagram (300 200)
  (grid)
  (drop-shadow)
  (with-theme (:uml-activity-default)
    (with-options (:filter :drop-shadow)
      (uml-expansion-region canvas.center 260 160 :keyword '(:concurrent :font 10)))))
-->

```kaavio
<!-- expand: UML-EXPANSION-REGION-SAMPLE -->
```
Figure. uml-expansion-region のサンプル

　上記の作図は以下のコードで行なっています。 `stereotype` または `keyword` パラメータ
を指定した場合、デフォルトでは左上に表示されます。この位置は `offset` パラメータで調整
することができます。

* ${{TODO}{上記の調整方法について改善タスクが起票されている。}}

```lisp
<!-- expand: UML-EXPANSION-REGION-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-expansion-region マクロ
* with-uml-expansion-region-options マクロ

#### uml-flow-final
<!-- autolink: [$$](#uml-flow-final) -->

　uml-flow-final は UML のアクティビティ図におけるフロー終了を表記するための図形要素です。

<!-- snippet: UML-FLOW-FINAL-SAMPLE
(diagram (200 80)
  (grid)
  (with-theme (:uml-activity-default)
    (uml-action (x+ canvas.center -40) "action" :id :step)
    (uml-flow-final (x+ $1.center 100) :id :final)
    (uml-flow :step :final)))
-->

```kaavio
<!-- expand: UML-FLOW-FINAL-SAMPLE -->
```
Figure. uml-flow-final のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-FLOW-FINAL-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-flow-final マクロ
* with-uml-flow-final-options マクロ

#### uml-flow
<!-- autolink: [$$](#uml-flow) -->

　uml-flow は UML のアクティビティ図におけるアクション間の遷移を表記するための
図形要素です。

<!-- snippet: UML-FLOW-SAMPLE
(diagram (400 100)
  (grid)
  (drop-shadow)
  (with-theme (:uml-activity-default)
    (with-uml-action-options (:filter :drop-shadow)
      (uml-action '( 60 50) "step1" :id :step1)
      (uml-action '(340 50) "step2" :id :step2)
      (uml-flow :step1 :step2 :spec '(:guard :idle :action "act()")))))
-->

```kaavio
<!-- expand: UML-FLOW-SAMPLE -->
```
Figure. uml-flow のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-FLOW-SAMPLE -->
```

　必須パラメータは接続元／接続先の指定のみで、それ以外は名前付きパラメータです。
そのうち、 `:stereotype :keyword :spec` 以外はほぼコネクタと同じように使えます。
`:spec` は遷移のガード条件やアクションを指定するためのものです。これらについて
の詳細は以下を参照してください。

* uml-flow-spec
* uml-flow マクロ
* with-uml-flow-options マクロ

#### uml-flow-spec
<!-- autolink: [$$](#uml-flow-spec) -->

　uml-flow-spec は UML のアクティビティ図におけるフロー仕様を記述するための、
ガード条件、アクションなどからなる情報です。以下に例を示します。

<!-- snippet: UML-FLOW-SPEC-SAMPLE
(diagram (400 190)
  (grid)
  (drop-shadow)
  (with-theme (:uml-activity-default)
    (with-uml-action-options (:filter :drop-shadow)
      (uml-action '( 60  40) "action1" :id :act1)
      (uml-action '(340  40) "action2" :id :act2)
      (uml-flow :act1 :act2 :spec :on-idle)
      (uml-action '( 60 100) "action3" :id :act3)
      (uml-action '(340 150) "action4" :id :act4)
      (uml-flow :act3 :act4
                :style :RL
                :spec '(:guard  "guard"
                        :action "action()"
                        :offset (-90 -35)
                        :font   (:fill :brown :size 9))))))
-->

```kaavio
<!-- expand: UML-FLOW-SPEC-SAMPLE -->
```
Figure. uml-flow-spec のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-FLOW-SPEC-SAMPLE -->
```

　アクティビティ図におけるフロー仕様は、uml-flow マクロに渡す `:spec` パラメータで
指定します。このパラメータは make-uml-flow-spec 関数に渡されます。単一のキーワード
シンボルや文字列を与えた場合、ガード条件として扱われます。リストを渡せばガード条件
やアクション、表示位置の調整やフォント指定を行なうことができます。

　詳細は以下を参照してください。

* uml-flow
* make-uml-flow-spec 関数

#### uml-fork
<!-- autolink: [$$](#uml-fork) -->

　uml-fork は UML のアクティビティ図におけるフォークを表記するための図形要素です。

<!-- snippet: UML-FORK-SAMPLE
(diagram (340 150)
  (grid)
  (drop-shadow)
  (with-theme (:uml-activity-default)
    (with-options (:filter :drop-shadow)
      (uml-action (x+ canvas.cc -100)"step1" :id :step1)
      (uml-fork   canvas.cc :h :id :fork)
      (uml-action (xy+ canvas.cc 100 -40) "step2" :id :step2)
      (uml-action (xy+ canvas.cc 100  40) "step3" :id :step3))
    (uml-flow :step1  :fork)
    (uml-flow fork.R1 :step2)
    (uml-flow fork.R3 :step3)))
-->

```kaavio
<!-- expand: UML-FORK-SAMPLE -->
```
Figure. uml-fork のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-FORK-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-fork マクロ
* with-uml-fork-join-options マクロ

#### uml-frame
<!-- autolink: [$$](#uml-frame) -->

　uml-frame は UML において汎用的に使用される区分要素です。ダイアグラム全体を包む場合も
ありますし、別図面へのリンクを提示して詳細を省略したりします。

<!-- snippet: UML-FRAME-SAMPLE
(diagram (300 150)
  (grid)
  (drop-shadow)
  (with-theme (:uml-activity-default)
    (uml-frame canvas.center 260 110 "this is frame." :filter :drop-shadow)
    (with-subcanvas-of ($1.id)
      (uml-action (y+ canvas.center 10) "action" :id :act1)
      (uml-activity-start (x+ $1.center -100) :id :start)
      (uml-activity-final (x+ $2.center  100) :id :final)))
  (connector :start :act1 :end2 :arrow)
  (connector :act1 :final :end2 :arrow))
-->

```kaavio
<!-- expand: UML-FRAME-SAMPLE -->
```
Figure. uml-frame のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-FRAME-SAMPLE -->
```

<!-- snippet: UML-FRAME-SAMPLE-2
(diagram (700 220)
  (grid)
  (with-theme (:uml-component-default)
    ;; frame1
    (uml-frame (x+ canvas.cc -180) 300 180 "frame1" :id :f1 :fragments 90)
    (with-uml-frame-fragment (:f1 0)
      (text (y+ canvas.tc 15) "[when foo]" :align :center))
    (with-uml-frame-fragment (:f1 1)
      (text (y+ canvas.tc 15) "[unless foo]" :align :center))
    ;; frame2
    (uml-frame (x+ canvas.cc 180) 300 180 "frame2" :id :f2 :fragments '(60 60))
    (with-uml-frame-fragment (:f2 0)
      (text (y+ canvas.tc 15) "[condition1]" :align :center))
    (with-uml-frame-fragment (:f2 1)
      (text (y+ canvas.tc 15) "[condition2]" :align :center))
    (with-uml-frame-fragment (:f2 2)
      (text (y+ canvas.tc 15) "[condition3]" :align :center))))
-->

　`:fragments` パラメータを使用することで、フレームを複数の領域に分割することができます。
以下に例を示します。分割された個々の領域には、with-uml-frame-fragment マクロを使って
アクセスできます。

```kaavio
<!-- expand: UML-FRAME-SAMPLE-2 -->
```
Figure. fragments パラメータを使った uml-frame のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-FRAME-SAMPLE-2 -->
```


　詳細は以下を参照してください。

* uml-frame マクロ
* with-uml-frame-fragment マクロ
* with-uml-frame-options マクロ

#### uml-generalization
<!-- autolink: [$$](#uml-generalization) -->

　uml-generalization は UML のクラス図などにおける汎化関係を表記するための図形要素です。

<!-- snippet: UML-GENERALIZATION-SAMPLE
(diagram (400 80)
  (grid)
  (with-theme (:uml-class-default)
    (uml-class   (x+ canvas.cl  50) "foo" :id :foo)
    (uml-class   (x+ canvas.cr -50) "bar" :id :bar)
    (uml-generalization :foo :bar
                        :stereotype '("stereotype" :font 9)
                        :name       '("name" :offset (0 5)))))
-->

```kaavio
<!-- expand: UML-GENERALIZATION-SAMPLE -->
```
Figure. uml-generalization のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-GENERALIZATION-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-generalization マクロ
* with-uml-generalization-options マクロ

#### uml-icon-setting
<!-- autolink: [$$](#uml-icon-setting) -->

　uml-icon-setting は UML の描画要素において使用されるアイコンの設定をするための要素です。
以下に例を示します。

<!-- snippet: UML-ICON-SETTING-SAMPLE
(diagram (400 110)
  (grid)
  (with-theme (:uml-component-default)
    (uml-component (x+ canvas.CL  100) "component" :width 120)
    (uml-component (x+ canvas.CR -100) "component" :width 120
                   :icon '(:fill :lightcyan :stroke :navy
                           :size 16 :pivot :TL :offset (17 13)))))
-->

```kaavio
<!-- expand: UML-ICON-SETTING-SAMPLE -->
```
Figure. uml-icon-setting のサンプル

　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-ICON-SETTING-SAMPLE -->
```

　左の uml-component はデフォルトのアイコン設定で描画されていますが、右の uml-component 
は `:icon` パラメータによってアイコン設定を指定されています。指定できるのは塗り潰しと
ストローク、サイズ、位置です。詳細は以下を参照してください。

* make-uml-icon-setting 関数

#### uml-interface
<!-- autolink: [$$](#uml-interface) -->

　uml-interface は UML のクラス図などにおけるインターフェースを表記するための図形要素です。

<!-- snippet: UML-INTERFACE-SAMPLE
(diagram (360 100)
  (grid)
  (with-theme (:uml-class-default)
    (uml-class     (x+ canvas.CL  100) "class")
    (uml-interface (x+ canvas.CR -100) "interface")
    (uml-association $2.id $1.id)))
-->

```kaavio
<!-- expand: UML-INTERFACE-SAMPLE -->
```
Figure. uml-interface のサンプル

　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-INTERFACE-SAMPLE -->
```

詳細は以下を参照してください。

* uml-interface マクロ

#### uml-interface-request
<!-- autolink: [$$](#uml-interface-request) -->

　uml-interface-request は UML のクラス図などにおけるインターフェースのソケットを表記する
ための図形要素です。

<!-- snippet: UML-INTERFACE-REQUEST-SAMPLE
(diagram (360 150)
  (grid)
  (with-theme (:uml-class-default)
    (uml-class     (xy+ canvas.TL 60 40) "foo" :id :foo)
    (uml-class     (x+  foo.cc 240)      "bar" :id :bar)
    (uml-interface (x+  bar.cc -110)     "baz" :id :baz)
    (uml-association       :bar :baz)
    (uml-interface-request :foo :baz)
    (uml-interface-request :foo (xy+ foo.cc 120 60)
                           :style :BL :name '("quux" :offset (0 25)))))
-->

```kaavio
<!-- expand: UML-INTERFACE-REQUEST-SAMPLE -->
```
Figure. uml-interface-request のサンプル

　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-INTERFACE-REQUEST-SAMPLE -->
```

　uml-interface-request には 2 つの使い方があります。ひとつは要求されるインターフェース
がuml-interface で描画されている場合にそれを接続先として指定するもので、上記の例ではクラス 
`foo` からインターフェース `baz` の要求が描かれています。

　もうひとつは接続先として座標を指定することでソケットだけを描画するもので、上記の例では
クラス `foo` からインターフェース `quux` を要求するソケットが描画されています。この場合、
インターフェース名は `:name` パラメータで指定することになります。いずれの場合も、
uml-interface-request は実質的にコネクタなので、 `:style` や `:spacing` パラメータ
で接続線の折れ曲がり方を調整することができます。

　詳細は以下を参照してください。

* uml-interface-request マクロ

#### uml-join
<!-- autolink: [$$](#uml-join) -->

　uml-join は UML のアクティビティ図におけるフォークを表記するための図形要素です。見た目は
[$$](#uml-fork) と同じですが、ジョイン仕様を指定することができます。

<!-- snippet: UML-JOIN-SAMPLE
(diagram (340 150)
  (grid)
  (drop-shadow)
  (with-theme (:uml-activity-default)
    (with-options (:filter :drop-shadow)
      (uml-action (xy+ canvas.cc -100 -40) "step1" :id :step1)
      (uml-action (xy+ canvas.cc -100  40) "step2" :id :step2)
      (uml-join canvas.cc :h :id :join
                :spec '("{joinSpec=blah blah}" :offset (30 8) :font 10))
      (uml-action (x+ canvas.cc 100) "step3" :id :step3))
    (uml-flow :step1 join.L1)
    (uml-flow :step2 join.L3)
    (uml-flow :join  :step3)))
-->

```kaavio
<!-- expand: UML-JOIN-SAMPLE -->
```
Figure. uml-join のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-JOIN-SAMPLE -->
```

　ジョイン仕様は `:spec` パラメータでラベル形式で指定します。詳細は以下を参照してください。

* uml-join マクロ
* with-uml-fork-join-options マクロ

#### uml-merge
<!-- autolink: [$$](#uml-merge) -->

　uml-merge は UML のアクティビティ図において判断による分岐の終了を表記するための
図形要素です。以下の例では右端のひし形の要素が uml-merge です。uml-decision とは
異なり、テキストを指定することはできません。

```kaavio
<!-- expand: UML-DECISION-MERGE-SAMPLE -->
```
Figure. uml-merge のサンプル

　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-DECISION-MERGE-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-decision マクロ
* uml-merge マクロ
* with-uml-decision-merge-options マクロ

#### uml-multiplicity-info
<!-- autolink: [$$](#uml-multiplicity-info) -->

* ${{TODO}{uml-class では uml-multiplicity-info の offset が利用されていない疑いあり。}}

　uml-multiplicity-info は UML のクラスや関連において使用される多重度の記述を
するための要素です。以下に例を示します。

<!-- snippet: UML-MULTIPLICITY-INFO-SAMPLE
(diagram (300 140)
  (grid)
  (drop-shadow)
  (with-theme (:uml-class-default)
    (with-uml-class-options (:width 80 :height 40 :filter :drop-shadow)
      (uml-class (xy+ canvas.tl  60  40) "app" :multiplicity 1)
      (uml-class (xy+ canvas.bl  60 -40) "foo")
      (uml-class (xy+ canvas.br -60 -40) "bar")
      (uml-association $2.id $1.id :arrows 1 :mult2 '(:min 2 :max :* :font 9)))))
-->

```kaavio
<!-- expand: UML-MULTIPLICITY-INFO-SAMPLE -->
```
Figure. uml-multiplicity-info のサンプル

　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-MULTIPLICITY-INFO-SAMPLE -->
```

　uml-class では、多重度は `:multiplicity` パラメータで指定します。関連などでは
両端に多重度の指定がありうるため、 `:mult1 :mult2` で指定を行ないます。いずれに
おいても、指定されたパラメータは make-uml-multiplicity 関数に渡されます。詳細は
以下を参照してください。

* make-uml-multiplicity 関数

#### uml-node
<!-- autolink: [$$](#uml-node) -->

　uml-node は UML の配置図などにおけるノードを表記するための図形要素です。

<!-- snippet: UML-NODE-SAMPLE
(diagram (300 160)
  (grid)
  (drop-shadow)
  (with-theme (:uml-deployment-default)
    (uml-node (x+ canvas.cl  60) "client")
    (uml-node (x+ canvas.cr -90) "server" :width 120 :height 120
      :contents
      ((paragraph (xy+ canvas.tl 5 5)
                  "* Web server~%* Postgre SQL"
                  :align :left :valign :top)))))
-->

```kaavio
<!-- expand: UML-NODE-SAMPLE -->
```
Figure. uml-node のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-NODE-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-node マクロ
* with-uml-node-options マクロ

#### uml-note
<!-- autolink: [$$](#uml-note) -->

　uml-note は UML において汎用的にノートを付加させる図形要素です。ほぼメモと同じように
使えますが、targets パラメータによって複数の図形要素に接続することが可能です。

<!-- snippet: UML-NOTE-SAMPLE
(diagram (400 170)
  (grid)
  (with-theme (:uml-class-default)
    (uml-class '( 80 45) "Foo" :id :foo)
    (uml-class '(300 25) "Bar" :id :bar)
    (uml-note (xy+ bar.center -10 80) 120 60
              "test note."
              :targets '(:foo :bar) :keyword "memo")
    (uml-note (xy+ foo.center 20 80) 160 60
              "this is spec memo.~%multi line is OK."
              :targets :foo :keyword "spec")))
-->

```kaavio
<!-- expand: UML-NOTE-SAMPLE -->
```
Figure. uml-note のサンプル


　上記の作図は以下のコードで行なっています。targets パラメータは、単一の要素だけを指定する
場合はリストである必要はありません。また、図面中で uml-note のスタイルを統一するために 
with-uml-note-options マクロを使用しています。

```lisp
<!-- expand: UML-NOTE-SAMPLE -->
```

　version 0.022 より、targets パラメータに指定する接続先として、図形要素の ID だけでなく
座標の直接指定も可能になりました。図形要素の ID を指定した場合はコネクタで `:style :CC` を
指定したイメージで接続されますが、座標の直接指定では任意の位置に接続線をひくことが可能になり
ます。

　詳細は以下を参照してください。

* uml-note マクロ
* with-uml-note-options マクロ

#### uml-package
<!-- autolink: [$$](#uml-package) -->

　uml-package は UML のパッケージ図などにおけるパッケージを表記するための図形要素です。

<!-- snippet: UML-PACKAGE-SAMPLE
(diagram (300 160)
  (grid)
  (drop-shadow)
  (with-theme (:uml-package-default)
    (uml-package (xy+ canvas.cl  60 10) "app")
    (uml-package (xy+ canvas.cr -90 10) "lib" :width 120 :height 100
      :contents
      ((paragraph (xy+ canvas.tl 5 10)
                  "* pattern match~%* multi threads"
                  :align :left :valign :top)))))
-->

```kaavio
<!-- expand: UML-PACKAGE-SAMPLE -->
```
Figure. uml-package のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-PACKAGE-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-package マクロ
* with-uml-package-options マクロ

#### uml-partition
<!-- autolink: [$$](#uml-partition) -->

　uml-partition は UML のアクティビティ図における区画（パーティション）を表記するための
図形要素です。

<!-- snippet: UML-PARTITION-SAMPLE
(diagram (560 300)
  (grid)
  (with-theme (:uml-activity-default)
    (uml-partition canvas.center '(("FrontEnd" 140)
                                   ("BackEnd"  140)) 540 :id :lanes)
    (with-uml-partition-lane (:lanes "FrontEnd")
      (uml-activity-start (x+ canvas.left 30)  :id :start)
      (uml-action (x+ $1.center 100) "action1" :id :act1)
      (uml-action (x+ $1.center 130) "action3" :id :act3)
      (uml-merge  (x+ $1.center 120)           :id :merge)
      (uml-activity-final (x+ $1.center 90)   :id :final))
    (with-uml-partition-lane (:lanes "BackEnd")
      (uml-action   (y+ act1.center 140)  "action2" :id :act2)
      (uml-decision (y+ act3.center 140)            :id :decision)
      (uml-action   (y+ merge.center 140) "action4" :id :act4))
    (labels ((route (lst)
               (when (and (car lst) (cadr lst))
                 (uml-flow (car lst) (cadr lst))
                 (route (cdr lst)))))
      (route '(:decision :act4 :merge))
      (route '(:start :act1 :act2 :decision :act3 :merge :final)))))
-->

```kaavio
<!-- expand: UML-PARTITION-SAMPLE -->
```
Figure. uml-partition のサンプル

　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-PARTITION-SAMPLE -->
```

${BLANK_PARAGRAPH}

　uml-partition のパラメータとして必須なのは `center rows cols` の３つで、 `rows cols` の指定
の仕方で垂直・水平方向のスイムレーンとマトリクス状の区画を作成することができます。以下に例を示します。

<!-- snippet: UML-PARTITION-SAMPLE-2
(diagram (600 140)
  (grid)
  (with-uml-partition-options (:stroke :black :fill :azure :lines :mid)
    (with-subcanvas ('( 0 0) 200 140)
      (uml-partition canvas.center 120 '((A 60) (B 60) (C 60))))
    (with-subcanvas ('(200 0) 200 140)
      (uml-partition canvas.center '((X 40) (Y 40) (Z 40)) 180))
    (with-subcanvas ('(400 0) 200 140)
      (uml-partition canvas.center
                     '((X 30) (Y 30) (Z 30)) '((A 50) (B 50) (C 50))))))
-->

```kaavio
<!-- expand: UML-PARTITION-SAMPLE-2 -->
```
Figure. uml-partition のサンプル - 2

　上記の作図は以下のコードで行なっています。左の縦方向のスイムレーンでは、 `cols` に `'((A 60) (B 60) (C 60))` を、 
`rows` に `120` を指定しています。中央の横方向のスイムレーンでは、 `cols` に `180` を、 `rows` に 
`'((X 40) (Y 40) (Z 40))` を指定しています。このように `rows cols` の一方を数値にして他方をリストにすると
縦または横のスイムレーンになります。両方をリストにするとマトリクス状の区画になります。

```lisp
<!-- expand: UML-PARTITION-SAMPLE-2 -->
```

　注意が必要なのは、区画の全体サイズを決めるルールです。

* スイムレーンになる場合、
    * `rows` または `cols` に数値を指定した側は、その値がそのまま幅または高さになります
    * レーンの名前部分の領域は、 `header` で指定されたサイズが上記の中で確保されます
    * `rows` または `cols` にリストを指定した側はその幅の合計が幅または高さになります
* マトリクスになる場合、
    * `rows` または `cols` に指定したリストに含まれる幅の合計に `header` の指定値を加えたものが幅または高さになります

${BLANK_PARAGRAPH}

　uml-partition はテーブルをベースに作成されていますが、描画される「罫線」は `lines` パラメータに
よってカスタマイズすることができます。指定する値は `:min :mid :max` のいずれかです。区画の形状別の
サンプルを以下に示します。

```kaavio
(diagram (630 450)
; (grid)
  (with-textbox-options  (:align :center :font '(:fill :brown :size 20))
    (textbox '(130  20) ":lines :min" :no-frame t)
    (textbox '(330  20) ":lines :mid" :no-frame t)
    (textbox '(530  20) ":lines :max" :no-frame t)
    (textbox '( 20 100) "vertical"    :no-frame t :rotate -90)
    (textbox '( 20 240) "horizontal"  :no-frame t :rotate -90)
    (textbox '( 20 380) "matrix"      :no-frame t :rotate -90))
  (with-uml-partition-options (:stroke :black :fill :azure)
    (labels ((vertical (x lines)
               (with-subcanvas (`(,x 30) 200 140)
                 (uml-partition canvas.center 120 '((A 60) (B 60) (C 60)) :lines lines)))
             (horizontal (x lines)
               (with-subcanvas (`(,x 170) 200 140)
                 (uml-partition canvas.center '((X 40) (Y 40) (Z 40)) 180 :lines lines)))
             (matrix (x lines)
               (with-subcanvas (`(,x 310) 200 140)
                 (uml-partition canvas.center '((X 30) (Y 30) (Z 30))
                                              '((A 50) (B 50) (C 50)) :lines lines))))
      (dolist (func (list #'vertical #'horizontal #'matrix))
        (map nil func '(30 230 430) '(:min :mid :max))))))
```
Figure. uml-partition における lines パラメータのサンプル

　詳細は以下を参照してください。

* uml-partition マクロ
* with-uml-partition-lane マクロ
* with-uml-partition-options マクロ

#### uml-pin
<!-- autolink: [$$](#uml-pin) -->

　uml-pin は UML のアクティビティ図におけるピンを表記するための図形要素です。

<!-- snippet: UML-PIN-SAMPLE
(diagram (620 120)
  (grid)
  (drop-shadow)
  (with-theme (:uml-activity-default)
    (with-options (:filter :drop-shadow)
      (uml-action (x+ canvas.left 60) "action1" :id :act1)
      (uml-action (x+ $1.center  200) "action2" :id :act2 :height 80)
      (uml-action (x+ $1.center  160) "action3" :id :act3 :height 60)
      (uml-action (x+ $1.center  140) "action4" :id :act4))
    (uml-pin :act1 :R  "foo"  :id :foo)
    (uml-pin :act2 :L1 "bar"  :id :bar)
    (uml-pin :act2 :L3 "baz"  :id :baz)
    (uml-pin :act3 :L  "quux" :id :quux1 :multi t :offset '(-10 5))
    (uml-pin :act3 :R  nil    :id :quux2 :multi t)
    (uml-flow :foo   :bar  :style :RL)
    (uml-flow :foo   :baz  :style :RL)
    (uml-flow :act2  :quux1)
    (uml-flow :quux2 :act4)))
-->

```kaavio
<!-- expand: UML-PIN-SAMPLE -->
```
Figure. uml-pin のサンプル

　上記の作図は以下のコードで行なっています。ピンはフローの接続点に付けるものなので、
対象アクションの位置とその接続点を指定します。接続点は、コネクタであれば `:RL` などと
記述する表記の片側分なので、 `:R` や `:T3` などと記述することになります。また、通常は
[拡張領域](#uml-expansion-region)に渡すことになる入出力コレクションは `:multi t` と
することで表現できます。

```lisp
<!-- expand: UML-PIN-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-pin マクロ
* with-uml-pin-options マクロ

#### uml-port
<!-- autolink: [$$](#uml-port) -->

　uml-port は UML のコンポーネント図などにおけるポートを表記するための図形要素です。

<!-- snippet: UML-PORT-SAMPLE
(diagram (400 200)
  (grid)
  (with-theme (:uml-component-default)
    (uml-component (x+ canvas.cc 40) "server"
                   :width 200 :height 150 :contents t :id :svr)
    (uml-port svr.CL :name '("port" :offset (-20 2)) :id :port1)
    (with-subcanvas-of (:svr)
      (uml-component (xy+ port1.cc 100 0)
                     "transactor" :height 70 :id :tran)
      (uml-dependency :port1 :tran))
    (uml-interface (xy+ port1.cc -100 0) "interface" :id :interface)
    (uml-association :port1 :interface)))
-->

```kaavio
<!-- expand: UML-PORT-SAMPLE -->
```
Figure. uml-port のサンプル

　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-PORT-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-port マクロ
* with-uml-port-options マクロ

#### uml-realization
<!-- autolink: [$$](#uml-realization) -->

　uml-realization は UML のクラス図などにおける実現関係を表記するための図形要素です。

<!-- snippet: UML-REALIZATION-SAMPLE
(diagram (400 80)
  (grid)
  (with-theme (:uml-class-default)
    (uml-class   (x+ canvas.cl  50) "foo" :id :foo)
    (uml-class   (x+ canvas.cr -50) "bar" :id :bar)
    (uml-realization :foo :bar
                     :stereotype '("stereotype" :font 9)
                     :name       '("name" :offset (0 5)))))
-->

```kaavio
<!-- expand: UML-REALIZATION-SAMPLE -->
```
Figure. uml-realization のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-REALIZATION-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-realization マクロ
* with-uml-realization-options マクロ

#### uml-role-info
<!-- autolink: [$$](#uml-role-info) -->

　uml-role-info は UML の関連などにおいて使用されるロール情報の記述をするための
要素です。以下に例を示します。

<!-- snippet: UML-ROLE-INFO-SAMPLE
(diagram (340 100)
  (grid)
  (drop-shadow)
  (with-theme (:uml-class-default)
    (with-uml-class-options (:width 80 :height 40 :filter :drop-shadow)
      (uml-class (x+ canvas.cl  60) "foo")
      (uml-class (x+ canvas.cr -60) "bar")
      (uml-association $2.id $1.id :arrows 1 :role1 :parent :role2 :child))))
-->

```kaavio
<!-- expand: UML-ROLE-INFO-SAMPLE -->
```
Figure. uml-role-info のサンプル

　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-ROLE-INFO-SAMPLE -->
```

　関連などでは両端にロール情報の指定がありうるため、 `:role1 :role2` で指定を行ない
ます。指定されたパラメータは make-uml-role 関数に渡されます。詳細は以下を参照して
ください。

* make-uml-role 関数

#### uml-signal
<!-- autolink: [$$](#uml-signal) -->

　uml-signal は UML のアクティビティ図におけるシグナル送受信を表記するための図形要素です。
ほぼテキストボックスと同じように使えますが、少なくとも送信か受信かの区別をパラメータで与える必要
があります。また、形状は `:direction` パラメータで `:left` か `:right` で与えます。

<!-- snippet: UML-SIGNAL-SAMPLE
(diagram (400 160)
  (grid)
  (drop-shadow)
  (with-theme (:uml-activity-default)
    (uml-activity-start '(50 40) :id :start)
    (with-options (:filter :drop-shadow)
      (uml-signal (x+ start.center 150) :send
                  "sending~%signal"   :direction :right :id :step1)
      (uml-signal (y+ step1.center  80) :receive
                  "receiving~%signal" :direction :left  :id :step2))
    (uml-activity-final (x+ step2.center 150) :id :final)
    (uml-flow :start :step1)
    (uml-flow :step1 :step2)
    (uml-flow :step2 :final)))
-->

```kaavio
<!-- expand: UML-SIGNAL-SAMPLE -->
```
Figure. uml-signal のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-SIGNAL-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-signal マクロ
* with-uml-signal-options マクロ


#### uml-state-begin
<!-- autolink: [$$](#uml-state-begin) -->

　uml-state-begin は UML の状態マシン図における開始状態を表記するための図形要素です。

<!-- snippet: UML-STATE-BEGIN-SAMPLE
(diagram (200 80)
  (grid)
  (with-theme (:uml-statemachine-default)
    (uml-state-begin (x+ canvas.cc -60) :id :start)
    (uml-state (x+ $1.cc 100) "state" :id :state)
    (uml-transition :start :state)))
-->

```kaavio
<!-- expand: UML-STATE-BEGIN-SAMPLE -->
```
Figure. uml-state-begin のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-STATE-BEGIN-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-state-begin マクロ
* with-uml-state-begin-options マクロ

#### uml-state-end
<!-- autolink: [$$](#uml-state-end) -->

　uml-state-end は UML の状態マシン図における終了状態を表記するための図形要素です。

<!-- snippet: UML-STATE-END-SAMPLE
(diagram (200 80)
  (grid)
  (with-theme (:uml-statemachine-default)
    (uml-state (x+ canvas.cc -40) "state" :id :state)
    (uml-state-end (x+ $1.cc 100) :id :end)
    (uml-flow :state :end)))
-->

```kaavio
<!-- expand: UML-STATE-END-SAMPLE -->
```
Figure. uml-state-end のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-STATE-END-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-state-end マクロ
* with-uml-state-end-options マクロ

#### uml-state-history
<!-- autolink: [$$](#uml-state-history) -->

　uml-state-history は UML の状態マシン図における履歴状態を表記するための
図形要素です。

<!-- snippet: UML-STATE-HISTORY-SAMPLE
(diagram (500 180)
  (grid)
  (drop-shadow)
  (with-theme (:uml-statemachine-default)
    (with-uml-state-options (:filter :drop-shadow)
      (uml-state (xy+ canvas.tc -70 90) "active"
        :width 320 :height 150 :id :active
        :contents
        ((uml-state-begin   (xy+ canvas.tl 20 50) :id :begin)
         (uml-state (x+  $1.cc  90) "state1" :id :state1)
         (uml-state (x+  $1.cc 140) "state2" :id :state2)
         (uml-state-history (xy+ canvas.br -40 -20) :id :history)
         (uml-transition :begin  :state1)
         (uml-transition :state1 :state2 :style :R1L1)
         (uml-transition :state2 :state1 :style :L3R3)))
      (uml-state (x+ active.cr 100) "sleep" :id :sleep)
      (uml-transition :active :sleep)
      (uml-transition :sleep  :history :style :BR))))
-->

```kaavio
<!-- expand: UML-STATE-HISTORY-SAMPLE -->
```
Figure. uml-state-history のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-STATE-HISTORY-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-state-history マクロ
* with-uml-state-history-options マクロ

#### uml-state
<!-- autolink: [$$](#uml-state) -->

　uml-state は UML の状態マシン図における状態を表記するための図形要素です。

<!-- snippet: UML-STATE-SAMPLE
(diagram (400 220)
  (grid)
  (with-theme (:uml-statemachine-default)
    (uml-state-begin '(50 40) :id :start)
    (uml-state (x+ start.cc 100) "state1" :id :state1)
    (uml-state (x+ state1.cc 150) "state2" :id :state2)
    (uml-state (xy+ canvas.cc -60 30) "state3" :id :state3 :width 250 :height 110
        :contents
        ((uml-state (x+ canvas.cc -70) "sub-state1")
         (uml-state (x+ canvas.cc  70) "sub-state2")
         (uml-transition $2.id $1.id)))
    (uml-state-end (xy+ state3.cc 220 50) :id :final)
    (uml-transition :start :state1)
    (uml-transition :state1 :state2)
    (uml-transition :state2 :state3 :style :BR1)
    (uml-transition :state3 :final)))
-->

```kaavio
<!-- expand: UML-STATE-SAMPLE -->
```
Figure. uml-state のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-STATE-SAMPLE -->
```

　`:contents` パラメータと with-subcanvas-of マクロは通常は同じように使えますが、
uml-state では違いが存在します。 `:contents` で内部を描画する場合、uml-state は
テキストを上端付近に描画しますが、 `:contents` を使わずに with-subcanvas-of マクロを
使用した場合、テキストは中央に描画されます。この問題を解決するため、uml-state で
は `:contents t` と指定することでテキストを上端付近に描画させることができます。

　詳細は以下を参照してください。

* uml-state マクロ
* with-uml-state-options マクロ

#### uml-stereotype-info
<!-- autolink: [$$](#uml-stereotype-info) -->

　uml-stereotype-info は UML の各種要素において使用されるステレオタイプおよび
キーワードの指定をするための要素です。以下に例を示します。

<!-- snippet: UML-STEREOTYPE-INFO-SAMPLE
(diagram (340 100)
  (grid)
  (drop-shadow)
  (with-theme (:uml-class-default)
    (with-uml-class-options (:width 80 :height 40 :filter :drop-shadow)
      (uml-class (x+ canvas.cl  60) "foo")
      (uml-class (x+ canvas.cr -60) "bar" :keyword '(:thread :font 8))
      (uml-dependency $2.id $1.id :keyword :uses))))
-->

```kaavio
<!-- expand: UML-STEREOTYPE-INFO-SAMPLE -->
```
Figure. uml-stereotype-info のサンプル

　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-STEREOTYPE-INFO-SAMPLE -->
```

　どの UML 描画要素でも、 `:stereotype` と `:keyword` のいずれかのパラメータを使用
します。指定されたパラメータは make-uml-stereotype 関数に渡されます。詳細は以下を
参照してください。

* make-uml-stereotype 関数

#### uml-time-event
<!-- autolink: [$$](#uml-time-event) -->

　uml-time-event は UML のアクティビティ図における時間シグナルを表記するための図形要素です。


<!-- snippet: UML-TIME-EVENT-SAMPLE
(diagram (300 100)
  (grid)
  (drop-shadow)
  (with-theme (:uml-activity-default)
    (with-options (:filter :drop-shadow)
      (uml-time-event (x+ canvas.center -80) :label "wait 20min." :id :clock)
      (uml-action     (x+ canvas.center  60) "action" :id :next))
    (uml-flow :clock :next)))
-->

```kaavio
<!-- expand: UML-TIME-EVENT-SAMPLE -->
```
Figure. uml-time-event のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-TIME-EVENT-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-time-event マクロ
* with-uml-time-event-options マクロ

#### uml-transition
<!-- autolink: [$$](#uml-transition) -->

　uml-transition は UML の状態マシン図における状態間の遷移を表記するための
図形要素です。

<!-- snippet: UML-TRANSITION-SAMPLE
(diagram (400 100)
  (grid)
  (drop-shadow)
  (with-theme (:uml-statemachine-default)
    (with-uml-state-options (:filter :drop-shadow)
      (uml-state '( 60 50) "state1" :id :state1)
      (uml-state '(340 50) "state2" :id :state2)
      (uml-transition :state1 :state2
                      :spec '(:trigger :on-click
                              :action  "OnClick()")))))
-->

```kaavio
<!-- expand: UML-TRANSITION-SAMPLE -->
```
Figure. uml-transition のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-TRANSITION-SAMPLE -->
```

　必須パラメータは接続元／接続先の指定のみで、それ以外は名前付きパラメータです。
そのうち、 `:stereotype :keyword :spec` 以外はほぼコネクタと同じように使えます。
`:spec` は遷移のトリガーやガード条件、アクションを指定するためのものです。これら
についての詳細は以下を参照してください。

* uml-transition-spec
* uml-transition マクロ
* with-uml-transition-options マクロ

#### uml-transition-spec
<!-- autolink: [$$](#uml-transition-spec) -->

　uml-transition-spec は UML の状態マシン図における遷移仕様を記述するための、
トリガー、ガード条件、アクションなどからなる情報です。以下に例を示します。

<!-- snippet: UML-TRANSITION-SPEC-SAMPLE
(diagram (400 190)
  (grid)
  (drop-shadow)
  (with-theme (:uml-statemachine-default)
    (with-uml-state-options (:filter :drop-shadow)
      (uml-state '( 60  40) "state1" :id :st1)
      (uml-state '(340  40) "state2" :id :st2)
      (uml-transition :st1 :st2 :spec :on-idle)
      (uml-state '( 60 100) "state3" :id :st3)
      (uml-state '(340 150) "state4" :id :st4)
      (uml-transition :st3 :st4
                      :style :RL
                      :spec '(:trigger "trigger"
                              :guard   "guard"
                              :action  "state()"
                              :offset  (-90 -35)
                              :font    (:fill :brown :size 9))))))
-->

```kaavio
<!-- expand: UML-TRANSITION-SPEC-SAMPLE -->
```
Figure. uml-transition-spec のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-TRANSITION-SPEC-SAMPLE -->
```

　状態マシン図における遷移仕様は、uml-transition マクロに渡す `:spec` パラメータで
指定します。このパラメータは make-uml-transition-spec 関数に渡されます。単一のキーワード
シンボルや文字列を与えた場合、トリガーとして扱われます。リストを渡せばトリガーやガード条件、
アクション、表示位置の調整やフォント指定を行なうことができます。

　詳細は以下を参照してください。

* uml-transition
* make-uml-transition-spec 関数

#### uml-usecase
<!-- autolink: [$$](#uml-usecase) -->

　uml-usecase は UML のユースケース図におけるユースケースを表記するための図形要素です。

```kaavio
<!-- expand: UML-ACTOR-USECASE-SAMPLE -->
```
Figure. uml-usecase のサンプル


　上記の作図は以下のコードで行なっています。

```lisp
<!-- expand: UML-ACTOR-USECASE-SAMPLE -->
```

　詳細は以下を参照してください。

* uml-usecase マクロ
* with-uml-usecase-options マクロ


${BLANK_PARAGRAPH}

