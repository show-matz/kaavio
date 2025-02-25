#|
#|ASD|#                (:file "create-svg"                :depends-on ("kaavio"
#|ASD|#                                                                "constants"
#|ASD|#                                                                "colormap"
#|ASD|#                                                                "entity"
#|ASD|#                                                                "definition"
#|ASD|#                                                                "layer-manager"
#|ASD|#                                                                "dictionary"
#|ASD|#                                                                "point"
#|ASD|#                                                                "canvas"
#|ASD|#                                                                "font-info"
#|ASD|#                                                                "stroke-info"
#|ASD|#                                                                "writer"))
#|EXPORT|#                ;create-svg.lisp
 |#

(in-package :kaavio)


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro diagram
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{diagram}} (width height ${KEY} fill) ${BODY} body
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `width` ---- 幅を数値で指定します。
;;* `height` ---- 高さを数値で指定します。
;;* `fill` ---- 背景全体の塗り潰しが必要な場合は指定します。
;;* `body` ---- 描画コードを記述します。
;;
;;${DESCRIPTION}
;;
;;　幅 `width` 、高さ `height` の SVG 画像を作成し、内部を `body` のコードで描画します。
;;
;;${NO_SEE_ALSO}
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :create-svg
#|EXPORT|#                :diagram
#|EXPORT|#                :register-entity
#|EXPORT|#                :width
#|EXPORT|#                :height
 |#
(defmacro diagram ((width height &key fill) &rest body)
  `(create-svg (,width ,height :fill ,fill) ,@body))

(defmacro create-svg ((width height &key fill desc) &rest body)
  (let ((g-layer-mgr (gensym "LAYER-MGR"))
        (g-writer    (gensym "WRITER"))
        (g-filter    (gensym "FILTER"))
        (g-filters   (gensym "FILTERS"))
        (g-entity    (gensym "ENTITY"))
        (g-entities  (gensym "ENTITIES"))
        (g-desc      (gensym "DESC"))
        (g-dict      (gensym "DICT")))
    `(let ((,g-layer-mgr (layer-create-manager))
           (,g-desc      ,desc)
           (,g-filters   nil)
           (,g-entities  nil)
           (,g-dict      (dict-create *default-history-count*))  ;;ToDo : export dict-create!
           (canvas       (make-canvas (make-point 0 0 :absolute) ,width ,height)))
       (declare (special canvas))
       (labels ((layer (name &optional (display :inline))
                  (layer-register ,g-layer-mgr name display))
                (register-filter (,g-filter)
                  (unless (typep ,g-filter 'filter)
                    (throw-exception "Can't register ~A as filter." ,g-filter))
                  (push ,g-filter ,g-filters))
                (register-entity (,g-entity)
                  (unless (typep ,g-entity 'entity)
                    (throw-exception "Can't register ~A to dictionary : NOT entity." ,g-entity))
                  (push ,g-entity ,g-entities)
                  (check ,g-entity canvas ,g-dict)
                  (dict-register ,g-dict ,g-entity)))
         (declare (ignorable #'layer #'register-entity #'register-filter))
         (let ((*default-font*   (or *default-font*   (make-font)))
               (*default-fill*   (or *default-fill*   (make-fill)))
               (*default-stroke* (or *default-stroke* (make-stroke))))
           (with-dictionary ,g-dict
             ,@body)))

       ;; stable sort by priority of layer
       (setf ,g-entities (stable-sort (nreverse ,g-entities)
                                      (lambda (e1 e2)
                                        (< (layer-get-priority ,g-layer-mgr (slot-value e1 'layer))
                                           (layer-get-priority ,g-layer-mgr (slot-value e2 'layer))))))

       (let ((,g-writer (create-svg-writer)))
         (writer-write ,g-writer "<?xml version='1.0' encoding='utf-8' ?>")
         (writer-write ,g-writer "<svg xmlns='http://www.w3.org/2000/svg' "
                                 "xmlns:xlink='http://www.w3.org/1999/xlink' "
                                 "version='1.1' baseProfile='full' xml:space='default' "
                                 "width='"  (canvas-width  canvas)  "' "
                                 "height='" (canvas-height canvas) "'>")
         (writer-incr-level ,g-writer)
         (when ,g-desc
           (writer-write ,g-writer "<desc>" ,g-desc "</desc>"))
         (writer-write ,g-writer)

         (labels ((definitionp (ent)
                    (typep ent 'definition)))
           ;; filter があれば最初に出力
           (dolist (,g-filter ,g-filters)
             (write-filter ,g-filter ,g-writer))
           ;; definition は layer とは無関係に先頭に出力
           (dolist (,g-entity (remove-if-not #'definitionp ,g-entities))
             (write-header ,g-entity ,g-writer)
             (draw-entity  ,g-entity ,g-writer))
           (when ,fill
             (writer-write ,g-writer "<rect x='0' y='0' "
                                     "width='"  (canvas-width  canvas)  "' "
                                     "height='" (canvas-height  canvas) "' "
                                     "fill='" ,(colormap-fix fill) "' stroke='none' />"))
           ;; definition 以外を layer の優先順で出力
           (dolist (,g-entity (remove-if #'definitionp ,g-entities))
             (layer-change ,g-layer-mgr (slot-value ,g-entity 'layer) ,g-writer)
             (write-header ,g-entity ,g-writer)
             (draw-entity  ,g-entity ,g-writer))
           (layer-change ,g-layer-mgr nil ,g-writer))

         (writer-decr-level ,g-writer)
         (writer-write ,g-writer)
         (writer-write ,g-writer "</svg>")
         (writer-close ,g-writer)))))


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function layer
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{layer}} name ${OPTIONAL} (display :inline)
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `name` ---- レイヤーの名前をキーワードで指定します。
;;* `display` ---- レイヤー全体の表示を `:inline` または `:none` で指定します。省略時のデフォルトは `:inline` で、レイヤー全体を表示します。 `:none` を指定すると、レイヤー全体を非表示にします。
;;
;;${DESCRIPTION}
;;
;;　レイヤーの導入を宣言します。レイヤーについては [$@ 節](#レイヤー)を参照してください。
;;
;;${SEE_ALSO}
;;
;;* [](#レイヤー)
;;
;;${NOTES}
;;
;;　layer 関数は、実際には diagram マクロ内で作成される局所関数です。
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :layer
 |#
