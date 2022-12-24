(defsystem :kaavio
  :description "kaavio: svg diagram generating utility."
  :version     "0.1.0"
  :depends-on  ()
  :components  (;; ---------------------------------------- BEGIN COMPONENTS
				(:file "2d-curve"                  :depends-on ("kaavio"
																"constants"
																"point"
																"mathutil"
																"stroke-info"
																"endmark-info"
																"entity"
																"filter"
																"writer"))
				(:file "3d-curve"                  :depends-on ("kaavio"
																"constants"
																"point"
																"mathutil"
																"stroke-info"
																"endmark-info"
																"entity"
																"filter"
																"writer"))
				(:file "arc"                       :depends-on ("kaavio"
																"point"
																"canvas"
																"mathutil"
																"path"))
				(:file "balloon"                   :depends-on ("kaavio"
																"constants"
																"arc"
																"polygon"
																"filter"
																"text-shape"))
				(:file "binutil"                   :depends-on ("kaavio"))
				(:file "block-arrow"               :depends-on ("kaavio"
																"constants"
																"polygon"))
				(:file "brace"                     :depends-on ("kaavio"
																"constants"
																"path"))
				(:file "canvas"                    :depends-on ("kaavio"
																"point"))
				(:file "circle"                    :depends-on ("kaavio"
																"constants"
																"mathutil"
																"canvas"
																"point"
																"shape"
																"stroke-info"
																"link-info"
																"filter"
																"writer"))
				(:file "colormap"                  :depends-on ("kaavio"))
				(:file "connector"                 :depends-on ("kaavio"
																"constants"
																"line"
																"shape"
				                                                "dictionary"
				                                                "rectangle"
				                                                "writer"))
				(:file "constants"                 :depends-on ("kaavio"))
				(:file "create-svg"                :depends-on ("kaavio"
				                                                "constants"
																"colormap"
				                                                "entity"
				                                                "definition"
				                                                "layer-manager"
				                                                "dictionary"
				                                                "point"
				                                                "canvas"
				                                                "font-info"
				                                                "stroke-info"
				                                                "writer"))
				(:file "cross"                     :depends-on ("kaavio"
																"constants"
																"canvas"
																"point"
																"shape"
																"stroke-info"
																"link-info"
																"filter"
																"writer"))
				(:file "cube"                      :depends-on ("kaavio"
																"constants"
																"path"
																"filter"
																"text-shape"))
				(:file "cylinder"                  :depends-on ("kaavio"
																"constants"
																"path"
																"filter"
																"text-shape"))
				(:file "defgradient"               :depends-on ("kaavio"
				                                                "definition"
																"colormap"
				                                                "writer"))
				(:file "defgroup"                  :depends-on ("kaavio"
				                                                "constants"
				                                                "definition"
				                                                "layer-manager"
				                                                "dictionary"
				                                                "point"
				                                                "canvas"
				                                                "font-info"
				                                                "stroke-info"
				                                                "writer"))
				(:file "definition"                :depends-on ("kaavio"
																"entity"))
				(:file "defpattern"                :depends-on ("kaavio"
				                                                "constants"
				                                                "definition"
				                                                "layer-manager"
				                                                "dictionary"
				                                                "point"
				                                                "canvas"
				                                                "font-info"
				                                                "stroke-info"
				                                                "writer"))
				(:file "diamond"                   :depends-on ("kaavio"
																"mathutil"
																"constants"
																"canvas"
																"point"
																"shape"
																"stroke-info"
																"link-info"
																"filter"
																"writer"))
				(:file "dictionary"                :depends-on ("kaavio"))
				(:file "document"                  :depends-on ("kaavio"
																"constants"
																"path"
																"filter"
																"text-shape"))
				(:file "ellipse"                   :depends-on ("kaavio"
																"constants"
																"canvas"
																"point"
																"shape"
																"stroke-info"
																"link-info"
																"filter"
																"writer"))
				(:file "endmark-info"              :depends-on ("kaavio"
																"constants"
																"mathutil"
																"point"
																"canvas"
																"dictionary"
																"fill-info"
																"stroke-info"
																"writer"))
				(:file "entity"                    :depends-on ("kaavio"
																"canvas"
																"point"
																"writer"))
				(:file "explosion"                 :depends-on ("kaavio"
																"constants"
																"polygon"
																"filter"
																"text-shape"))
				(:file "fill-info"                 :depends-on ("kaavio"
																"colormap"))
				(:file "filter"                    :depends-on ("kaavio"
																"writer"))
				(:file "folder"                    :depends-on ("kaavio"
																"constants"
																"polygon"
																"filter"
																"text-shape"))
				(:file "font-info"                 :depends-on ("kaavio"
																"colormap"
																"fill-info"
																"stroke-info"))
				(:file "grid"                      :depends-on ("kaavio"
																"colormap"
																"stroke-info"
																"entity"
																"writer"))
				(:file "group"                     :depends-on ("kaavio"
																"canvas"
																"shape"
																"rectangle"
																"writer"))
				(:file "image"                     :depends-on ("kaavio"
																"binutil"
																"shape"
																"label-info"
																"link-info"
																"point"
																"filter"
																"writer"))
				(:file "kaavio")
				(:file "label-info"                :depends-on ("kaavio"
																"constants"
																"point"
																"canvas"
																"font-info"
																"shape"
																"writer"))
				(:file "layer-manager"             :depends-on ("kaavio"
																"writer"))
				(:file "line"                      :depends-on ("kaavio"
																"constants"
																"point"
																"mathutil"
																"label-info"
																"stroke-info"
																"endmark-info"
																"entity"
																"filter"
																"writer"))
				(:file "link-info"                 :depends-on ("kaavio"
																"constants"
																"writer"))
				(:file "mathutil"                  :depends-on ("kaavio"
																"point"))
				(:file "memo"                      :depends-on ("kaavio"
																"constants"
																"arc"
																"polygon"
																"filter"
																"text-shape"))
				(:file "paragraph"                 :depends-on ("kaavio"
																"constants"
																"text"
																"shape"
																"font-info"
																"link-info"
																"point"
																"writer"))
				(:file "parallelogram"             :depends-on ("kaavio"
																"mathutil"
																"constants"
																"canvas"
																"point"
																"shape"
																"stroke-info"
																"link-info"
																"filter"
																"writer"))
				(:file "path"                      :depends-on ("kaavio"
																"constants"
																"fill-info"
																"stroke-info"
																"entity"
																"filter"
																"writer"))
				(:file "pathutil"                  :depends-on ("kaavio"))
				(:file "person"                    :depends-on ("kaavio"
																"constants"
																"path"))
				(:file "point"                     :depends-on ("kaavio"))
				(:file "polygon"                   :depends-on ("kaavio"
																"constants"
																"fill-info"
																"stroke-info"
																"link-info"
																"entity"
																"filter"
																"writer"))
				(:file "raw-svg"                   :depends-on ("kaavio"
																"constants"
																"entity"
																"writer"))
				(:file "rectangle"                 :depends-on ("kaavio"
																"constants"
																"canvas"
																"point"
																"shape"
																"stroke-info"
																"link-info"
																"filter"
																"writer"))
				(:file "shadow-filter"             :depends-on ("kaavio"
																"filter"
																"writer"))
				(:file "shape"                     :depends-on ("kaavio"
																"point"
																"canvas"
																"mathutil"
																"entity"
																"link-info"))
				(:file "stencil"                   :depends-on ("kaavio"
																"pathutil"))
				(:file "stroke-info"               :depends-on ("kaavio"
																"colormap"))
				(:file "table"                     :depends-on ("kaavio"
																"constants"
																"group"
																"font-info"
																"fill-info"
																"stroke-info"
																"writer"))
				(:file "text-shape"                :depends-on ("kaavio"
																"canvas"
																"group"
																"paragraph"
																"font-info"
																"fill-info"
																"stroke-info"
																"writer"))
				(:file "text"                      :depends-on ("kaavio"
																"constants"
																"entity"
																"font-info"
																"link-info"
																"writer"))
				(:file "textbox"                   :depends-on ("kaavio"
																"constants"
																"rectangle"
																"filter"
																"text-shape"))
				(:file "theme"                     :depends-on ("kaavio"))
				(:file "use"                       :depends-on ("kaavio"
																"constants"
																"defgroup"
																"canvas"
																"point"
																"shape"
																"writer"))
				(:file "writer"                    :depends-on ("kaavio"))
				;; ------------------------------------------ END COMPONENTS
))

