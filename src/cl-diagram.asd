(defsystem :cl-diagram
  :description "cl-diagram: svg diagram creating utility."
  :version     "0.1.0"
  :depends-on  ()
  :components  (;; ---------------------------------------- BEGIN COMPONENTS
				(:file "arc"                       :depends-on ("cl-diagram"
																"point"
																"canvas"
																"mathutil"
																"path"))
				(:file "balloon"                   :depends-on ("cl-diagram"
																"arc"
																"polygon"
																"filter"
																"text-shape"))
				(:file "binutil"                   :depends-on ("cl-diagram"))
				(:file "canvas"                    :depends-on ("cl-diagram"
																"point"))
				(:file "circle"                    :depends-on ("cl-diagram"
																"constants"
																"mathutil"
																"canvas"
																"point"
																"shape"
																"stroke-info"
																"link-info"
																"filter"
																"writer"))
				(:file "cl-diagram")
				(:file "connector"                 :depends-on ("cl-diagram"
																"constants"
																"line"
																"shape"
				                                                "dictionary"
				                                                "rectangle"
				                                                "writer"))
				(:file "constants"                 :depends-on ("cl-diagram"))
				(:file "create-svg"                :depends-on ("cl-diagram"
				                                                "constants"
				                                                "entity"
				                                                "layer-manager"
				                                                "dictionary"
				                                                "point"
				                                                "canvas"
				                                                "font-info"
				                                                "stroke-info"
				                                                "writer"))
				(:file "defs"                      :depends-on ("cl-diagram"
				                                                "constants"
																"entity"
				                                                "layer-manager"
				                                                "dictionary"
				                                                "point"
				                                                "canvas"
				                                                "font-info"
				                                                "stroke-info"
				                                                "writer"))
				(:file "dictionary"                :depends-on ("cl-diagram"))
				(:file "ellipse"                   :depends-on ("cl-diagram"
																"constants"
																"canvas"
																"point"
																"shape"
																"stroke-info"
																"link-info"
																"filter"
																"writer"))
				(:file "endmark-info"              :depends-on ("cl-diagram"
																"constants"
																"mathutil"
																"point"
																"canvas"
																"dictionary"
																"fill-info"
																"stroke-info"
																"writer"))
				(:file "entity"                    :depends-on ("cl-diagram"
																"canvas"
																"writer"))
				(:file "explosion"                 :depends-on ("cl-diagram"
																"polygon"
																"filter"
																"text-shape"))
				(:file "fill-info"                 :depends-on ("cl-diagram"))
				(:file "filter"                    :depends-on ("cl-diagram"
																"writer"))
				(:file "folder"                    :depends-on ("cl-diagram"
																"polygon"
																"filter"
																"text-shape"))
				(:file "font-info"                 :depends-on ("cl-diagram"
																"fill-info"
																"stroke-info"))
				(:file "grid"                      :depends-on ("cl-diagram"
																"stroke-info"
																"entity"
																"writer"))
				(:file "group"                     :depends-on ("cl-diagram"
																"canvas"
																"shape"
																"rectangle"
																"writer"))
				(:file "image"                     :depends-on ("cl-diagram"
																"binutil"
																"shape"
																"label-info"
																"link-info"
																"point"
																"filter"
																"writer"))
				(:file "label-info"                :depends-on ("cl-diagram"
																"constants"
																"point"
																"canvas"
																"font-info"
																"shape"
																"writer"))
				(:file "layer-manager"             :depends-on ("cl-diagram"
																"writer"))
				(:file "line"                      :depends-on ("cl-diagram"
																"constants"
																"point"
																"mathutil"
																"label-info"
																"stroke-info"
																"endmark-info"
																"entity"
																"filter"
																"writer"))
				(:file "link-info"                 :depends-on ("cl-diagram"
																"constants"
																"writer"))
				(:file "mathutil"                  :depends-on ("cl-diagram"
																"point"))
				(:file "paragraph"                 :depends-on ("cl-diagram"
																"constants"
																"text"
																"shape"
																"font-info"
																"link-info"
																"point"
																"filter"
																"writer"))
				(:file "path"                      :depends-on ("cl-diagram"
																"constants"
																"fill-info"
																"stroke-info"
																"entity"
																"filter"
																"writer"))
				(:file "pathutil"                  :depends-on ("cl-diagram"))
				(:file "point"                     :depends-on ("cl-diagram"))
				(:file "polygon"                   :depends-on ("cl-diagram"
																"constants"
																"fill-info"
																"stroke-info"
																"link-info"
																"entity"
																"filter"
																"writer"))
				(:file "raw-svg"                   :depends-on ("cl-diagram"
																"entity"
																"writer"))
				(:file "rectangle"                 :depends-on ("cl-diagram"
																"constants"
																"canvas"
																"point"
																"shape"
																"stroke-info"
																"link-info"
																"filter"
																"writer"))
				(:file "shadow-filter"             :depends-on ("cl-diagram"
																"filter"
																"writer"))
				(:file "shape"                     :depends-on ("cl-diagram"
																"point"
																"canvas"
																"mathutil"
																"entity"
																"link-info"))
				(:file "stencil"                   :depends-on ("cl-diagram"
																"pathutil"))
				(:file "stroke-info"               :depends-on ("cl-diagram"))
				(:file "stylesheet"                :depends-on ("cl-diagram"
																"entity"
																"stroke-info"
																"fill-info"
																"font-info"
																"writer"))
				(:file "text-shape"                :depends-on ("cl-diagram"
																"canvas"
																"group"
																"paragraph"
																"font-info"
																"fill-info"
																"stroke-info"
																"writer"))
				(:file "text"                      :depends-on ("cl-diagram"
																"constants"
																"entity"
																"font-info"
																"link-info"
																"filter"
																"writer"))
				(:file "textbox"                   :depends-on ("cl-diagram"
																"rectangle"
																"filter"
																"text-shape"))
				(:file "use"                       :depends-on ("cl-diagram"
																"constants"
																"canvas"
																"point"
																"shape"
																"writer"))
				(:file "writer"                    :depends-on ("cl-diagram"))
				;; ------------------------------------------ END COMPONENTS
))

