(defsystem :cl-diagram
  :description "cl-diagram: svg diagram creating utility."
  :version     "0.1.0"
  :depends-on  ("jp"
				"pathnames"
				"cl-binary")
  :components  (;; ---------------------------------------- BEGIN COMPONENTS
				(:file "arc"                       :depends-on ("cl-diagram"
																"mathutil"
																"path"))
				(:file "binutil"                   :depends-on ("cl-diagram"))
				(:file "canvas"                    :depends-on ("cl-diagram"))
				(:file "circle"                    :depends-on ("cl-diagram"
																"constants"
																"mathutil"
																"canvas"
																"point"
																"shape"
																"stroke-info"
																"link-info"
																"writer"))
				(:file "cl-apps-main"              :depends-on ("cl-diagram"
																"pathutil"
																"create-svg"))
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
				                                                "canvas"
				                                                "font-info"
				                                                "stroke-info"
				                                                "svg-writer"))
				(:file "css"                       :depends-on ("cl-diagram"
																"entity"
																"writer"))
				(:file "dictionary"                :depends-on ("cl-diagram"))
				(:file "ellipse"                   :depends-on ("cl-diagram"
																"constants"
																"canvas"
																"point"
																"shape"
																"stroke-info"
																"link-info"
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
																"writer"))
				(:file "fill-info"                 :depends-on ("cl-diagram"
																"constants"))
				(:file "font-info"                 :depends-on ("cl-diagram"
																"constants"))
				(:file "group"                     :depends-on ("cl-diagram"
																"canvas"
																"shape"
																"rectangle"
																"writer"))
				(:file "image"                     :depends-on ("cl-diagram"
																"binutil"
																"shape"
																"rectangle"
																"label-info"
																"link-info"
																"point"
																"writer"))
				(:file "label-info"                :depends-on ("cl-diagram"
																"constants"
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
																"stroke-info"
																"endmark-info"
																"entity"
																"writer"))
				(:file "link-info"                 :depends-on ("cl-diagram"
																"constants"
																"writer"))
				(:file "mathutil"                  :depends-on ("cl-diagram"
																"point"))
				(:file "paragraph"                 :depends-on ("cl-diagram"
																"constants"
																"shape"
																"font-info"
																"link-info"
																"point"
																"writer"))
				(:file "path"                      :depends-on ("cl-diagram"
																"constants"
																"fill-info"
																"stroke-info"
																"entity"
																"writer"))
				(:file "pathutil"                  :depends-on ("cl-diagram"))
				(:file "point"                     :depends-on ("cl-diagram"))
				(:file "polygon"                   :depends-on ("cl-diagram"
																"constants"
																"fill-info"
																"stroke-info"
																"link-info"
																"entity"
																"writer"))
				(:file "raw-svg"                   :depends-on ("cl-diagram"
																"entity"
																"writer"))
				(:file "rectangle"                 :depends-on ("cl-diagram"
																"constants"
																"mathutil"
																"canvas"
																"point"
																"shape"
																"stroke-info"
																"link-info"
																"writer"))
				(:file "shape"                     :depends-on ("cl-diagram"
																"entity"
																"link-info"))
				(:file "stencil"                   :depends-on ("cl-diagram"
																"pathutil"))
				(:file "stroke-info"               :depends-on ("cl-diagram"
																"constants"))
				(:file "text"                      :depends-on ("cl-diagram"
																"constants"
																"entity"
																"font-info"
																"link-info"
																"writer"))
				(:file "writer"                    :depends-on ("cl-diagram"))
				;; ------------------------------------------ END COMPONENTS
))

