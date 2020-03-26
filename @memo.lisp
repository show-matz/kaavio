(load "./@load.lisp")

(let ((path (if (member :linux *features*)
				"~/sandbox/cl-diagram/lib/"
				(merge-pathnames "cl-diagram/lib/"
								 (sb-ext:posix-getenv "CVS_SANDBOX")))))
  (setf diagram:*include-paths* (list path)))

(diagram::path/set-current-directory "./sample")
(diagram::path/get-current-directory)
(diagram::cl-apps-main '("tmp.diagram" :utf8 "tmp.svg" :utf8))
(diagram::cl-apps-main '("tmp2.diagram" :utf8 "tmp2.svg" :utf8))
(diagram::cl-apps-main '("textbox.diagram" :utf8 "textbox.svg" :utf8))
(diagram::cl-apps-main '("stylesheet.diagram" :utf8 "stylesheet.svg" :utf8))

