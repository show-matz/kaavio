#|
#|ASD|#             (:file "theme"                     :depends-on ("kaavio"))
#|EXPORT|#              ;theme.lisp
 |#

(in-package :kaavio)


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *theme-hash* nil)

  (defun register-theme-impl (name base settings)
    (unless *theme-hash*
      (setf *theme-hash* (make-hash-table :test #'eq)))
    (setf (gethash name *theme-hash*) (cons base settings))
    nil)

  (defun find-theme-impl (name)
    (unless *theme-hash*
      (setf *theme-hash* (make-hash-table :test #'eq)))
    (gethash name *theme-hash*))

  (register-theme-impl :default nil
    '((t :font '(:family "sans-serif"))
      (:textbox     :stroke :black         :fill :white)
      (:document    :stroke :darkslategray :fill :whitesmoke)
      (:folder      :stroke :darkkhaki     :fill :cornsilk)
      (:person      :stroke :maroon        :fill :linen)
      (:balloon     :stroke :navy          :fill :azure)
      (:memo        :stroke :darkgreen     :fill :mintcream
                    :fill2  :palegreen3    :crease 30 :align :left :valign :top)
      (:cube        :stroke :black         :fill :lightgray :fill2 :darkgray)
      (:cylinder    :stroke :black         :fill :white)
      (:explosion   :stroke :red           :fill :pink)
      (:cross       :stroke :black         :fill :white)
      (:block-arrow :stroke :navy          :fill :skyblue))))


#|
#|EXPORT|#              :register-theme
 |#
(defmacro register-theme ((name &optional base) &rest settings)
  (register-theme-impl name base settings))


#|
#|EXPORT|#              :with-theme
 |#
(defmacro with-theme ((name) &body body)
  (let ((entries (find-theme-impl name)))
    (unless entries
      (throw-exception "theme ~A is not found." name))
    (destructuring-bind (base &rest settings) entries
      (let ((body `(locally ,@body)))
        (dolist (entry (reverse settings))
          (let* ((target (car entry))
                 (params (cdr entry))
                 (macro-name (concatenate 'string "WITH-" (symbol-name target) "-OPTIONS"))
                 (macro  (if (eq t target)
                             'WITH-OPTIONS
                             (find-symbol macro-name *package*))))
            (unless macro
              (throw-exception "macro ~A is not found." macro-name))
            (setf body `(,macro (,@params) ,body))))
        (when base
          (setf body `(with-theme (,base) ,body)))
        body))))


