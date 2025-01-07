#|
#|ASD|#                (:file "constants"                 :depends-on ("kaavio"))
#|EXPORT|#                ;constants.lisp
 |#

(in-package :kaavio)


;;------------------------------------------------------------------------------
;;
;; default parameter values
;;
;;------------------------------------------------------------------------------

;; default parameter for link
#|
#|EXPORT|#                :*default-link-target*
 |#
(defparameter *default-link-target* :self)

;; default parameter for endmark-info
#|
#|EXPORT|#                :*default-endmark-1*
#|EXPORT|#                :*default-endmark-2*
#|EXPORT|#                :*default-endmark-type*
#|EXPORT|#                :*default-endmark-size*
#|EXPORT|#                :*default-endmark-fill*
 |#
(defparameter *default-endmark-1*       nil)
(defparameter *default-endmark-2*       nil)
(defparameter *default-endmark-type*    :none)
(defparameter *default-endmark-size*    :medium)
(defparameter *default-endmark-fill*    nil)

;; default parameter for label-info
#|
#|EXPORT|#                :*default-label-position*
#|EXPORT|#                :*default-label-font*
#|EXPORT|#                :*default-label-offset*
 |#
(defparameter *default-label-position*  :below)
(defparameter *default-label-font*      nil)
(defparameter *default-label-offset*    nil)

;; default parameter for connector
#|
#|EXPORT|#                :*default-connector-style*
#|EXPORT|#                :*default-connector-spacing*
 |#
(defparameter *default-connector-style*   :CC)
(defparameter *default-connector-spacing*  30)

;; default parameter for rectangle
#|
#|EXPORT|#                :*default-rectangle-rx*
#|EXPORT|#                :*default-rectangle-ry*
 |#
(defparameter *default-rectangle-rx*    nil)
(defparameter *default-rectangle-ry*    nil)

;; default parameter for text
#|
#|EXPORT|#                :*default-text-align*
 |#
(defparameter *default-text-align*      :left)

;; default parameter for paragraph
#|
#|EXPORT|#                :*default-paragraph-align*
#|EXPORT|#                :*default-paragraph-valign*
 |#
(defparameter *default-paragraph-align*        :left)
(defparameter *default-paragraph-valign*       :top)

#|
#|EXPORT|#                :*default-history-count*
 |#
(defparameter *default-history-count*          9)

#|
#|EXPORT|#                :*default-layer*
 |#
(defparameter *default-layer*  nil)

