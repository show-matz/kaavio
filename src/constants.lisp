#|
#|ASD|#				(:file "constants"                 :depends-on ("cl-diagram"))
#|EXPORT|#				;constants.lisp
 |#

(in-package :cl-diagram)


;-------------------------------------------------------------------------------
;
; default parameter values
;
;-------------------------------------------------------------------------------

;; default parameter for link
#|
#|EXPORT|#				:*default-link-target*
 |#
(defparameter *default-link-target* :self)

;; default parameter for fill
#|
#|EXPORT|#				:*default-fill*
#|EXPORT|#				:*default-fill-color*
#|EXPORT|#				:*default-fill-opacity*
 |#
(defparameter *default-fill*           nil)
(defparameter *default-fill-color*     :none)
(defparameter *default-fill-opacity*   nil)

;; default parameter for stroke-info
#|
#|EXPORT|#				:*default-stroke*
#|EXPORT|#				:*default-stroke-color*
#|EXPORT|#				:*default-stroke-width*
#|EXPORT|#				:*default-stroke-opacity*
#|EXPORT|#				:*default-stroke-dasharray*
 |#
(defparameter *default-stroke*           nil)
(defparameter *default-stroke-color*     :black)
(defparameter *default-stroke-width*     1)
(defparameter *default-stroke-opacity*   nil)
(defparameter *default-stroke-dasharray* nil)

;; default parameter for endmark-info
#|
#|EXPORT|#				:*default-endmark-1*
#|EXPORT|#				:*default-endmark-2*
#|EXPORT|#				:*default-endmark-type*
#|EXPORT|#				:*default-endmark-size*
 |#
(defparameter *default-endmark-1*       nil)
(defparameter *default-endmark-2*       nil)
(defparameter *default-endmark-type*    :none)
(defparameter *default-endmark-size*    :midium)

;; default parameter for font-info
#|
#|EXPORT|#				:*default-font*
#|EXPORT|#				:*default-font-family*
#|EXPORT|#				:*default-font-size*
#|EXPORT|#				:*default-font-color*
#|EXPORT|#				:*default-font-style*
#|EXPORT|#				:*default-font-decoration*
#|EXPORT|#				:*default-font-weight*
#|EXPORT|#				:*default-font-line-spacing*
#|EXPORT|#				:*default-font-width-spice*
 |#
(defparameter *default-font*              nil)
(defparameter *default-font-family*       nil)
(defparameter *default-font-size*         12)
(defparameter *default-font-color*        :black)
(defparameter *default-font-style*        nil)
(defparameter *default-font-decoration*   nil)
(defparameter *default-font-weight*       nil)
(defparameter *default-font-line-spacing* 2)
(defparameter *default-font-width-spice*  0.65)

;; default parameter for label-info
#|
#|EXPORT|#				:*default-label-position*
#|EXPORT|#				:*default-label-font*
#|EXPORT|#				:*default-label-offset*
 |#
(defparameter *default-label-position*	:below)
(defparameter *default-label-font*		nil)
(defparameter *default-label-offset*	10)

;; default parameter for connector
#|
#|EXPORT|#				:*default-connector-style*
#|EXPORT|#				:*default-connector-spacing*
 |#
(defparameter *default-connector-style*   :CC)
(defparameter *default-connector-spacing*  30)

;; default parameter for rectangle
#|
#|EXPORT|#				:*default-rectangle-rx*
#|EXPORT|#				:*default-rectangle-ry*
 |#
(defparameter *default-rectangle-rx*    nil)
(defparameter *default-rectangle-ry*    nil)

;; default parameter for text
#|
#|EXPORT|#				:*default-text-align*
 |#
(defparameter *default-text-align*      :left)

;; default parameter for paragraph
#|
#|EXPORT|#				:*default-paragraph-align*
#|EXPORT|#				:*default-paragraph-valign*
 |#
(defparameter *default-paragraph-align*        :left)
(defparameter *default-paragraph-valign*       :top)

#|
#|EXPORT|#				:*default-output-encoding*
#|EXPORT|#				:*default-history-count*
 |#
(defparameter *default-output-encoding*        :utf8)
(defparameter *default-history-count*          9)

