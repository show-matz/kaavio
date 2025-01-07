#|
#|ASD|#                (:file "font-info"                 :depends-on ("kaavio"
#|ASD|#                                                                "colormap"
#|ASD|#                                                                "fill-info"
#|ASD|#                                                                "stroke-info"))
#|EXPORT|#                ;font-info.lisp
 |#


(in-package :kaavio)

;; default parameter for font-info
#|
#|EXPORT|#                :*default-font*
#|EXPORT|#                :*mute-font*
#|EXPORT|#                :*default-font-fill*
#|EXPORT|#                :*default-font-stroke*
#|EXPORT|#                :*default-font-filter*
 |#
(defparameter *default-font*        nil)
(defparameter *mute-font*           nil)
(defparameter *default-font-fill*   nil)
(defparameter *default-font-stroke* nil)
(defparameter *default-font-filter* nil)

;;------------------------------------------------------------------------------
;;
;; font-info
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#                :font-info
 |#
(defclass font-info ()
  ((family          :initform nil :initarg :family)          ; string
   (size            :initform nil :initarg :size)            ; number
   (fill            :initform nil :initarg :fill)            ; (or nil fill-info)
   (stroke          :initform nil :initarg :stroke)          ; (or nil stroke-info)
   (style           :initform nil :initarg :style)           ; (or nil keyword)
                                                             ;    :normal :italic :oblique
   (decoration      :initform nil :initarg :decoration)      ; (or nil keyword)
                                                             ;  :none :underline :overline :line-through
   (weight          :initform nil :initarg :weight)          ; (or number keyword)
                                                             ;  :normal :bold :bolder :lighter
                                                             ;  100 200 300 400 500 600 700 800 900
   (filter          :initform nil :initarg :filter)          ; (or nil keyword)
   (line-spacing    :initform nil :initarg :line-spacing)    ; number
   (width-spice     :initform nil :initarg :width-spice)))   ; number


(defmethod initialize-instance :after ((font font-info) &rest initargs)
  (declare (ignore initargs))
  (with-slots (fill stroke filter) font
    (setf fill   (make-fill   (or fill   *default-font-fill*)))
    (setf stroke (when (or stroke *default-font-stroke*)
                   (make-stroke (or stroke *default-font-stroke*))))
    (setf filter (if (eq filter :none)
                     nil
                     (or filter *default-font-filter*))))
  font)

(defmethod check ((ent font-info) canvas dict)
  (with-slots (family size fill stroke style
                      decoration weight filter line-spacing width-spice) ent
    (check-member family       :nullable   t :types string)
    (check-member size         :nullable   t :types number)
    (check-object fill         canvas dict :nullable t :class   fill-info)
    (check-object stroke       canvas dict :nullable t :class stroke-info)
    (check-member style        :nullable   t :types keyword)
    (check-member decoration   :nullable   t :types keyword)
    (check-member weight       :nullable   t :types (or fixnum keyword))
    (check-member filter       :nullable   t :types keyword)
    (check-member line-spacing :nullable nil :types number)
    (check-member width-spice  :nullable nil :types number)
    (when style
      (check-keywords style :normal :italic :oblique))
    (when decoration
      (check-keywords decoration :none :underline :overline :line-through))
    (when (keywordp weight)
      (check-keywords weight :normal :bold :bolder :lighter))
    (when (numberp weight)
      (check-numbers weight 100 200 300 400 500 600 700 800 900)))
  nil)

(defmethod to-property-strings ((fnt font-info))
  (unless *mute-font*
    (macrolet ((add-when (member &rest args)
                 `(let ((it ,member))
                    (when it
                      (setf buf (concatenate 'string buf (format-string ,@args)))))))
      (let ((buf ""))
        (with-slots (family size fill stroke
                            style decoration weight filter) fnt
          (add-when family          "font-family='" it "' ")
          (add-when size              "font-size='" it "pt' ")
          (add-when fill       (to-property-strings it))
          (add-when stroke     (to-property-strings it))
          (add-when style            "font-style='" it "' ")
          (add-when decoration  "text-decoration='" it "' ")
          (add-when weight          "font-weight='" it "' ")
          (add-when filter          "filter='url(#" it ")' "))
        buf))))

#|
#|EXPORT|#                :make-font
 |#
(defun make-font (&rest params)
  (if (= 1 (length params))
      (let ((param (car params)))
        (cond
          ((typep param 'font-info) param)
          ((numberp param) (make-font :size   param))
          ((keywordp param) (make-font :fill  param))
          ((listp   param) (apply #'make-font param))
          (t               (make-font :family param))))
      (if (and (null params) *default-font*)
          *default-font*
          (destructuring-bind (&key (family       nil       family-p)
                                    (size         nil         size-p)
                                    (fill         nil         fill-p)
                                    (stroke       nil       stroke-p)
                                    (style        nil        style-p)
                                    (decoration   nil   decoration-p)
                                    (weight       nil       weight-p)
                                    (filter       nil       filter-p)
                                    (width-spice  nil  width-spice-p)
                                    (line-spacing nil line-spacing-p) base) params
            (let ((base (or base *default-font*)))
              (labels ((fixval (val-p val slot-sym default)
                         (if val-p
                             val
                             (if base
                                 (slot-value base slot-sym) default))))
                (make-instance 'font-info
                               :family       (fixval family-p       family       'family       nil)
                               :size         (fixval size-p         size         'size          12)
                               :fill         (fixval fill-p         fill         'fill      :black)
                               :stroke       (fixval stroke-p       stroke       'stroke       nil)
                               :style        (fixval style-p        style        'style        nil)
                               :decoration   (fixval decoration-p   decoration   'decoration   nil)
                               :weight       (fixval weight-p       weight       'weight       nil)
                               :filter       (fixval filter-p       filter       'filter       nil)
                               :width-spice  (fixval width-spice-p  width-spice  'width-spice 0.65)
                               :line-spacing (fixval line-spacing-p line-spacing 'line-spacing   2))))))))


#|
#|EXPORT|#                :font-calc-textarea
 |#
(defun font-calc-textarea (font text)
  (with-slots (size width-spice line-spacing) font
    (let* ((lst (string/split text #\newline))
           (line-count (length lst))
           (width-fnc  (lambda (line)
                         (* (length line) size width-spice))))    ;ToDo : what can I do ?
      ;;ToDo : implement... fix width-fnc.
      (values (apply #'max (mapcar width-fnc lst))
              (+ (* size line-count)
                 (* line-spacing (1- line-count)))))))


(setf *default-font* (make-font :family        nil
                                :size           12
                                :fill       :black
                                :stroke        nil
                                :style         nil
                                :decoration    nil
                                :weight        nil
                                :filter        nil
                                :width-spice  0.65
                                :line-spacing    2))

(setf *default-font-fill*   (make-fill :color :black :opacity 1.0))
(setf *default-font-stroke* nil)

;;(to-property-strings (make-font))
;;(to-property-strings (make-font "Courier New"))
;;(to-property-strings (make-font 18))
;;(to-property-strings (make-font :family "Vernada" :size 10))
;;(to-property-strings (make-font '(:family "Vernada" :size 10)))


