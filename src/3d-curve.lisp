#|
#|ASD|#                (:file "3d-curve"                  :depends-on ("kaavio"
#|ASD|#                                                                "constants"
#|ASD|#                                                                "point"
#|ASD|#                                                                "mathutil"
#|ASD|#                                                                "stroke-info"
#|ASD|#                                                                "endmark-info"
#|ASD|#                                                                "entity"
#|ASD|#                                                                "filter"
#|ASD|#                                                                "writer"))
#|EXPORT|#                ;3d-curve.lisp
 |#

(in-package :kaavio)


;;------------------------------------------------------------------------------
;;
;; class 3d-curve
;;
;;------------------------------------------------------------------------------
(defclass 3d-curve (entity)
  ((points  :initform nil :initarg :points)     ; list of point (pt1 ptC1 ptC2 pt2 [ptC3 pt3...])
   (end1    :initform nil :initarg :end1)       ; keyword
   (end2    :initform nil :initarg :end2)       ; keyword
   (stroke  :initform nil :initarg :stroke)     ; (or nil stroke-info)
   (filter  :initform nil :initarg :filter)     ; (or nil stroke-info)
   (debug   :initform nil :initarg :debug)))    ; (or nil t keyword)


(defmethod initialize-instance :after ((ent 3d-curve) &rest initargs)
  (declare (ignore initargs))
  (with-slots (end1 end2 stroke filter layer debug) ent
    (setf end1   (make-endmark (or end1   *default-endmark-1*)))
    (setf end2   (make-endmark (or end2   *default-endmark-2*)))
    (setf stroke (make-stroke (or stroke *default-stroke* :none)))
    (setf filter (if (eq filter :none)
                     nil
                     (or filter *default-filter*)))
    (setf layer  (if (eq layer :none)
                     nil
                     (or layer *default-layer*)))
    (when debug
      (setf debug (if (keywordp debug) debug :red))))
  ent)

;; type := :from|:dest
;; returns : (cons point point)
(defun 3d-curve-get-endpoints (points type)
  (type-assert type keyword)
  (check-keywords type :from :dest)
  (if (eq type :from)
      (cons (second points) (first points))
      (let ((pts points)
            (cnt (length points)))
        (dotimes (x (- cnt 2))
          (setf pts (cdr pts)))
        (cons (first pts) (second pts)))))

(defun 3d-curve-get-ctrl-points (points)
  (labels ((impl (lst acc)
             (if (null lst)
                 (nreverse acc)
                 (progn
                   (push (pt- (pt* (first acc) 2) (second acc)) acc)
                   (push (first  lst) acc)
                   (push (second lst) acc)
                   (impl (cddr lst) acc)))))
    (impl (nthcdr 4 points) (list (fourth points)
                                  (third  points)
                                  (second points)
                                  (first  points)))))

(defun 3d-curve-draw-controls (color ctrl-pts writer)
  (let ((stroke (make-stroke :color color :dasharray '(2 3)))
        (fill   (make-fill   :color color)))
    (labels ((draw-ctrl-lines (pts)
               (when (<= 4 (length pts))
                 (draw-line-impl (first pts) (second pts))
                 (draw-line-impl (third pts) (fourth pts))
                 (draw-ctrl-lines (cdddr pts))))
             (draw-ctrl-points (pts)
               (when pts
                 (writer-write writer
                               "<circle "
                               "cx='" (point-x (car pts)) "' "
                               "cy='" (point-y (car pts)) "' "
                               "r='3' stroke='none' "
                               (to-property-strings fill)
                               "/>")
                 (draw-ctrl-points (cdr pts))))
             (draw-line-impl (pt1 pt2)
               (writer-write writer
                             "<polyline fill='none' "
                             (to-property-strings stroke)
                             "points='"
                             (with-output-to-string (st)
                               (format st "~A,~A ~A,~A"
                                       (coerce (point-x pt1) 'single-float)
                                       (coerce (point-y pt1) 'single-float)
                                       (coerce (point-x pt2) 'single-float)
                                       (coerce (point-y pt2) 'single-float)))
                             "' />")))
      (draw-ctrl-lines  ctrl-pts)
      (draw-ctrl-points ctrl-pts))))

(defmethod check ((ent 3d-curve) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (with-slots (points end1 end2 stroke filter debug) ent
    (check-member points :nullable nil :types list)
    (check-object end1   canvas dict :nullable   t :class endmark-info)
    (check-object end2   canvas dict :nullable   t :class endmark-info)
    (check-object stroke canvas dict :nullable nil :class stroke-info)
    (check-member filter :nullable   t :types keyword)
    (check-member debug  :nullable   t :types keyword)
    (when end1 (check end1 canvas dict))
    (when end2 (check end2 canvas dict))
    (let ((len (length points)))
      (unless (<= 4 len)
        (throw-exception "Less than 4 elements in points of 3d-curve."))
      (when (= 1 (mod len 2))
        (throw-exception "Odd elements in points of 3d-curve.")))
    (labels ((fix-points (lst acc)
               (if (null lst)
                   (nreverse acc)
                   (let ((pt (car lst)))
                     (unless (point-p pt)
                       (throw-exception "Invalid point '~A' in points of 3d-curve." pt))
                     (fix-points (cdr lst)
                                 (push (canvas-fix-point canvas pt) acc))))))
      (setf points (fix-points points nil))))
  nil)
 
(defmethod entity-composition-p ((ent 3d-curve))
  (or (slot-value ent 'end1)
      (slot-value ent 'end2)))

(defmethod draw-entity ((ent 3d-curve) writer)
  (with-slots (points end1 end2 stroke filter debug) ent
    (let ((id  (and (not (entity-composition-p ent))
                    (slot-value ent 'id))))
      (pre-draw ent writer)
      (labels ((write-point (pt st)
                 (format st " ~A ~A" (coerce (point-x pt) 'single-float)
                                     (coerce (point-y pt) 'single-float)))
               (format-path-data (pts st)
                 (format st "M")
                 (write-point (first pts) st)
                 (format-path-data-1 (cdr pts) st))
               (format-path-data-1 (pts st)
                 (format st "C")
                 (write-point (first  pts) st)
                 (write-point (second pts) st)
                 (write-point (third  pts) st)
                 (format-path-data-2 (cdddr pts) st))
               (format-path-data-2 (pts st)
                 (when pts
                   (format st "S")
                   (write-point (first pts) st)
                   (write-point (second pts) st)
                   (format-path-data-2 (cddr pts) st))))
        (writer-write writer
                      "<path "
                      (write-when (keywordp id) "id='" id "' ")
                      "fill='none' "
                      (to-property-strings stroke)
                      "d='" (with-output-to-string (st)
                              (format-path-data points st)) "' "
                      (write-when filter "filter='url(#" it ")' ")
                      "/>"))
      (let ((ctrl-pts (and (or end2 debug)
                           (3d-curve-get-ctrl-points points))))
        (when end1
          (draw-endmark end1 (3d-curve-get-endpoints ctrl-pts :from) stroke writer))
        (when end2
          (draw-endmark end2 (3d-curve-get-endpoints ctrl-pts :dest) stroke writer))
        (when debug
          (3d-curve-draw-controls debug ctrl-pts writer)))
      (post-draw ent writer)))
  nil)


(defmethod attribute-center ((ent 3d-curve))
  (pt/ (pt+ (attribute-end1 ent)
            (attribute-end2 ent)) 2))

(defmethod attribute-end1 ((ent 3d-curve))
  (first (slot-value ent 'points)))

(defmethod attribute-end2 ((ent 3d-curve))
  (car (last (slot-value ent 'points))))


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro 3d-curve
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{3d-curve}} points ${KEY} stroke end1 end2 layer filter id debug
;;
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　${{TODO}{まだ記述されていません。}}
;;
;;${NO_SEE_ALSO}
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :3d-curve
 |#
(defmacro 3d-curve (points &key stroke end1 end2 layer filter id debug)
  `(register-entity (make-instance 'kaavio:3d-curve
                                   :points ,points
                                   :end1 ,end1 :end2 ,end2
                                   :stroke ,stroke :filter ,filter
                                   :layer ,layer :id ,id :debug ,debug)))

