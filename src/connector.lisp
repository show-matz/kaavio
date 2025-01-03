#|
#|ASD|#             (:file "connector"                 :depends-on ("kaavio"
#|ASD|#                                                             "constants"
#|ASD|#                                                             "line"
#|ASD|#                                                             "shape"
#|ASD|#                                                             "dictionary"
#|ASD|#                                                             "rectangle"
#|ASD|#                                                             "writer"))
#|EXPORT|#              ;connector.lisp
 |#

(in-package :kaavio)


;;------------------------------------------------------------------------------
;;
;; utility functions
;;
;;------------------------------------------------------------------------------
(defun check-and-fix-connector-style (style-kwd)
  (type-assert style-kwd keyword)
  (if (eq style-kwd :CC)
      (list :CC 0 0)
      (let (c0 c1 c2 c3
            (tmp (symbol-name style-kwd)))
        (symbol-macrolet ((x0 (char tmp 0)) (x1 (char tmp 1))
                          (x2 (char tmp 2)) (x3 (char tmp 3)))
          (let ((ret (case (length tmp)
                       (2 (setf c0 x0 c1 #\2 c2 x1 c3 #\2) t)
                       (3 (if (find x1 "123")
                              (setf c0 x0 c1  x1 c2 x2 c3 #\2)
                              (setf c0 x0 c1 #\2 c2 x1 c3  x2)) t)
                       (4 (setf c0 x0 c1 x1 c2 x2 c3 x3) t)
                       (t nil))))
            (setf ret (and ret (find c0 "TLBR")
                               (find c1  "123")
                               (find c2 "TLBR")
                               (find c3  "123")))
            (if (null ret)
                (throw-exception "Invalid line-style '~A'" style-kwd)
                (list (intern (with-output-to-string (s)
                                (princ c0 s)
                                (princ c2 s)) :keyword)
                      (- (char-code c1) (char-code #\0))
                      (- (char-code c3) (char-code #\0)))))))))


(defun resolve-connector-points-for-CC (pos1 pos2 e1 e2 spacing)
  (declare (ignore pos1 pos2 spacing))
  (let ((pt1 (shape-connect-point e1 :from :center (shape-cc-center e2 :dest)))
        (pt2 (shape-connect-point e2 :dest :center (shape-cc-center e1 :from))))
    (list pt1 pt2)))

(symbol-macrolet ((left1   (point-x (attribute-left   e1)))
                  (center1 (point-x (attribute-center e1)))
                  (right1  (point-x (attribute-right  e1)))
                  (top1    (point-y (attribute-top    e1)))
                  (middle1 (point-y (attribute-center e1)))
                  (bottom1 (point-y (attribute-bottom e1)))
                  (left2   (point-x (attribute-left   e2)))
                  (center2 (point-x (attribute-center e2)))
                  (right2  (point-x (attribute-right  e2)))
                  (top2    (point-y (attribute-top    e2)))
                  (middle2 (point-y (attribute-center e2)))
                  (bottom2 (point-y (attribute-bottom e2))))

  (defun resolve-connector-points-for-BB (pos1 pos2 e1 e2 spacing)
    (let ((dist *default-connector-spacing*)
          (pt1  (shape-connect-point e1 :from :bottom pos1))
          (pt2  (shape-connect-point e2 :dest :bottom pos2)))
      (with-point (x1 y1) pt1
        (with-point (x2 y2) pt2
            (labels ((points4 ()
                       (let ((y3 (+ (max y1 y2) dist (or (first spacing) 0))))
                         (list pt1 (make-point x1 y3 :absolute)
                                   (make-point x2 y3 :absolute) pt2)))
                     (points6 (x3 y3 y4)
                       (let ((x3 (if (< x3 x1)
                                     (- x3 (or (second spacing) 0))
                                     (+ x3 (or (second spacing) 0))))
                             (y3 (+ y3 (or (first  spacing) 0)))
                             (y4 (if (< y4 y3)
                                     (- y4 (or (third spacing) 0))
                                     (+ y4 (or (third spacing) 0)))))
                         (list pt1 (make-point x1 y3 :absolute)
                                   (make-point x3 y3 :absolute)
                                   (make-point x3 y4 :absolute)
                                   (make-point x2 y4 :absolute) pt2))))
              (cond
                ((and (< y1 top2) (< left2 x1 right2))
                 (points6 (let ((lx (- left2  dist))
                                (rx (+ right2 dist)))
                            (if (< (+ x1 x2) (+ lx rx)) lx rx))
                          (/ (+ y1 top2) 2) (+ y2 dist)))
                ((and (< y2 top1) (< left1 x2 right1))
                 (points6 (let ((lx (- left1  dist))
                                (rx (+ right1 dist)))
                            (if (< (+ x1 x2) (+ lx rx)) lx rx))
                          (+ y1 dist) (/ (+ top1 y2) 2)))
                (t (points4))))))))

  (defun resolve-connector-points-for-TT (pos1 pos2 e1 e2 spacing)
    (let ((dist *default-connector-spacing*)
          (pt1  (shape-connect-point e1 :from :top pos1))
          (pt2  (shape-connect-point e2 :dest :top pos2)))
      (with-point (x1 y1) pt1
        (with-point (x2 y2) pt2
            (labels ((points4 ()
                       (let ((y3 (- (min y1 y2) dist (or (first spacing) 0))))
                         (list pt1 (make-point x1 y3 :absolute)
                                   (make-point x2 y3 :absolute) pt2)))
                     (points6 (x3 y3 y4)
                       (let ((x3 (if (< x3 x1)
                                     (- x3 (or (second spacing) 0))
                                     (+ x3 (or (second spacing) 0))))
                             (y3 (- y3 (or (first  spacing) 0)))
                             (y4 (if (< y4 y3)
                                     (- y4 (or (third spacing) 0))
                                     (+ y4 (or (third spacing) 0)))))
                         (list pt1 (make-point x1 y3 :absolute)
                                   (make-point x3 y3 :absolute)
                                   (make-point x3 y4 :absolute)
                                   (make-point x2 y4 :absolute) pt2))))
              (cond
                ((and (< bottom1 y2) (< left1 x2 right1))
                 (points6 (let ((lx (- left1  dist))
                                (rx (+ right1 dist)))
                            (if (< (+ x1 x2) (+ lx rx)) lx rx))
                          (- y1 dist) (/ (+ bottom1 y2) 2)))
                ((and (< bottom2 y1) (< left2 x1 right2))
                 (points6 (let ((lx (- left2  dist))
                                (rx (+ right2 dist)))
                            (if (< (+ x1 x2) (+ lx rx)) lx rx))
                          (/ (+ y1 bottom2) 2) (- y2 dist)))
                (t (points4))))))))


  (defun resolve-connector-points-for-LL (pos1 pos2 e1 e2 spacing)
    (let ((dist *default-connector-spacing*)
          (pt1  (shape-connect-point e1 :from :left pos1))
          (pt2  (shape-connect-point e2 :dest :left pos2)))
      (with-point (x1 y1) pt1
        (with-point (x2 y2) pt2
            (labels ((points4 ()
                       (let ((x3 (- (min x1 x2) dist (or (first spacing) 0))))
                         (list pt1 (make-point x3 y1 :absolute)
                                   (make-point x3 y2 :absolute) pt2)))
                     (points6 (y3 x3 x4)
                       (let ((y3 (if (< y3 y1)
                                     (- y3 (or (second spacing) 0))
                                     (+ y3 (or (second spacing) 0))))
                             (x3 (- x3 (or (first  spacing) 0)))
                             (x4 (if (< x4 x3)
                                     (- x4 (or (third spacing) 0))
                                     (+ x4 (or (third spacing) 0)))))
                         (list pt1 (make-point x3 y1 :absolute)
                                   (make-point x3 y3 :absolute)
                                   (make-point x4 y3 :absolute)
                                   (make-point x4 y2 :absolute) pt2))))
              (cond
                ((and (< right1 x2) (< top1 y2 bottom1))
                 (points6 (let ((ty (- top1    dist))
                                (by (+ bottom1 dist)))
                            (if (< (+ y1 y2) (+ ty by)) ty by))
                          (- x1 dist) (/ (+ right1 x2) 2)))
                ((and (< right2 x1) (< top2 y1 bottom2))
                 (points6 (let ((ty (- top2    dist))
                                (by (+ bottom2 dist)))
                            (if (< (+ y1 y2) (+ ty by)) ty by))
                          (/ (+ right2 x1) 2) (- x2 dist)))
                (t (points4))))))))

  (defun resolve-connector-points-for-RR (pos1 pos2 e1 e2 spacing)
    (let ((dist *default-connector-spacing*)
          (pt1  (shape-connect-point e1 :from :right pos1))
          (pt2  (shape-connect-point e2 :dest :right pos2)))
      (with-point (x1 y1) pt1
        (with-point (x2 y2) pt2
            (labels ((points4 ()
                       (let ((x3 (+ (max x1 x2) dist (or (first spacing) 0))))
                         (list pt1 (make-point x3 y1 :absolute)
                                   (make-point x3 y2 :absolute) pt2)))
                     (points6 (y3 x3 x4)
                       (let ((y3 (if (< y3 y1)
                                     (- y3 (or (second spacing) 0))
                                     (+ y3 (or (second spacing) 0))))
                             (x3 (+ x3 (or (first  spacing) 0)))
                             (x4 (if (< x4 x3)
                                     (- x4 (or (third spacing) 0))
                                     (+ x4 (or (third spacing) 0)))))
                         (list pt1 (make-point x3 y1 :absolute)
                                   (make-point x3 y3 :absolute)
                                   (make-point x4 y3 :absolute)
                                   (make-point x4 y2 :absolute) pt2))))
              (cond
                ((and (< x1 left2) (< top2 y1 bottom2))
                 (points6 (let ((ty (- top2    dist))
                                (by (+ bottom2 dist)))
                            (if (< (+ y1 y2) (+ ty by)) ty by))
                          (/ (+ x1 left2) 2) (+ x2 dist)))
                ((and (< x2 left1) (< top1 y2 bottom1))
                 (points6 (let ((ty (- top1    dist))
                                (by (+ bottom1 dist)))
                            (if (< (+ y1 y2) (+ ty by)) ty by))
                          (+ x1 dist) (/ (+ x2 left1) 2)))
                (t (points4))))))))

  (defun resolve-connector-points-for-BT (pos1 pos2 e1 e2 spacing)
    (let ((dist *default-connector-spacing*)
          (pt1  (shape-connect-point e1 :from :bottom pos1))
          (pt2  (shape-connect-point e2 :dest :top    pos2)))
      (with-point (x1 y1) pt1
        (with-point (x2 y2) pt2
            (labels ((points2 ()
                       (list pt1 pt2))
                     (points4 ()
                       (let ((y3 (+ (/ (+ y1 y2) 2) (or (first spacing) 0))))
                         (list pt1 (make-point x1 y3 :absolute)
                                   (make-point x2 y3 :absolute) pt2)))
                     (points6 (x3 y3 y4)
                       (let ((x3 (if (< x3 x1)
                                     (- x3 (or (second spacing) 0))
                                     (+ x3 (or (second spacing) 0))))
                             (y3 (+ y3 (or (first spacing) 0)))
                             (y4 (- y4 (or (third spacing) 0))))
                         (list pt1 (make-point x1 y3 :absolute)
                                   (make-point x3 y3 :absolute)
                                   (make-point x3 y4 :absolute)
                                   (make-point x2 y4 :absolute) pt2))))
              (if (< y1 y2)
                  (if (= x1 x2)
                      (points2)
                      (if (< dist (- y2 y1))
                          (points4)
                          (points6 (/ (+ center1 center2) 2) (+ y1 dist) (- y2 dist))))
                  (if (< right2 left1)
                      (points6 (/ (+ right2 left1) 2) (+ y1 dist) (- y2 dist))
                      (if (< right1 left2)
                          (points6 (/ (+ right1 left2) 2) (+ y1 dist) (- y2 dist))
                          (let ((lx (- (min left1  left2)  dist))
                                (rx (+ (max right1 right2) dist)))
                            ;; (x1 - lx) + (x2 - lx) < (rx - x1) + (rx - x2)
                            ;; -> x1 + x2 - 2lx < 2rx - x1 - x2
                            ;; -> 2x1 + 2x2 < 2rx + 2lx
                            ;; -> x1 + x2 < rx + lx
                            (points6 (if (< (+ x1 x2)
                                            (+ rx lx)) lx rx)
                                     (+ y1 dist) (- y2 dist)))))))))))

  (defun resolve-connector-points-for-TB (pos1 pos2 e1 e2 spacing)
    (let ((dist *default-connector-spacing*)
          (pt1  (shape-connect-point e1 :from :top    pos1))
          (pt2  (shape-connect-point e2 :dest :bottom pos2)))
      (with-point (x1 y1) pt1
        (with-point (x2 y2) pt2
            (labels ((points2 ()
                       (list pt1 pt2))
                     (points4 ()
                       (let ((y3 (- (/ (+ y1 y2) 2) (or (first spacing) 0))))
                         (list pt1 (make-point x1 y3 :absolute)
                                   (make-point x2 y3 :absolute) pt2)))
                     (points6 (x3 y3 y4)
                       (let ((x3 (if (< x3 x1)
                                     (- x3 (or (second spacing) 0))
                                     (+ x3 (or (second spacing) 0))))
                             (y3 (- y3 (or (first spacing) 0)))
                             (y4 (+ y4 (or (third spacing) 0))))
                         (list pt1 (make-point x1 y3 :absolute)
                                   (make-point x3 y3 :absolute)
                                   (make-point x3 y4 :absolute)
                                   (make-point x2 y4 :absolute) pt2))))
              (if (< y2 y1)
                  (if (= x1 x2)
                      (points2)
                      (if (< dist (- y1 y2))
                          (points4)
                          (points6 (/ (+ center1 center2) 2) (- y1 dist) (+ y2 dist))))
                  (if (< right1 left2)
                      (points6 (/ (+ right1 left2) 2) (- y1 dist) (+ y2 dist))
                      (if (< right2 left1)
                          (points6 (/ (+ right2 left1) 2) (- y1 dist) (+ y2 dist))
                          (let ((lx (- (min left1  left2)  dist))
                                (rx (+ (max right1 right2) dist)))
                            ;; (x1 - lx) + (x2 - lx) < (rx - x1) + (rx - x2)
                            ;; -> x1 + x2 - 2lx < 2rx - x1 - x2
                            ;; -> 2x1 + 2x2 < 2rx + 2lx
                            ;; -> x1 + x2 < rx + lx
                            (points6 (if (< (+ x1 x2)
                                            (+ rx lx)) lx rx)
                                     (- y1 dist) (+ y2 dist)))))))))))

  (defun resolve-connector-points-for-LR (pos1 pos2 e1 e2 spacing)
    (let ((dist *default-connector-spacing*)
          (pt1  (shape-connect-point e1 :from :left  pos1))
          (pt2  (shape-connect-point e2 :dest :right pos2)))
      (with-point (x1 y1) pt1
        (with-point (x2 y2) pt2
            (labels ((points2 ()
                       (list pt1 pt2))
                     (points4 ()
                       (let ((x3 (- (/ (+ x1 x2) 2) (or (first spacing) 0))))
                         (list pt1 (make-point x3 y1 :absolute)
                                   (make-point x3 y2 :absolute) pt2)))
                     (points6 (y3 x3 x4)
                       (let ((y3 (if (< y3 y1)
                                     (- y3 (or (second spacing) 0))
                                     (+ y3 (or (second spacing) 0))))
                             (x3 (- x3 (or (first spacing) 0)))
                             (x4 (+ x4 (or (third spacing) 0))))
                         (list pt1 (make-point x3 y1 :absolute)
                                   (make-point x3 y3 :absolute)
                                   (make-point x4 y3 :absolute)
                                   (make-point x4 y2 :absolute) pt2))))
              (if (< x2 x1)
                  (if (= y1 y2)
                      (points2)
                      (if (< dist (- x1 x2))
                          (points4)
                          (points6 (/ (+ middle1 middle2) 2) (- x1 dist) (+ x2 dist))))
                  (if (< bottom1 top2)
                      (points6 (/ (+ bottom1 top2) 2) (- x1 dist) (+ x2 dist))
                      (if (< bottom2 top1)
                          (points6 (/ (+ bottom2 top1) 2) (- x1 dist) (+ x2 dist))
                          (let ((ty (- (min top1  top2)  dist))
                                (by (+ (max bottom1 bottom2) dist)))
                            ;; (y1 - ty) + (y2 - ty) < (by - y1) + (by - y2)
                            ;; -> y1 + y2 - 2ty < 2by - y1 - y2
                            ;; -> 2y1 + 2y2 < 2ty + 2by
                            ;; -> y1 + y2 < ty + by
                            (points6 (if (< (+ y1 y2)
                                            (+ ty by)) ty by)
                                     (- x1 dist) (+ x2 dist)))))))))))

  (defun resolve-connector-points-for-RL (pos1 pos2 e1 e2 spacing)
    (let ((dist *default-connector-spacing*)
          (pt1  (shape-connect-point e1 :from :right pos1))
          (pt2  (shape-connect-point e2 :dest :left  pos2)))
      (with-point (x1 y1) pt1
        (with-point (x2 y2) pt2
            (labels ((points2 ()
                       (list pt1 pt2))
                     (points4 ()
                       (let ((x3 (+ (/ (+ x1 x2) 2) (or (first spacing) 0))))
                         (list pt1 (make-point x3 y1 :absolute)
                                   (make-point x3 y2 :absolute) pt2)))
                     (points6 (y3 x3 x4)
                       (let ((y3 (if (< y3 y1)
                                     (- y3 (or (second spacing) 0))
                                     (+ y3 (or (second spacing) 0))))
                             (x3 (+ x3 (or (first spacing) 0)))
                             (x4 (- x4 (or (third spacing) 0))))
                         (list pt1 (make-point x3 y1 :absolute)
                                   (make-point x3 y3 :absolute)
                                   (make-point x4 y3 :absolute)
                                   (make-point x4 y2 :absolute) pt2))))
              (if (< x1 x2)
                  (if (= y1 y2)
                      (points2)
                      (if (< dist (- x2 x1))
                          (points4)
                          (points6 (/ (+ middle1 middle2) 2) (+ x1 dist) (- x2 dist))))
                  (if (< bottom2 top1)
                      (points6 (/ (+ bottom2 top1) 2) (+ x1 dist) (- x2 dist))
                      (if (< bottom1 top2)
                          (points6 (/ (+ bottom1 top2) 2) (+ x1 dist) (- x2 dist))
                          (let ((ty (- (min top1  top2)  dist))
                                (by (+ (max bottom1 bottom2) dist)))
                            ;; (y1 - ty) + (y2 - ty) < (by - y1) + (by - y2)
                            ;; -> y1 + y2 - 2ty < 2by - y1 - y2
                            ;; -> 2y1 + 2y2 < 2ty + 2by
                            ;; -> y1 + y2 < ty + by
                            (points6 (if (< (+ y1 y2)
                                            (+ ty by)) ty by)
                                     (+ x1 dist) (- x2 dist)))))))))))

  (defun resolve-connector-points-for-BL (pos1 pos2 e1 e2 spacing)
    (let ((dist *default-connector-spacing*)
          (pt1  (shape-connect-point e1 :from :bottom pos1))
          (pt2  (shape-connect-point e2 :dest :left   pos2)))
      (with-point (x1 y1) pt1
        (with-point (x2 y2) pt2
            (labels ((points3 ()
                       (list pt1 (make-point x1 y2 :absolute) pt2))
                     (points5 (x y)
                       (let ((x3 (- x (or (second spacing) 0)))
                             (y3 (+ y (or (first  spacing) 0))))
                         (list pt1 (make-point x1 y3 :absolute)
                                   (make-point x3 y3 :absolute)
                                   (make-point x3 y2 :absolute) pt2))))
              (if (and (< y1 y2) (< x1 x2))
                  (points3)
                  (points5 (if (< right1 x2)
                               (/ (+ right1 x2) 2)
                               (- (min left1 x2) dist))
                           (if (< y1 top2)
                               (/ (+ y1 top2) 2)
                               (+ (max y1 bottom2) dist)))))))))

  (defun resolve-connector-points-for-BR (pos1 pos2 e1 e2 spacing)
    (let ((dist *default-connector-spacing*)
          (pt1  (shape-connect-point e1 :from :bottom pos1))
          (pt2  (shape-connect-point e2 :dest :right  pos2)))
      (with-point (x1 y1) pt1
        (with-point (x2 y2) pt2
            (labels ((points3 ()
                       (list pt1 (make-point x1 y2 :absolute) pt2))
                     (points5 (x y)
                       (let ((x3 (+ x (or (second spacing) 0)))
                             (y3 (+ y (or (first  spacing) 0))))
                         (list pt1 (make-point x1 y3 :absolute)
                                   (make-point x3 y3 :absolute)
                                   (make-point x3 y2 :absolute) pt2))))
              (if (and (< y1 y2) (< x2 x1))
                  (points3)
                  (points5 (if (< x2 left1)
                               (/ (+ x2 left1) 2)
                               (+ (max right1 x2) dist))
                           (if (< y1 top2)
                               (/ (+ y1 top2) 2)
                               (+ (max y1 bottom2) dist)))))))))

  (defun resolve-connector-points-for-TL (pos1 pos2 e1 e2 spacing)
    (let ((dist *default-connector-spacing*)
          (pt1  (shape-connect-point e1 :from :top  pos1))
          (pt2  (shape-connect-point e2 :dest :left pos2)))
      (with-point (x1 y1) pt1
        (with-point (x2 y2) pt2
            (labels ((points3 ()
                       (list pt1 (make-point x1 y2 :absolute) pt2))
                     (points5 (x y)
                       (let ((x3 (- x (or (second spacing) 0)))
                             (y3 (- y (or (first  spacing) 0))))
                         (list pt1 (make-point x1 y3 :absolute)
                                   (make-point x3 y3 :absolute)
                                   (make-point x3 y2 :absolute) pt2))))
              (if (and (< y2 y1) (< x1 x2))
                  (points3)
                  (points5 (if (< right1 x2)
                               (/ (+ right1 x2) 2)
                               (- (min left1 x2) dist))
                           (if (< bottom2 y1)
                               (/ (+ bottom2 y1) 2)
                               (- (min y1 top2) dist)))))))))

  (defun resolve-connector-points-for-TR (pos1 pos2 e1 e2 spacing)
    (let ((dist *default-connector-spacing*)
          (pt1  (shape-connect-point e1 :from :top   pos1))
          (pt2  (shape-connect-point e2 :dest :right pos2)))
      (with-point (x1 y1) pt1
        (with-point (x2 y2) pt2
            (labels ((points3 ()
                       (list pt1 (make-point x1 y2 :absolute) pt2))
                     (points5 (x y)
                       (let ((x3 (+ x (or (second spacing) 0)))
                             (y3 (- y (or (first  spacing) 0))))
                         (list pt1 (make-point x1 y3 :absolute)
                                   (make-point x3 y3 :absolute)
                                   (make-point x3 y2 :absolute) pt2))))
              (if (and (< y2 y1) (< x2 x1))
                  (points3)
                  (points5 (if (< x2 left1)
                               (/ (+ x2 left1) 2)
                               (+ (max right1 x2) dist))
                           (if (< bottom2 y1)
                               (/ (+ y1 bottom2) 2)
                               (- (min y1 top2) dist)))))))))

  (defun resolve-connector-points-for-LB (pos1 pos2 e1 e2 spacing)
    (let ((dist *default-connector-spacing*)
          (pt1  (shape-connect-point e1 :from :left   pos1))
          (pt2  (shape-connect-point e2 :dest :bottom pos2)))
      (with-point (x1 y1) pt1
        (with-point (x2 y2) pt2
            (labels ((points3 ()
                       (list pt1 (make-point x2 y1 :absolute) pt2))
                     (points5 (x y)
                       (let ((x3 (- x (or (first  spacing) 0)))
                             (y3 (+ y (or (second spacing) 0))))
                         (list pt1 (make-point x3 y1 :absolute)
                                   (make-point x3 y3 :absolute)
                                   (make-point x2 y3 :absolute) pt2))))
              (if (and (< x2 x1) (< y2 y1))
                  (points3)
                  (points5 (if (< right2 x1)
                               (/ (+ right2 x1) 2)
                               (- (min x1 left2) dist))
                           (if (< y2 top1)
                               (/ (+ y2 top1) 2)
                               (+ (max bottom1 y2) dist)))))))))

  (defun resolve-connector-points-for-LT (pos1 pos2 e1 e2 spacing)
    (let ((dist *default-connector-spacing*)
          (pt1  (shape-connect-point e1 :from :left pos1))
          (pt2  (shape-connect-point e2 :dest :top  pos2)))
      (with-point (x1 y1) pt1
        (with-point (x2 y2) pt2
            (labels ((points3 ()
                       (list pt1 (make-point x2 y1 :absolute) pt2))
                     (points5 (x y)
                       (let ((x3 (- x (or (first  spacing) 0)))
                             (y3 (- y (or (second spacing) 0))))
                         (list pt1 (make-point x3 y1 :absolute)
                                   (make-point x3 y3 :absolute)
                                   (make-point x2 y3 :absolute) pt2))))
              (if (and (< x2 x1) (< y1 y2))
                  (points3)
                  (points5 (if (< right2 x1)
                               (/ (+ right2 x1) 2)
                               (- (min x1 left2) dist))
                           (if (< bottom1 y2)
                               (/ (+ bottom1 y2) 2)
                               (- (min top1 y2) dist)))))))))

  (defun resolve-connector-points-for-RB (pos1 pos2 e1 e2 spacing)
    (let ((dist *default-connector-spacing*)
          (pt1  (shape-connect-point e1 :from :right  pos1))
          (pt2  (shape-connect-point e2 :dest :bottom pos2)))
      (with-point (x1 y1) pt1
        (with-point (x2 y2) pt2
            (labels ((points3 ()
                       (list pt1 (make-point x2 y1 :absolute) pt2))
                     (points5 (x y)
                       (let ((x3 (+ x (or (first  spacing) 0)))
                             (y3 (+ y (or (second spacing) 0))))
                         (list pt1 (make-point x3 y1 :absolute)
                                   (make-point x3 y3 :absolute)
                                   (make-point x2 y3 :absolute) pt2))))
              (if (and (< x1 x2) (< y2 y1))
                  (points3)
                  (points5 (if (< x1 left2)
                               (/ (+ x1 left2) 2)
                               (+ (max x1 right2) dist))
                           (if (< y2 top1)
                               (/ (+ y2 top1) 2)
                               (+ (max bottom1 y2) dist)))))))))

  (defun resolve-connector-points-for-RT (pos1 pos2 e1 e2 spacing)
    (let ((dist *default-connector-spacing*)
          (pt1  (shape-connect-point e1 :from :right pos1))
          (pt2  (shape-connect-point e2 :dest :top   pos2)))
      (with-point (x1 y1) pt1
        (with-point (x2 y2) pt2
            (labels ((points3 ()
                       (list pt1 (make-point x2 y1 :absolute) pt2))
                     (points5 (x y)
                       (let ((x3 (+ x (or (first  spacing) 0)))
                             (y3 (- y (or (second spacing) 0))))
                         (list pt1 (make-point x3 y1 :absolute)
                                   (make-point x3 y3 :absolute)
                                   (make-point x2 y3 :absolute) pt2))))
              (if (and (< x1 x2) (< y1 y2))
                  (points3)
                  (points5 (if (< x1 left2)
                               (/ (+ x1 left2) 2)
                               (+ (max x1 right2) dist))
                           (if (< bottom1 y2)
                               (/ (+ bottom1 y2) 2)
                               (- (min top1 y2) dist))))))))))


#|
#|EXPORT|#              :resolve-connector-points
 |#
(defun resolve-connector-points (from dest style spacing)
  (labels ((create-dummy-rect (x y)
             (make-instance 'kaavio:rectangle
                            :position (make-point x y :absolute)
                            :width 0.01 :height 0.01 :rx 0 :ry 0))    ; 0.01 ... OK?
           (fix-target (target)
             (cond
               ((point-p target)
                (with-point (x y) target
                  (create-dummy-rect x y)))
               ((typep target 'line)
                (multiple-value-bind (x y) (line-get-center target)
                  (create-dummy-rect x y)))
               (t target))))
    (setf from (fix-target from))
    (setf dest (fix-target dest)))
  (when (keywordp style)
    (setf style (check-and-fix-connector-style style)))
  (type-assert style list)
  (let ((style (first  style))
        (pos1  (- (second style) 2))
        (pos2  (- (third  style) 2)))
    (ecase style
      ((:CC) (resolve-connector-points-for-CC pos1 pos2 from dest spacing))
      ((:BB) (resolve-connector-points-for-BB pos1 pos2 from dest spacing))
      ((:TT) (resolve-connector-points-for-TT pos1 pos2 from dest spacing))
      ((:LL) (resolve-connector-points-for-LL pos1 pos2 from dest spacing))
      ((:RR) (resolve-connector-points-for-RR pos1 pos2 from dest spacing))
      ((:BT) (resolve-connector-points-for-BT pos1 pos2 from dest spacing))
      ((:TB) (resolve-connector-points-for-TB pos1 pos2 from dest spacing))
      ((:LR) (resolve-connector-points-for-LR pos1 pos2 from dest spacing))
      ((:RL) (resolve-connector-points-for-RL pos1 pos2 from dest spacing))
      ((:BL) (resolve-connector-points-for-BL pos1 pos2 from dest spacing))
      ((:BR) (resolve-connector-points-for-BR pos1 pos2 from dest spacing))
      ((:TL) (resolve-connector-points-for-TL pos1 pos2 from dest spacing))
      ((:TR) (resolve-connector-points-for-TR pos1 pos2 from dest spacing))
      ((:LB) (resolve-connector-points-for-LB pos1 pos2 from dest spacing))
      ((:LT) (resolve-connector-points-for-LT pos1 pos2 from dest spacing))
      ((:RB) (resolve-connector-points-for-RB pos1 pos2 from dest spacing))
      ((:RT) (resolve-connector-points-for-RT pos1 pos2 from dest spacing))
)))




;;------------------------------------------------------------------------------
;;
;; class connector
;;
;;------------------------------------------------------------------------------
(defclass connector (line)
  ((from    :initform nil :initarg :from)      ; keyword
   (to      :initform nil :initarg :to)        ; keyword
   (spacing :initform nil :initarg :spacing)   ; list
   (style   :initform nil :initarg :style)))   ; keyword - :(CC|[BLRT][123]?[BLRT][123]?) (keyword->list)


(defmethod initialize-instance :after ((ent connector) &rest initargs)
  (declare (ignore initargs))
  (with-slots (style) ent
    (setf style (or style *default-connector-style* :CC)))
  ent)

(defmethod check ((ent connector) canvas dict)
  (with-slots (from to spacing style) ent
    (unless (point-p from)
      (check-member from  :nullable nil :types symbol))
    (unless (point-p to)
      (check-member to    :nullable nil :types symbol))
    (check-member spacing :nullable   t :types (or number list))
    (check-member style   :nullable nil :types keyword)
    (setf spacing (if (numberp spacing) (list spacing) spacing))
    (when (remove-if #'numberp spacing)
      (throw-exception "Invalid spacing : '~A'." spacing))
    (setf style (check-and-fix-connector-style style))
    (labels ((get-target (target)
               (if (point-p target)
                   target
                   (let ((entity (dict-get-entity dict target)))
                     (unless entity
                       (throw-exception "Entity '~A' not found in dictionary." target))
                     (unless (or (typep entity 'shape) (typep entity 'line))
                       (throw-exception "Entity '~A' is not shape nor line object." target))
                     entity))))
      (let ((from-target (get-target from))
            (dest-target (get-target   to)))
        (setf (slot-value ent 'points)
              (resolve-connector-points from-target dest-target style spacing))
        (call-next-method)))))

(defmethod write-header ((ent connector) writer)
  (with-slots (id from to) ent
    (writer-write writer "<!-- "
                         (write-when (keywordp id) id " : ")
                         (string-downcase (symbol-name (type-of ent)))
                         (write-when (keywordp from) " from " from)
                         (write-when (keywordp   to) " to " to)
                         " -->")))


;;------------------------------------------------------------------------------
;;
;; macro connector
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#              :connector
 |#
(defmacro connector (from to &key style spacing label stroke end1 end2 layer filter id)
  `(register-entity (make-instance 'kaavio:connector
                                   :from ,from :to ,to
                                   :style ,style :spacing ,spacing
                                   :label ,label :end1 ,end1 :end2 ,end2
                                   :stroke ,stroke :filter ,filter :layer ,layer :id ,id)))

#|
#|EXPORT|#              :connect
 |#
(defmacro connect (from to &key style spacing label stroke end1 end2 layer filter id)
  `(register-entity (make-instance 'kaavio:connector
                                   :from ,from :to ,to
                                   :style ,style :spacing ,spacing
                                   :label ,label :end1 ,end1 :end2 ,end2
                                   :stroke ,stroke :filter ,filter :layer ,layer :id ,id)))


