#|
#|ASD|#				(:file "connector"                 :depends-on ("cl-diagram"
#|ASD|#																"constants"
#|ASD|#																"line"
#|ASD|#																"shape"
#|ASD|#				                                                "dictionary"
#|ASD|#				                                                "rectangle"
#|ASD|#				                                                "writer"))
#|EXPORT|#				;connector.lisp
 |#

(in-package :cl-diagram)


;-------------------------------------------------------------------------------
;
; utility functions
;
;-------------------------------------------------------------------------------
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


(defun resolve-connector-points-for-CC (pos1 pos2 e1 e2)
  (declare (ignore pos1 pos2))
  (with-property
	(let ((pt1 (shape-connect-point e1 :center (make-point e2[center] e2[middle])))
		  (pt2 (shape-connect-point e2 :center (make-point e1[center] e1[middle]))))
	  (list (point-x pt1) (point-y pt1)
			(point-x pt2) (point-y pt2)))))

(defun resolve-connector-points-for-BB (pos1 pos2 e1 e2)
  (let ((dist *default-connector-spacing*))
	(with-point (x1 y1) (shape-connect-point e1 :bottom pos1)
	  (with-point (x2 y2) (shape-connect-point e2 :bottom pos2)
		(labels ((points4 ()
				   (let ((y3 (+ (max y1 y2) dist)))
					 (list x1 y1 x1 y3 x2 y3 x2 y2)))
				 (points6 (x3 y3 y4)
				   (list x1 y1 x1 y3 x3 y3 x3 y4 x2 y4 x2 y2)))
		  (with-property
			  (cond
				((and (< y1 e2[top]) (< e2[left] x1 e2[right]))
				 (points6 (let ((lx (- e2[left]  dist))
								(rx (+ e2[right] dist)))
							(if (< (+ x1 x2) (+ lx rx)) lx rx))
						  (/ (+ y1 e2[top]) 2) (+ y2 dist)))
				((and (< y2 e1[top]) (< e1[left] x2 e1[right]))
				 (points6 (let ((lx (- e1[left]  dist))
								(rx (+ e1[right] dist)))
							(if (< (+ x1 x2) (+ lx rx)) lx rx))
						  (+ y1 dist) (/ (+ e1[top] y2) 2)))
				(t (points4)))))))))

(defun resolve-connector-points-for-TT (pos1 pos2 e1 e2)
  (let ((dist *default-connector-spacing*))
	(with-point (x1 y1) (shape-connect-point e1 :top pos1)
	  (with-point (x2 y2) (shape-connect-point e2 :top pos2)
		(labels ((points4 ()
				   (let ((y3 (- (min y1 y2) dist)))
					 (list x1 y1 x1 y3 x2 y3 x2 y2)))
				 (points6 (x3 y3 y4)
				   (list x1 y1 x1 y3 x3 y3 x3 y4 x2 y4 x2 y2)))
		  (with-property
			  (cond
				((and (< e1[bottom] y2) (< e1[left] x2 e1[right]))
				 (points6 (let ((lx (- e1[left]  dist))
								(rx (+ e1[right] dist)))
							(if (< (+ x1 x2) (+ lx rx)) lx rx))
						  (- y1 dist) (/ (+ e1[bottom] y2) 2)))
				((and (< e2[bottom] y1) (< e2[left] x1 e2[right]))
				 (points6 (let ((lx (- e2[left]  dist))
								(rx (+ e2[right] dist)))
							(if (< (+ x1 x2) (+ lx rx)) lx rx))
						  (/ (+ y1 e2[bottom]) 2) (- y2 dist)))
				(t (points4)))))))))


(defun resolve-connector-points-for-LL (pos1 pos2 e1 e2)
  (let ((dist *default-connector-spacing*))
	(with-point (x1 y1) (shape-connect-point e1 :left pos1)
	  (with-point (x2 y2) (shape-connect-point e2 :left pos2)
		(labels ((points4 ()
				   (let ((x3 (- (min x1 x2) dist)))
					 (list x1 y1 x3 y1 x3 y2 x2 y2)))
				 (points6 (y3 x3 x4)
				   (list x1 y1 x3 y1 x3 y3 x4 y3 x4 y2 x2 y2)))
		  (with-property
			  (cond
				((and (< e1[right] x2) (< e1[top] y2 e1[bottom]))
				 (points6 (let ((ty (- e1[top]    dist))
								(by (+ e1[bottom] dist)))
							(if (< (+ y1 y2) (+ ty by)) ty by))
						  (- x1 dist) (/ (+ e1[right] x2) 2)))
				((and (< e2[right] x1) (< e2[top] y1 e2[bottom]))
				 (points6 (let ((ty (- e2[top]    dist))
								(by (+ e2[bottom] dist)))
							(if (< (+ y1 y2) (+ ty by)) ty by))
						  (/ (+ e2[right] x1) 2) (- x2 dist)))
				(t (points4)))))))))

(defun resolve-connector-points-for-RR (pos1 pos2 e1 e2)
  (let ((dist *default-connector-spacing*))
	(with-point (x1 y1) (shape-connect-point e1 :right pos1)
	  (with-point (x2 y2) (shape-connect-point e2 :right pos2)
		(labels ((points4 ()
				   (let ((x3 (+ (max x1 x2) dist)))
					 (list x1 y1 x3 y1 x3 y2 x2 y2)))
				 (points6 (y3 x3 x4)
				   (list x1 y1 x3 y1 x3 y3 x4 y3 x4 y2 x2 y2)))
		  (with-property
			  (cond
				((and (< x1 e2[left]) (< e2[top] y1 e2[bottom]))
				 (points6 (let ((ty (- e2[top]    dist))
								(by (+ e2[bottom] dist)))
							(if (< (+ y1 y2) (+ ty by)) ty by))
						  (/ (+ x1 e2[left]) 2) (+ x2 dist)))
				((and (< x2 e1[left]) (< e1[top] y2 e1[bottom]))
				 (points6 (let ((ty (- e1[top]    dist))
								(by (+ e1[bottom] dist)))
							(if (< (+ y1 y2) (+ ty by)) ty by))
						  (+ x1 dist) (/ (+ x2 e1[left]) 2)))
				(t (points4)))))))))

(defun resolve-connector-points-for-BT (pos1 pos2 e1 e2)
  (let ((dist *default-connector-spacing*))
	(with-point (x1 y1) (shape-connect-point e1 :bottom pos1)
	  (with-point (x2 y2) (shape-connect-point e2 :top pos2)
		(labels ((points2 ()
				   (list x1 y1 x2 y2))
				 (points4 ()
				   (let ((y3 (/ (+ y1 y2) 2)))
					 (list x1 y1 x1 y3 x2 y3 x2 y2)))
				 (points6 (x3 y3 y4)
				   (list x1 y1 x1 y3 x3 y3 x3 y4 x2 y4 x2 y2)))
		  (with-property
			  (if (< y1 y2)
				  (if (= x1 x2)
					  (points2)
					  (if (< dist (- y2 y1))
						  (points4)
						  (points6 (/ (+ e1[center] e2[center]) 2) (+ y1 dist) (- y2 dist))))
				  (if (< e2[right] e1[left])
					  (points6 (/ (+ e2[right] e1[left]) 2) (+ y1 dist) (- y2 dist))
					  (if (< e1[right] e2[left])
						  (points6 (/ (+ e1[right] e2[left]) 2) (+ y1 dist) (- y2 dist))
						  (let ((lx (- (min e1[left]  e2[left])  dist))
								(rx (+ (max e1[right] e2[right]) dist)))
							;; (x1 - lx) + (x2 - lx) < (rx - x1) + (rx - x2)
							;; -> x1 + x2 - 2lx < 2rx - x1 - x2
							;; -> 2x1 + 2x2 < 2rx + 2lx
							;; -> x1 + x2 < rx + lx
							(points6 (if (< (+ x1 x2)
											(+ rx lx)) lx rx) (+ y1 dist) (- y2 dist))))))))))))

(defun resolve-connector-points-for-TB (pos1 pos2 e1 e2)
  (let ((dist *default-connector-spacing*))
	(with-point (x1 y1) (shape-connect-point e1 :top pos1)
	  (with-point (x2 y2) (shape-connect-point e2 :bottom pos2)
		(labels ((points2 ()
				   (list x1 y1 x2 y2))
				 (points4 ()
				   (let ((y3 (/ (+ y1 y2) 2)))
					 (list x1 y1 x1 y3 x2 y3 x2 y2)))
				 (points6 (x3 y3 y4)
				   (list x1 y1 x1 y3 x3 y3 x3 y4 x2 y4 x2 y2)))
		  (with-property
			  (if (< y2 y1)
				  (if (= x1 x2)
					  (points2)
					  (if (< dist (- y1 y2))
						  (points4)
						  (points6 (/ (+ e1[center] e2[center]) 2) (- y1 dist) (+ y2 dist))))
				  (if (< e1[right] e2[left])
					  (points6 (/ (+ e1[right] e2[left]) 2) (- y1 dist) (+ y2 dist))
					  (if (< e2[right] e1[left])
						  (points6 (/ (+ e2[right] e1[left]) 2) (- y1 dist) (+ y2 dist))
						  (let ((lx (- (min e1[left]  e2[left])  dist))
								(rx (+ (max e1[right] e2[right]) dist)))
							;; (x1 - lx) + (x2 - lx) < (rx - x1) + (rx - x2)
							;; -> x1 + x2 - 2lx < 2rx - x1 - x2
							;; -> 2x1 + 2x2 < 2rx + 2lx
							;; -> x1 + x2 < rx + lx
							(points6 (if (< (+ x1 x2)
											(+ rx lx)) lx rx) (- y1 dist) (+ y2 dist))))))))))))

(defun resolve-connector-points-for-LR (pos1 pos2 e1 e2)
  (let ((dist *default-connector-spacing*))
	(with-point (x1 y1) (shape-connect-point e1 :left pos1)
	  (with-point (x2 y2) (shape-connect-point e2 :right pos2)
		(labels ((points2 ()
				   (list x1 y1 x2 y2))
				 (points4 ()
				   (let ((x3 (/ (+ x1 x2) 2)))
					 (list x1 y1 x3 y1 x3 y2 x2 y2)))
				 (points6 (y3 x3 x4)
				   (list x1 y1 x3 y1 x3 y3 x4 y3 x4 y2 x2 y2)))
		  (with-property
			  (if (< x2 x1)
				  (if (= y1 y2)
					  (points2)
					  (if (< dist (- x1 x2))
						  (points4)
						  (points6 (/ (+ e1[middle] e2[middle]) 2) (- x1 dist) (+ x2 dist))))
				  (if (< e1[bottom] e2[top])
					  (points6 (/ (+ e1[bottom] e2[top]) 2) (- x1 dist) (+ x2 dist))
					  (if (< e2[bottom] e1[top])
						  (points6 (/ (+ e2[bottom] e1[top]) 2) (- x1 dist) (+ x2 dist))
						  (let ((ty (- (min e1[top]  e2[top])  dist))
								(by (+ (max e1[bottom] e2[bottom]) dist)))
							;; (y1 - ty) + (y2 - ty) < (by - y1) + (by - y2)
							;; -> y1 + y2 - 2ty < 2by - y1 - y2
							;; -> 2y1 + 2y2 < 2ty + 2by
							;; -> y1 + y2 < ty + by
							(points6 (if (< (+ y1 y2)
											(+ ty by)) ty by) (- x1 dist) (+ x2 dist))))))))))))

(defun resolve-connector-points-for-RL (pos1 pos2 e1 e2)
  (let ((dist *default-connector-spacing*))
	(with-point (x1 y1) (shape-connect-point e1 :right pos1)
	  (with-point (x2 y2) (shape-connect-point e2 :left pos2)
		(labels ((points2 ()
				   (list x1 y1 x2 y2))
				 (points4 ()
				   (let ((x3 (/ (+ x1 x2) 2)))
					 (list x1 y1 x3 y1 x3 y2 x2 y2)))
				 (points6 (y3 x3 x4)
				   (list x1 y1 x3 y1 x3 y3 x4 y3 x4 y2 x2 y2)))
		  (with-property
			  (if (< x1 x2)
				  (if (= y1 y2)
					  (points2)
					  (if (< dist (- x2 x1))
						  (points4)
						  (points6 (/ (+ e1[middle] e2[middle]) 2) (+ x1 dist) (- x2 dist))))
				  (if (< e2[bottom] e1[top])
					  (points6 (/ (+ e2[bottom] e1[top]) 2) (+ x1 dist) (- x2 dist))
					  (if (< e1[bottom] e2[top])
						  (points6 (/ (+ e1[bottom] e2[top]) 2) (+ x1 dist) (- x2 dist))
						  (let ((ty (- (min e1[top]  e2[top])  dist))
								(by (+ (max e1[bottom] e2[bottom]) dist)))
							;; (y1 - ty) + (y2 - ty) < (by - y1) + (by - y2)
							;; -> y1 + y2 - 2ty < 2by - y1 - y2
							;; -> 2y1 + 2y2 < 2ty + 2by
							;; -> y1 + y2 < ty + by
							(points6 (if (< (+ y1 y2)
											(+ ty by)) ty by) (+ x1 dist) (- x2 dist))))))))))))

(defun resolve-connector-points-for-BL (pos1 pos2 e1 e2)
  (let ((dist *default-connector-spacing*))
	(with-point (x1 y1) (shape-connect-point e1 :bottom pos1)
	  (with-point (x2 y2) (shape-connect-point e2 :left pos2)
		(labels ((points3 ()
				   (list x1 y1 x1 y2 x2 y2))
				 (points5 (x3 y3)
				   (list x1 y1 x1 y3 x3 y3 x3 y2 x2 y2)))
		  (with-property
			  (if (and (< y1 y2) (< x1 x2))
				  (points3)
				  (points5 (if (< e1[right] x2)
							   (/ (+ e1[right] x2) 2)
							   (- (min e1[left] x2) dist))
						   (if (< y1 e2[top])
							   (/ (+ y1 e2[top]) 2)
							   (+ (max y1 e2[bottom]) dist))))))))))

(defun resolve-connector-points-for-BR (pos1 pos2 e1 e2)
  (let ((dist *default-connector-spacing*))
	(with-point (x1 y1) (shape-connect-point e1 :bottom pos1)
	  (with-point (x2 y2) (shape-connect-point e2 :right pos2)
		(labels ((points3 ()
				   (list x1 y1 x1 y2 x2 y2))
				 (points5 (x3 y3)
				   (list x1 y1 x1 y3 x3 y3 x3 y2 x2 y2)))
		  (with-property
			  (if (and (< y1 y2) (< x2 x1))
				  (points3)
				  (points5 (if (< x2 e1[left])
							   (/ (+ x2 e1[left]) 2)
							   (+ (max e1[right] x2) dist))
						   (if (< y1 e2[top])
							   (/ (+ y1 e2[top]) 2)
							   (+ (max y1 e2[bottom]) dist))))))))))

(defun resolve-connector-points-for-TL (pos1 pos2 e1 e2)
  (let ((dist *default-connector-spacing*))
	(with-point (x1 y1) (shape-connect-point e1 :top pos1)
	  (with-point (x2 y2) (shape-connect-point e2 :left pos2)
		(labels ((points3 ()
				   (list x1 y1 x1 y2 x2 y2))
				 (points5 (x3 y3)
				   (list x1 y1 x1 y3 x3 y3 x3 y2 x2 y2)))
		  (with-property
			  (if (and (< y2 y1) (< x1 x2))
				  (points3)
				  (points5 (if (< e1[right] x2)
							   (/ (+ e1[right] x2) 2)
							   (- (min e1[left] x2) dist))
						   (if (< e2[bottom] y1)
							   (/ (+ e2[bottom] y1) 2)
							   (- (min y1 e2[top]) dist))))))))))

(defun resolve-connector-points-for-TR (pos1 pos2 e1 e2)
  (let ((dist *default-connector-spacing*))
	(with-point (x1 y1) (shape-connect-point e1 :top pos1)
	  (with-point (x2 y2) (shape-connect-point e2 :right pos2)
		(labels ((points3 ()
				   (list x1 y1 x1 y2 x2 y2))
				 (points5 (x3 y3)
				   (list x1 y1 x1 y3 x3 y3 x3 y2 x2 y2)))
		  (with-property
			  (if (and (< y2 y1) (< x2 x1))
				  (points3)
				  (points5 (if (< x2 e1[left])
							   (/ (+ x2 e1[left]) 2)
							   (+ (max e1[right] x2) dist))
						   (if (< e2[bottom] y1)
							   (/ (+ y1 e2[bottom]) 2)
							   (- (min y1 e2[top]) dist))))))))))

(defun resolve-connector-points-for-LB (pos1 pos2 e1 e2)
  (let ((dist *default-connector-spacing*))
	(with-point (x1 y1) (shape-connect-point e1 :left pos1)
	  (with-point (x2 y2) (shape-connect-point e2 :bottom pos2)
		(labels ((points3 ()
				   (list x1 y1 x2 y1 x2 y2))
				 (points5 (x3 y3)
				   (list x1 y1 x3 y1 x3 y3 x2 y3 x2 y2)))
		  (with-property
			  (if (and (< x2 x1) (< y2 y1))
				  (points3)
				  (points5 (if (< e2[right] x1)
							   (/ (+ e2[right] x1) 2)
							   (- (min x1 e2[left]) dist))
						   (if (< y2 e1[top])
							   (/ (+ y2 e1[top]) 2)
							   (+ (max e1[bottom] y2) dist))))))))))


(defun resolve-connector-points-for-LT (pos1 pos2 e1 e2)
  (let ((dist *default-connector-spacing*))
	(with-point (x1 y1) (shape-connect-point e1 :left pos1)
	  (with-point (x2 y2) (shape-connect-point e2 :top pos2)
		(labels ((points3 ()
				   (list x1 y1 x2 y1 x2 y2))
				 (points5 (x3 y3)
				   (list x1 y1 x3 y1 x3 y3 x2 y3 x2 y2)))
		  (with-property
			  (if (and (< x2 x1) (< y1 y2))
				  (points3)
				  (points5 (if (< e2[right] x1)
							   (/ (+ e2[right] x1) 2)
							   (- (min x1 e2[left]) dist))
						   (if (< e1[bottom] y2)
							   (/ (+ e1[bottom] y2) 2)
							   (- (min e1[top] y2) dist))))))))))

(defun resolve-connector-points-for-RB (pos1 pos2 e1 e2)
  (let ((dist *default-connector-spacing*))
	(with-point (x1 y1) (shape-connect-point e1 :right pos1)
	  (with-point (x2 y2) (shape-connect-point e2 :bottom pos2)
		(labels ((points3 ()
				   (list x1 y1 x2 y1 x2 y2))
				 (points5 (x3 y3)
				   (list x1 y1 x3 y1 x3 y3 x2 y3 x2 y2)))
		  (with-property
			  (if (and (< x1 x2) (< y2 y1))
				  (points3)
				  (points5 (if (< x1 e2[left])
							   (/ (+ x1 e2[left]) 2)
							   (+ (max x1 e2[right]) dist))
						   (if (< y2 e1[top])
							   (/ (+ y2 e1[top]) 2)
							   (+ (max e1[bottom] y2) dist))))))))))

(defun resolve-connector-points-for-RT (pos1 pos2 e1 e2)
  (let ((dist *default-connector-spacing*))
	(with-point (x1 y1) (shape-connect-point e1 :right pos1)
	  (with-point (x2 y2) (shape-connect-point e2 :top pos2)
		(labels ((points3 ()
				   (list x1 y1 x2 y1 x2 y2))
				 (points5 (x3 y3)
				   (list x1 y1 x3 y1 x3 y3 x2 y3 x2 y2)))
		  (with-property
			  (if (and (< x1 x2) (< y1 y2))
				  (points3)
				  (points5 (if (< x1 e2[left])
							   (/ (+ x1 e2[left]) 2)
							   (+ (max x1 e2[right]) dist))
						   (if (< e1[bottom] y2)
							   (/ (+ e1[bottom] y2) 2)
							   (- (min e1[top] y2) dist))))))))))


#|
#|EXPORT|#				:resolve-connector-points
 |#
(defun resolve-connector-points (e1 e2 style)
  (labels ((get-dummy-rect-when-line (entity)
			 (if (not (typep entity 'line))
				 entity
				 (multiple-value-bind (x y dummy) (line-get-center entity)
				   (declare (ignore dummy))
				   (make-instance 'diagram:rectangle
								  :center-x x :center-y y
								  :width 0.01 :height 0.01 :rx 0 :ry 0)))))    ; 0.01 ... OK?
	(setf e1 (get-dummy-rect-when-line e1))
	(setf e2 (get-dummy-rect-when-line e2)))
  (when (keywordp style)
	(setf style (check-and-fix-connector-style style)))
  (type-assert style list)
  (let ((style (first  style))
		(pos1  (- (second style) 2))
		(pos2  (- (third  style) 2)))
	(ecase style
	  ((:CC) (resolve-connector-points-for-CC pos1 pos2 e1 e2))
	  ((:BB) (resolve-connector-points-for-BB pos1 pos2 e1 e2))
	  ((:TT) (resolve-connector-points-for-TT pos1 pos2 e1 e2))
	  ((:LL) (resolve-connector-points-for-LL pos1 pos2 e1 e2))
	  ((:RR) (resolve-connector-points-for-RR pos1 pos2 e1 e2))
	  ((:BT) (resolve-connector-points-for-BT pos1 pos2 e1 e2))
	  ((:TB) (resolve-connector-points-for-TB pos1 pos2 e1 e2))
	  ((:LR) (resolve-connector-points-for-LR pos1 pos2 e1 e2))
	  ((:RL) (resolve-connector-points-for-RL pos1 pos2 e1 e2))
	  ((:BL) (resolve-connector-points-for-BL pos1 pos2 e1 e2))
	  ((:BR) (resolve-connector-points-for-BR pos1 pos2 e1 e2))
	  ((:TL) (resolve-connector-points-for-TL pos1 pos2 e1 e2))
	  ((:TR) (resolve-connector-points-for-TR pos1 pos2 e1 e2))
	  ((:LB) (resolve-connector-points-for-LB pos1 pos2 e1 e2))
	  ((:LT) (resolve-connector-points-for-LT pos1 pos2 e1 e2))
	  ((:RB) (resolve-connector-points-for-RB pos1 pos2 e1 e2))
	  ((:RT) (resolve-connector-points-for-RT pos1 pos2 e1 e2))
)))




;-------------------------------------------------------------------------------
;
; class connector
;
;-------------------------------------------------------------------------------
(defclass connector (line)
  ((from  :initform nil :initarg :from)	   ; keyword
   (to    :initform nil :initarg :to)	   ; keyword
   (style :initform nil :initarg :style))) ; keyword - :(CC|[BLRT][123]?[BLRT][123]?) (keyword->list)


(defmethod initialize-instance :after ((ent connector) &rest initargs)
  (declare (ignore initargs))
  (with-slots (style) ent
	(setf style (or style *default-connector-style* :CC)))
  ent)

(defmethod check ((ent connector) canvas dict)
  (with-slots (from to style) ent
	(check-member from  :nullable nil :types keyword)
	(check-member to    :nullable nil :types keyword)
	(check-member style :nullable nil :types keyword)
	(setf style (check-and-fix-connector-style style))
	(let ((from-entity (dict-get-entity dict from))
		  (dest-entity (dict-get-entity dict   to)))
	  (unless from-entity
		(throw-exception "Entity '~A' not found in dictionary." from))
	  (unless dest-entity
		(throw-exception "Entity '~A' not found in dictionary."   to))
	  (unless (or (typep from-entity 'shape) (typep from-entity 'line))
		(throw-exception "Entity '~A' is not shape nor line object." from))
	  (unless (or (typep dest-entity 'shape) (typep dest-entity 'line))
		(throw-exception "Entity '~A' is not shape nor line object."   to))
;;	  (setf (line-points ent) '(0 0 0 0)) ; HACK.
;;	  (call-next-method)
	  (setf (line-points ent)
			(resolve-connector-points from-entity dest-entity style))
	  (call-next-method))))

(defmethod write-header ((ent connector) writer)
  (writer-write writer "<!-- "
					   (write-when (slot-value ent 'id) it " : ")
					   (string-downcase (symbol-name (type-of ent)))
					   " from " (slot-value ent 'from)
					   " to "   (slot-value ent 'to)
					   " -->"))


#|
#|EXPORT|#				:connector
 |#
(defmacro connector (from to &key style class stroke end1 end2 layer id)
  `(register-entity (make-instance 'diagram:connector
								   :from ,from :to ,to :class ,class
								   :style ,style :end1 ,end1 :end2 ,end2
								   :stroke ,stroke :layer ,layer :id ,id)))


