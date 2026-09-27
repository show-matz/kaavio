#|
#|ASD|#                (:file "block-arrow2"              :depends-on ("kaavio"
#|ASD|#                                                                "block-arrow"
#|ASD|#                                                                "point"
#|ASD|#                                                                "canvas"
#|ASD|#                                                                "mathutil"
#|ASD|#                                                                "path"))
#|EXPORT|#                ;block-arrow2.lisp
 |#

(in-package :kaavio)


(defun block-arrow-debug-extract-points (points)
  (labels ((recur (pts acc)
             (if (null pts)
                 (nreverse acc)
                 (let ((cur (car pts)))
                   (when (listp cur)
                     (ecase (first cur)
                       (:move-to (push (second  cur) acc))
                       (:line-to (push (second  cur) acc))
                       (:arc-to  (push (seventh cur) acc))))
                   (recur (cdr pts) acc)))))
    (recur points nil)))

(defun block-arrow-make-points-CC (ptA ptB width r1 margin endA endB length size)
  (if (and (= (point-x ptA) (point-x ptB)) (= (point-y ptA) (point-y ptB)))
      (values nil nil)
      (if (and endA (not endB))
          (block-arrow-make-points-CC ptB ptA width r1 margin endB endA length size)
          (let* ((lst nil)
                 (pt1 (xy+ ptA (* (kaavio::math/cos2 ptA ptB) margin)
                               (* (kaavio::math/sin2 ptA ptB) margin)))
                 (pt2 (xy+ ptB (* (kaavio::math/cos2 ptB ptA) margin)
                               (* (kaavio::math/sin2 ptB ptA) margin)))
                 (sin1 (kaavio::math/sin2 pt1 pt2))
                 (cos1 (kaavio::math/cos2 pt1 pt2)))
            (if (and (not endA) endB)
                (let ((pt3 (xy+ pt2 (* -1 length cos1) (* -1 length sin1))))
                  (push `(:move-to ,(xy+ pt1 (* -1 (/ width 2) sin1) (*    (/ width 2) cos1))) lst)
                  (push `(:line-to ,(xy+ pt1 (*    (/ width 2) sin1) (* -1 (/ width 2) cos1))) lst)
                  (push `(:line-to ,(xy+ pt3 (*    (/ width 2) sin1) (* -1 (/ width 2) cos1))) lst)
                  (push `(:line-to ,(xy+ pt3 (*    (/ size  2) sin1) (* -1 (/ size  2) cos1))) lst)
                  (push `(:line-to ,pt2)                                                       lst)
                  (push `(:line-to ,(xy+ pt3 (* -1 (/ size  2) sin1) (*    (/ size  2) cos1))) lst)
                  (push `(:line-to ,(xy+ pt3 (* -1 (/ width 2) sin1) (*    (/ width 2) cos1))) lst)
                  (push :close-path lst))
                (let ((pt3 (xy+ pt2 (* -1 length cos1) (* -1 length sin1)))
                      (pt4 (xy+ pt1 (*    length cos1) (*    length sin1))))
                  (push `(:move-to ,pt1)                                                       lst)
                  (push `(:line-to ,(xy+ pt4 (*    (/ size  2) sin1) (* -1 (/ size  2) cos1))) lst)
                  (push `(:line-to ,(xy+ pt4 (*    (/ width 2) sin1) (* -1 (/ width 2) cos1))) lst)
                  (push `(:line-to ,(xy+ pt3 (*    (/ width 2) sin1) (* -1 (/ width 2) cos1))) lst)
                  (push `(:line-to ,(xy+ pt3 (*    (/ size  2) sin1) (* -1 (/ size  2) cos1))) lst)
                  (push `(:line-to ,pt2)                                                       lst)
                  (push `(:line-to ,(xy+ pt3 (* -1 (/ size  2) sin1) (*    (/ size  2) cos1))) lst)
                  (push `(:line-to ,(xy+ pt3 (* -1 (/ width 2) sin1) (*    (/ width 2) cos1))) lst)
                  (push `(:line-to ,(xy+ pt4 (* -1 (/ width 2) sin1) (*    (/ width 2) cos1))) lst)
                  (push `(:line-to ,(xy+ pt4 (* -1 (/ size  2) sin1) (*    (/ size  2) cos1))) lst)
                  (push :close-path lst)))
            (values (nreverse lst) `(,ptA ,ptB))))))

(defun block-arrow-make-points-TL (ptA ptB width r1 margin endA endB length size)
  (if (or (not (< (point-x ptA) (point-x ptB)))
          (not (< (point-y ptB) (point-y ptA))))
      (values nil nil)
      (let* ((lst nil)
             (w/2 (/ width 2))
             (s/2 (/ size 2))
             (ptM (make-point (point-x ptA) (point-y ptB)))
             (r2  (if (< width r1) (- r1 width) 0)))
        ;; ptA  側の端点（必要なら矢印も描画）
        (if (not endA)
            (progn
              (push `(:move-to ,(xy+ ptA    w/2  (- margin))) lst)
              (push `(:line-to ,(xy+ ptA (- w/2) (- margin))) lst))
            (progn
              (push `(:move-to ,(xy+ ptA    w/2  (- 0 margin length))) lst)
              (push `(:line-to ,(xy+ ptA    s/2  (- 0 margin length))) lst)
              (push `(:line-to ,(xy+ ptA      0  (- 0 margin)       )) lst)
              (push `(:line-to ,(xy+ ptA (- s/2) (- 0 margin length))) lst)
              (push `(:line-to ,(xy+ ptA (- w/2) (- 0 margin length))) lst)))
        ;; 上に向かう外側の直線（と必要なら円弧）
        (push `(:line-to ,(xy+ ptM (- w/2) (- r1 w/2))) lst)
        (unless (zerop r1)
          (push `(:arc-to  ,r1 ,r1 0 0 1 ,(xy+ ptM (- r1 w/2) (- w/2))) lst))
        ;; 右に向かう外側の直線と ptB 側の端点（必要なら矢印も描画）
        (if (not endB)
            (progn
              (push `(:line-to ,(xy+ ptB (- margin) (- w/2))) lst)
              (push `(:line-to ,(xy+ ptB (- margin)    w/2 )) lst))
            (progn
              (push `(:line-to ,(xy+ ptB (- 0 margin length) (- w/2))) lst)
              (push `(:line-to ,(xy+ ptB (- 0 margin length) (- s/2))) lst)
              (push `(:line-to ,(xy+ ptB (- 0 margin)             0 )) lst)
              (push `(:line-to ,(xy+ ptB (- 0 margin length)    s/2 )) lst)
              (push `(:line-to ,(xy+ ptB (- 0 margin length)    w/2 )) lst)))
        ;; 左に向かう内側の直線（と必要なら円弧）
        (push `(:line-to ,(xy+ ptM (+ r2 w/2) w/2)) lst)
        (unless (zerop r2)
          (push `(:arc-to ,r2 ,r2 0 0 0 ,(xy+ ptM w/2 (+ r2 w/2))) lst))
        ;; 下に向かう内側の直線（これは :close-path で）
        (push :close-path lst)
        (values (nreverse lst) `(,ptA ,ptM ,ptB)))))

(defun block-arrow-make-points-RB (ptA ptB width r1 margin endA endB length size)
  (if (or (not (< (point-x ptA) (point-x ptB)))
          (not (< (point-y ptB) (point-y ptA))))
      (values nil nil)
      (let* ((lst nil)
             (w/2 (/ width 2))
             (s/2 (/ size  2))
             (ptM (make-point (point-x ptB) (point-y ptA)))
             (r2  (if (< width r1) (- r1 width) 0)))
        ;; ptA 側の端点（必要なら矢印も描画）
        (if (not endA)
            (progn
              (push `(:move-to ,(xy+ ptA margin (- w/2))) lst)
              (push `(:line-to ,(xy+ ptA margin    w/2 )) lst))
            (progn
              (push `(:move-to ,(xy+ ptA (+ margin length) (- w/2))) lst)
              (push `(:line-to ,(xy+ ptA (+ margin length) (- s/2))) lst)
              (push `(:line-to ,(xy+ ptA (+ margin)             0 )) lst)
              (push `(:line-to ,(xy+ ptA (+ margin length)    s/2 )) lst)
              (push `(:line-to ,(xy+ ptA (+ margin length)    w/2 )) lst)))
        ;; 右に向かう外側の直線（と必要なら円弧）
        (push `(:line-to ,(xy+ ptM (- w/2 r1) w/2)) lst)
        (unless (zerop r1)
          (push `(:arc-to ,r1 ,r1 0 0 0 ,(xy+ ptM w/2 (- w/2 r1))) lst))
        ;; 右に向かう外側の直線と ptB 側の端点（必要なら矢印も描画）
        (if (not endB)
            (progn
              (push `(:line-to ,(xy+ ptB    w/2     margin))         lst)
              (push `(:line-to ,(xy+ ptB (- w/2)    margin))         lst))
            (progn
              (push `(:line-to ,(xy+ ptB    w/2  (+ margin length))) lst)
              (push `(:line-to ,(xy+ ptB    s/2  (+ margin length))) lst)
              (push `(:line-to ,(xy+ ptB      0     margin        )) lst)
              (push `(:line-to ,(xy+ ptB (- s/2) (+ margin length))) lst)
              (push `(:line-to ,(xy+ ptB (- w/2) (+ margin length))) lst)))
        ;; 左に向かう内側の直線（と必要なら円弧）
        (push `(:line-to ,(xy+ ptM (- w/2) (- 0 w/2 r2))) lst)
        (unless (zerop r2)
          (push `(:arc-to ,r2 ,r2 0 0 1 ,(xy+ ptM (- 0 w/2 r2) (- w/2))) lst))
        ;; 下に向かう内側の直線（これは :close-path で）
        (push :close-path lst)
        (values (nreverse lst) `(,ptA ,ptM ,ptB)))))

(defun block-arrow-make-points-TR (ptA ptB width r1 margin endA endB length size)
  (if (or (not (< (point-x ptB) (point-x ptA)))
          (not (< (point-y ptB) (point-y ptA))))
      (values nil nil)
      (let* ((lst nil)
             (w/2 (/ width 2))
             (s/2 (/ size  2))
             (ptM (make-point (point-x ptA) (point-y ptB)))
             (r2  (if (< width r1) (- r1 width) 0)))
        ;; ptA 側の端点（必要なら矢印も描画）
        (if (not endA)
            (progn
              (push `(:move-to ,(xy+ ptA (- w/2) (- margin))) lst)
              (push `(:line-to ,(xy+ ptA    w/2  (- margin))) lst))
            (progn
              (push `(:move-to ,(xy+ ptA (- w/2) (- 0 margin length))) lst)
              (push `(:line-to ,(xy+ ptA (- s/2) (- 0 margin length))) lst)
              (push `(:line-to ,(xy+ ptA      0  (- 0 margin)       )) lst)
              (push `(:line-to ,(xy+ ptA    s/2  (- 0 margin length))) lst)
              (push `(:line-to ,(xy+ ptA    w/2  (- 0 margin length))) lst)))
        ;; 上に向かう外側の直線（と必要なら円弧）
        (push `(:line-to ,(xy+ ptM w/2 (- r1 w/2))) lst)
        (unless (zerop r1)
          (push `(:arc-to ,r1 ,r1 0 0 0 ,(xy+ ptM (- w/2 r1) (- w/2))) lst))
        ;; 左に向かう外側の直線と ptB 側の端点（必要なら矢印も描画）
        (if (not endB)
            (progn
              (push `(:line-to ,(xy+ ptB    margin         (- w/2))) lst)
              (push `(:line-to ,(xy+ ptB    margin            w/2 )) lst))
            (progn
              (push `(:line-to ,(xy+ ptB (+ margin length) (- w/2))) lst)
              (push `(:line-to ,(xy+ ptB (+ margin length) (- s/2))) lst)
              (push `(:line-to ,(xy+ ptB (+ margin)             0 )) lst)
              (push `(:line-to ,(xy+ ptB (+ margin length)    s/2 )) lst)
              (push `(:line-to ,(xy+ ptB (+ margin length)    w/2 )) lst)))
        ;; 右に向かう内側の直線（と必要なら円弧）
        (push `(:line-to ,(xy+ ptM (- 0 r2 w/2) w/2)) lst)
        (unless (zerop r2)
          (push `(:arc-to ,r2 ,r2 0 0 1 ,(xy+ ptM (- w/2) (+ r2 w/2))) lst))
        ;; 下に向かう内側の直線（これは :close-path で）
        (push :close-path lst)
        (values (nreverse lst) `(,ptA ,ptM ,ptB)))))

(defun block-arrow-make-points-LB (ptA ptB width r1 margin endA endB length size)
  (if (or (not (< (point-x ptB) (point-x ptA)))
          (not (< (point-y ptB) (point-y ptA))))
      (values nil nil)
      (let* ((lst nil)
             (w/2 (/ width 2))
             (s/2 (/ size  2))
             (ptM (make-point (point-x ptB) (point-y ptA)))
             (r2  (if (< width r1) (- r1 width) 0)))
        ;; ptA 側の端点（必要なら矢印も描画）
        (if (not endA)
            (progn
              (push `(:move-to ,(xy+ ptA (- margin)         (- w/2))) lst)
              (push `(:line-to ,(xy+ ptA (- margin)            w/2 )) lst))
            (progn
              (push `(:move-to ,(xy+ ptA (- 0 margin length) (- w/2))) lst)
              (push `(:line-to ,(xy+ ptA (- 0 margin length) (- s/2))) lst)
              (push `(:line-to ,(xy+ ptA (- 0 margin)             0 )) lst)
              (push `(:line-to ,(xy+ ptA (- 0 margin length)    s/2 )) lst)
              (push `(:line-to ,(xy+ ptA (- 0 margin length)    w/2 )) lst)))
        ;; 左に向かう外側の直線（と必要なら円弧）
        (push `(:line-to ,(xy+ ptM (- r1 w/2) w/2)) lst)
        (unless (zerop r1)
          (push `(:arc-to ,r1 ,r1 0 0 1 ,(xy+ ptM (- w/2) (- w/2 r1))) lst))
        ;; 上に向かう外側の直線と ptB 側の端点（必要なら矢印も描画）
        (if (not endB)
            (progn
              (push `(:line-to ,(xy+ ptB (- w/2)    margin))         lst)
              (push `(:line-to ,(xy+ ptB    w/2     margin))         lst))
            (progn
              (push `(:line-to ,(xy+ ptB (- w/2) (+ margin length))) lst)
              (push `(:line-to ,(xy+ ptB (- s/2) (+ margin length))) lst)
              (push `(:line-to ,(xy+ ptB      0     margin        )) lst)
              (push `(:line-to ,(xy+ ptB    s/2  (+ margin length))) lst)
              (push `(:line-to ,(xy+ ptB    w/2  (+ margin length))) lst)))
        ;; 下に向かう内側の直線（と必要なら円弧）
        (push `(:line-to ,(xy+ ptM w/2 (- 0 w/2 r2))) lst)
        (unless (zerop r2)
          (push `(:arc-to ,r2 ,r2 0 0 0 ,(xy+ ptM (+ w/2 r2) (- w/2))) lst))
        ;; 右に向かう内側の直線（これは :close-path で）
        (push :close-path lst)
        (values (nreverse lst) `(,ptA ,ptM ,ptB)))))

(defun block-arrow-make-points-TT (ptA ptB width r1 margin endA endB length size spacing)
  (if (= (point-x ptA) (point-x ptB))
      (values nil nil)
      (if (not (< (point-x ptA) (point-x ptB)))
          (block-arrow-make-points-TT ptB ptA width r1 margin endB endA length size spacing)
          (let* ((lst  nil)
                 (w/2  (/ width 2))
                 (s/2  (/ size  2))
                 (r2   (if (< width r1) (- r1 width) 0))
                 (yM   (- (min (point-y ptA) (point-y ptB))
                          (-   (point-x ptB) (point-x ptA)) (- spacing)))
                 (ptM1 (make-point (point-x ptA) yM))
                 (ptM2 (make-point (point-x ptB) yM)))
            ;; ptA 側の端点（必要なら矢印も描画）
            (if (not endA)
                (progn
                  (push `(:move-to ,(xy+ ptA    w/2   (- margin))) lst)
                  (push `(:line-to ,(xy+ ptA (- w/2)  (- margin))) lst))
                (progn
                  (push `(:move-to ,(xy+ ptA    w/2  (- 0 margin length))) lst)
                  (push `(:line-to ,(xy+ ptA    s/2  (- 0 margin length))) lst)
                  (push `(:line-to ,(xy+ ptA      0  (- 0 margin)       )) lst)
                  (push `(:line-to ,(xy+ ptA (- s/2) (- 0 margin length))) lst)
                  (push `(:line-to ,(xy+ ptA (- w/2) (- 0 margin length))) lst)))
            ;; 上に向かう外側の直線（と必要なら円弧）
            (push `(:line-to ,(xy+ ptM1 (- w/2) (- r1 w/2))) lst)
            (unless (zerop r1)
              (push `(:arc-to ,r1 ,r1 0 0 1 ,(xy+ ptM1 (- r1 w/2) (- w/2))) lst))
            ;; 右に向かう外側の直線（と必要なら円弧）
            (push `(:line-to ,(xy+ ptM2 (- w/2 r1) (- w/2))) lst)
            (unless (zerop r1)
              (push `(:arc-to ,r1 ,r1 0 0 1 ,(xy+ ptM2 w/2 (- r1 w/2))) lst))
            ;; 下に向かう外側の直線と ptB 側の端点（必要なら矢印も描画）
            (if (not endB)
                (progn
                  (push `(:line-to ,(xy+ ptB    w/2  (- margin))) lst)
                  (push `(:line-to ,(xy+ ptB (- w/2) (- margin))) lst))
                (progn
                  (push `(:line-to ,(xy+ ptB    w/2  (- 0 margin length))) lst)
                  (push `(:line-to ,(xy+ ptB    s/2  (- 0 margin length))) lst)
                  (push `(:line-to ,(xy+ ptB      0  (- 0 margin)       )) lst)
                  (push `(:line-to ,(xy+ ptB (- s/2) (- 0 margin length))) lst)
                  (push `(:line-to ,(xy+ ptB (- w/2) (- 0 margin length))) lst)))
            ;; 上に向かう内側の直線（と必要なら円弧）
            (push `(:line-to ,(xy+ ptM2 (- w/2) (+ w/2 r2))) lst)
            (unless (zerop r2)
              (push `(:arc-to ,r2 ,r2 0 0 0 ,(xy+ ptM2 (- 0 w/2 r2) w/2)) lst))
            ;; 左に向かう内側の直線（と必要なら円弧）
            (push `(:line-to ,(xy+ ptM1 (+ r2 w/2) w/2)) lst)
            (unless (zerop r2)
              (push `(:arc-to ,r2 ,r2 0 0 0 ,(xy+ ptM1 w/2 (+ r2 w/2))) lst))
            ;; 下に向かう内側の直線（これは :close-path で）
            (push :close-path lst)
            (values (nreverse lst) `(,ptA ,ptM1 ,ptM2 ,ptB))))))

(defun block-arrow-make-points-BB (ptA ptB width r1 margin endA endB length size spacing)
  (if (= (point-x ptA) (point-x ptB))
      (values nil nil)
      (if (not (< (point-x ptA) (point-x ptB)))
          (block-arrow-make-points-BB ptB ptA width r1 margin endB endA length size spacing)
          (let* ((lst  nil)
                 (w/2  (/ width 2))
                 (s/2  (/ size  2))
                 (r2   (if (< width r1) (- r1 width) 0))
                 (yM   (+ (max (point-y ptA) (point-y ptB))
                          (-   (point-x ptB) (point-x ptA)) spacing))
                 (ptM1 (make-point (point-x ptA) yM))
                 (ptM2 (make-point (point-x ptB) yM)))
            ;; ptA 側の端点（必要なら矢印も描画）
            (if (not endA)
                (progn
                  (push `(:move-to ,(xy+ ptA    w/2     margin))         lst)
                  (push `(:line-to ,(xy+ ptA (- w/2)    margin))         lst))
                (progn
                  (push `(:move-to ,(xy+ ptA    w/2  (+ margin length))) lst)
                  (push `(:line-to ,(xy+ ptA    s/2  (+ margin length))) lst)
                  (push `(:line-to ,(xy+ ptA      0  (+ margin)       )) lst)
                  (push `(:line-to ,(xy+ ptA (- s/2) (+ margin length))) lst)
                  (push `(:line-to ,(xy+ ptA (- w/2) (+ margin length))) lst)))
            ;; 下に向かう外側の直線（と必要なら円弧）
            (push `(:line-to ,(xy+ ptM1 (- w/2) (- w/2 r1))) lst)
            (unless (zerop r1)
              (push `(:arc-to ,r1 ,r1 0 0 0 ,(xy+ ptM1 (- r1 w/2) w/2)) lst))
            ;; 右に向かう外側の直線（と必要なら円弧）
            (push `(:line-to ,(xy+ ptM2 (- w/2 r1) w/2)) lst)
            (unless (zerop r1)
              (push `(:arc-to ,r1 ,r1 0 0 0 ,(xy+ ptM2 w/2 (- w/2 r1))) lst))
            ;; 上に向かう外側の直線と ptB 側の端点（必要なら矢印も描画）
            (if (not endB)
                (progn
                  (push `(:line-to ,(xy+ ptB    w/2     margin))         lst)
                  (push `(:line-to ,(xy+ ptB (- w/2)    margin))         lst))
                (progn
                  (push `(:line-to ,(xy+ ptB    w/2  (+ margin length))) lst)
                  (push `(:line-to ,(xy+ ptB    s/2  (+ margin length))) lst)
                  (push `(:line-to ,(xy+ ptB      0  (+ margin)       )) lst)
                  (push `(:line-to ,(xy+ ptB (- s/2) (+ margin length))) lst)
                  (push `(:line-to ,(xy+ ptB (- w/2) (+ margin length))) lst)))
            ;; 下に向かう内側の直線（と必要なら円弧）
            (push `(:line-to ,(xy+ ptM2 (- w/2) (- 0 w/2 r2))) lst)
            (unless (zerop r2)
              (push `(:arc-to ,r2 ,r2 0 0 1 ,(xy+ ptM2 (- 0 w/2 r2) (- w/2))) lst))
            ;; 左に向かう内側の直線（と必要なら円弧）
            (push `(:line-to ,(xy+ ptM1 (+ r2 w/2) (- w/2))) lst)
            (unless (zerop r2)
              (push `(:arc-to ,r2 ,r2 0 0 1 ,(xy+ ptM1 w/2 (- 0 r2 w/2))) lst))
            ;; 上に向かう内側の直線（これは :close-path で）
            (push :close-path lst)
            (values (nreverse lst) `(,ptA ,ptM1 ,ptM2 ,ptB))))))

(defun block-arrow-make-points-LL (ptA ptB width r1 margin endA endB length size spacing)
  (if (= (point-y ptA) (point-y ptB))
      (values nil nil)
      (if (not (< (point-y ptA) (point-y ptB)))
          (block-arrow-make-points-LL ptB ptA width r1 margin endB endA length size spacing)
          (let* ((lst  nil)
                 (w/2  (/ width 2))
                 (s/2  (/ size  2))
                 (r2   (if (< width r1) (- r1 width) 0))
                 (xM   (- (min (point-x ptA) (point-x ptB))
                          (-   (point-y ptB) (point-y ptA)) (- spacing)))
                 (ptM1 (make-point xM (point-y ptA)))
                 (ptM2 (make-point xM (point-y ptB))))
            ;; ptA 側の端点（必要なら矢印も描画）
            (If (not endA)
                (progn
                  (push `(:move-to ,(xy+ ptA (-   margin)           w/2 )) lst)
                  (push `(:line-to ,(xy+ ptA (-   margin)        (- w/2))) lst))
                (progn
                  (push `(:move-to ,(xy+ ptA (- 0 margin length)    w/2 )) lst)
                  (push `(:line-to ,(xy+ ptA (- 0 margin length)    s/2 )) lst)
                  (push `(:line-to ,(xy+ ptA (- 0 margin)             0 )) lst)
                  (push `(:line-to ,(xy+ ptA (- 0 margin length) (- s/2))) lst)
                  (push `(:line-to ,(xy+ ptA (- 0 margin length) (- w/2))) lst)))
            ;; 左に向かう外側の直線（と必要なら円弧）
            (push `(:line-to ,(xy+ ptM1 (- r1 w/2) (- w/2))) lst)
            (unless (zerop r1)
              (push `(:arc-to ,r1 ,r1 0 0 0 ,(xy+ ptM1 (- w/2) (- r1 w/2))) lst))
            ;; 下に向かう外側の直線（と必要なら円弧）
            (push `(:line-to ,(xy+ ptM2 (- w/2) (- w/2 r1))) lst)
            (unless (zerop r1)
              (push `(:arc-to ,r1 ,r1 0 0 0 ,(xy+ ptM2 (- r1 w/2) w/2)) lst))
            ;; 右に向かう外側の直線と ptB 側の端点（必要なら矢印も描画）
            (if (not endB)
                (progn
                  (push `(:line-to ,(xy+ ptB (- margin)    w/2 )) lst)
                  (push `(:line-to ,(xy+ ptB (- margin) (- w/2))) lst))
                (progn
                  (push `(:line-to ,(xy+ ptB (- 0 margin length)    w/2 )) lst)
                  (push `(:line-to ,(xy+ ptB (- 0 margin length)    s/2 )) lst)
                  (push `(:line-to ,(xy+ ptB (- 0 margin)             0 )) lst)
                  (push `(:line-to ,(xy+ ptB (- 0 margin length) (- s/2))) lst)
                  (Push `(:line-to ,(xy+ ptB (- 0 margin length) (- w/2))) lst)))
            ;; 左に向かう内側の直線（と必要なら円弧）
            (push `(:line-to ,(xy+ ptM2 (+ w/2 r2) (- w/2))) lst)
            (unless (zerop r2)
              (push `(:arc-to ,r2 ,r2 0 0 1 ,(xy+ ptM2 w/2 (- 0 w/2 r2))) lst))
            ;; 上に向かう内側の直線（と必要なら円弧）
            (push `(:line-to ,(xy+ ptM1 w/2 (+ r2 w/2))) lst)
            (unless (zerop r2)
              (push `(:arc-to ,r2 ,r2 0 0 1 ,(xy+ ptM1 (+ r2 w/2) w/2)) lst))
            ;; 右に向かう内側の直線（これは :close-path で）
            (push :close-path lst)
            (values (nreverse lst) `(,ptA ,ptM1 ,ptM2 ,ptB))))))

(defun block-arrow-make-points-RR (ptA ptB width r1 margin endA endB length size spacing)
  (if (= (point-y ptA) (point-y ptB))
      (values nil nil)
      (if (not (< (point-y ptA) (point-y ptB)))
          (block-arrow-make-points-RR ptB ptA width r1 margin endB endA length size spacing)
          (let* ((lst  nil)
                 (w/2  (/ width 2))
                 (s/2  (/ size  2))
                 (r2   (if (< width r1) (- r1 width) 0))
                 (xM   (+ (max (point-x ptA) (point-x ptB))
                          (-   (point-y ptB) (point-y ptA)) spacing))
                 (ptM1 (make-point xM (point-y ptA)))
                 (ptM2 (make-point xM (point-y ptB))))
            ;; ptA 側の端点（必要なら矢印も描画）
            (if (not endA)
                (progn
                  (push `(:move-to ,(xy+ ptA    margin            w/2 )) lst)
                  (push `(:line-to ,(xy+ ptA    margin         (- w/2))) lst))
                (progn
                  (push `(:move-to ,(xy+ ptA (+ margin length)    w/2 )) lst)
                  (push `(:line-to ,(xy+ ptA (+ margin length)    s/2 )) lst)
                  (push `(:line-to ,(xy+ ptA (+ margin)             0 )) lst)
                  (push `(:line-to ,(xy+ ptA (+ margin length) (- s/2))) lst)
                  (push `(:line-to ,(xy+ ptA (+ margin length) (- w/2))) lst)))
            ;; 右に向かう外側の直線（と必要なら円弧）
            (push `(:line-to ,(xy+ ptM1 (- w/2 r1) (- w/2))) lst)
            (unless (zerop r1)
              (push `(:arc-to ,r1 ,r1 0 0 1 ,(xy+ ptM1 w/2 (- r1 w/2))) lst))
            ;; 下に向かう外側の直線（と必要なら円弧）
            (push `(:line-to ,(xy+ ptM2 w/2 (- w/2 r1))) lst)
            (unless (zerop r1)
              (push `(:arc-to ,r1 ,r1 0 0 1 ,(xy+ ptM2 (- w/2 r1) w/2)) lst))
            ;; 左に向かう外側の直線と ptB 側の端点（必要なら矢印も描画）
            (if (not endB)
                (progn
                  (push `(:line-to ,(xy+ ptB margin    w/2 )) lst)
                  (push `(:line-to ,(xy+ ptB margin (- w/2))) lst))
                (progn
                  (push `(:line-to ,(xy+ ptB (+ margin length)    w/2 )) lst)
                  (push `(:line-to ,(xy+ ptB (+ margin length)    s/2 )) lst)
                  (push `(:line-to ,(xy+ ptB (+ margin)             0 )) lst)
                  (push `(:line-to ,(xy+ ptB (+ margin length) (- s/2))) lst)
                  (Push `(:line-to ,(xy+ ptB (+ margin length) (- w/2))) lst)))
            ;; 右に向かう内側の直線（と必要なら円弧）
            (push `(:line-to ,(xy+ ptM2 (- 0 w/2 r2) (- w/2))) lst)
            (unless (zerop r2)
              (push `(:arc-to ,r2 ,r2 0 0 0 ,(xy+ ptM2 (- w/2) (- 0 w/2 r2))) lst))
            ;; 上に向かう内側の直線（と必要なら円弧）
            (push `(:line-to ,(xy+ ptM1 (- w/2) (+ r2 w/2))) lst)
            (unless (zerop r2)
              (push `(:arc-to ,r2 ,r2 0 0 0 ,(xy+ ptM1 (- 0 w/2 r2) w/2)) lst))
            ;; 左に向かう内側の直線（これは :close-path で）
            (push :close-path lst)
            (values (nreverse lst) `(,ptA ,ptM1 ,ptM2 ,ptB))))))

(defun block-arrow-make-points-RL (ptA ptB width r1 margin endA endB length size spacing)
  (if (not (< (point-x ptA) (point-x ptB)))
      (values nil nil)
      (if (= (point-y pta) (point-y ptB))
          ;; ptA.y == ptB.y ならば block-arrow-make-points-CC に転送
          (block-arrow-make-points-CC ptA ptB width r1 margin endA endB length size)
          ;; 上記以外の場合はクランク描画
          (let* ((lst  nil)
                 (w/2  (/ width 2))
                 (s/2  (/ size  2))
                 (r2   (if (< width r1) (- r1 width) 0))
                 (xM   (+ (/ (+ (point-x ptA) (point-x ptB)) 2) spacing))
                 (ptM1 (make-point xM (point-y ptA)))
                 (ptM2 (make-point xM (point-y ptB)))
                 (up-p (< (point-y ptB) (point-y ptA))))
            ;; ptA 側の端点（必要なら矢印も描画）
            (if (not endA)
                (progn
                  (push `(:move-to ,(xy+ ptA margin (- w/2))) lst)
                  (push `(:line-to ,(xy+ ptA margin    w/2 )) lst))
                (progn
                  (push `(:move-to ,(xy+ ptA (+ margin length) (- w/2))) lst)
                  (push `(:line-to ,(xy+ ptA (+ margin length) (- s/2))) lst)
                  (push `(:line-to ,(xy+ ptA (+ margin)             0 )) lst)
                  (push `(:line-to ,(xy+ ptA (+ margin length)    s/2 )) lst)
                  (push `(:line-to ,(xy+ ptA (+ margin length)    w/2 )) lst)))
            ;; L->R で見て階段を上がるクランクか下がるクランクかで場合分け
            (if up-p
                (progn
                  ;; 右に向かう直線と必要なら円弧（外側）
                  (push `(:line-to ,(xy+ ptM1 (- w/2 r1) w/2)) lst)
                  (unless (zerop r1)
                    (push `(:arc-to ,r1 ,r1 0 0 0 ,(xy+ ptM1 w/2 (- w/2 r1))) lst))
                  ;; 上に向かう直線と必要なら円弧（内側）
                  (push `(:line-to ,(xy+ ptM2 w/2 (+ w/2 r2))) lst)
                  (unless (zerop r2)
                    (push `(:arc-to ,r2 ,r2 0 0 1 ,(xy+ ptM2 (+ w/2 r2) w/2)) lst)))
                (progn
                  ;; 右に向かう直線と必要なら円弧（内側）
                  (push `(:line-to ,(xy+ ptM1 (- 0 w/2 r2) w/2)) lst)
                  (unless (zerop r2)
                    (push `(:arc-to ,r2 ,r2 0 0 1 ,(xy+ ptM1 (- w/2) (+ w/2 r2))) lst))
                  ;; 下に向かう直線と必要なら円弧（外側）
                  (push `(:line-to ,(xy+ ptM2 (- w/2) (- w/2 r1))) lst)
                  (unless (zerop r1)
                    (push `(:arc-to ,r1 ,r1 0 0 0 ,(xy+ ptM2 (- r1 w/2) w/2)) lst))))
            ;; ptB 側の端点（必要なら矢印も描画）
            (if (not endB)
                (progn
                  (push `(:line-to ,(xy+ ptB (- margin)    w/2 )) lst)
                  (push `(:line-to ,(xy+ ptB (- margin) (- w/2))) lst))
                (progn
                  (push `(:line-to ,(xy+ ptB (- 0 margin length)    w/2 )) lst)
                  (push `(:line-to ,(xy+ ptB (- 0 margin length)    s/2 )) lst)
                  (push `(:line-to ,(xy+ ptB (- 0 margin)             0 )) lst)
                  (push `(:line-to ,(xy+ ptB (- 0 margin length) (- s/2))) lst)
                  (Push `(:line-to ,(xy+ ptB (- 0 margin length) (- w/2))) lst)))
            ;; L->R で見て階段を上がるクランクか下がるクランクかで場合分け
            (if up-p
                (progn
                  ;; 左に向かう直線と必要なら円弧（外側）
                  (push `(:line-to ,(xy+ ptM2 (- r1 w/2) (- w/2))) lst)
                  (unless (zerop r1)
                    (push `(:arc-to ,r1 ,r1 0 0 0 ,(xy+ ptM2 (- w/2) (- r1 w/2))) lst))
                  ;; 下に向かう直線と必要なら円弧（内側）
                  (push `(:line-to ,(xy+ ptM1 (- w/2) (- 0 w/2 r2))) lst)
                  (unless (zerop r2)
                    (push `(:arc-to ,r2 ,r2 0 0 1 ,(xy+ ptM1 (- 0 w/2 r2) (- w/2))) lst)))
                (progn
                  ;; 左に向かう直線と必要なら円弧（内側）
                  (push `(:line-to ,(xy+ ptM2 (+ r2 w/2) (- w/2))) lst)
                  (unless (zerop r2)
                    (push `(:arc-to ,r2 ,r2 0 0 1 ,(xy+ ptM2 w/2 (- 0 w/2 r2))) lst))
                  ;; 上に向かう直線と必要なら円弧（外側）
                  (push `(:line-to ,(xy+ ptM1 w/2 (- r1 w/2))) lst)
                  (unless (zerop r1)
                    (push `(:arc-to ,r1 ,r1 0 0 0 ,(xy+ ptM1 (- w/2 r1) (- w/2))) lst))))
            (push :close-path lst)
            (values (nreverse lst) `(,ptA ,ptM1 ,ptM2 ,ptB))))))

(defun block-arrow-make-points-BT (ptA ptB width r1 margin endA endB length size spacing)
  (if (not (< (point-y ptA) (point-y ptB)))
      (values nil nil)
      (if (= (point-x ptA) (point-x ptB))
          ;; ptA.x == ptB.x ならば block-arrow-make-points-CC に転送
          (block-arrow-make-points-CC ptA ptB width r1 margin endA endB length size)
          ;; 上記以外の場合はクランク描画
          (let* ((lst     nil)
                 (w/2     (/ width 2))
                 (s/2     (/ size  2))
                 (r2      (if (< width r1) (- r1 width) 0))
                 (yM      (+ (/ (+ (point-y ptA) (point-y ptB)) 2) spacing))
                 (ptM1    (make-point (point-x ptA) yM))
                 (ptM2    (make-point (point-x ptB) yM))
                 (right-p (< (point-x ptA) (point-x ptB))))
            ;; ptA 側の端点（必要なら矢印も描画）
            (if (not endA)
                (progn
                  (push `(:move-to ,(xy+ ptA    w/2  margin)) lst)
                  (push `(:line-to ,(xy+ ptA (- w/2) margin)) lst))
                (progn
                  (push `(:move-to ,(xy+ ptA    w/2  (+ margin length))) lst)
                  (push `(:line-to ,(xy+ ptA    s/2  (+ margin length))) lst)
                  (push `(:line-to ,(xy+ ptA      0  (+ margin)       )) lst)
                  (push `(:line-to ,(xy+ ptA (- s/2) (+ margin length))) lst)
                  (push `(:line-to ,(xy+ ptA (- w/2) (+ margin length))) lst)))
            ;; T->B で見て右に曲がるクランクか左に曲がるクランクかで場合分け
            (if right-p
                (progn
                  ;; 下に向かう直線と必要なら円弧（外側）
                  (push `(:line-to ,(xy+ ptM1 (- w/2) (- w/2 r1))) lst)
                  (unless (zerop r1)
                    (push `(:arc-to ,r1 ,r1 0 0 0 ,(xy+ ptM1 (- r1 w/2) w/2)) lst))
                  ;; 右に向かう直線と必要なら円弧（内側）
                  (push `(:line-to ,(xy+ ptM2 (- 0 w/2 r2) w/2)) lst)
                  (unless (zerop r2)
                    (push `(:arc-to ,r2 ,r2 0 0 1 ,(xy+ ptM2 (- w/2) (+ w/2 r2))) lst)))
                (progn
                  ;; 下に向かう直線と必要なら円弧（内側）
                  (push `(:line-to ,(xy+ ptM1 (- w/2) (- 0 r2 w/2))) lst)
                  (unless (zerop r2)
                    (push `(:arc-to ,r2 ,r2 0 0 1 ,(xy+ ptM1 (- 0 w/2 r2) (- w/2))) lst))
                  ;; 左に向かう直線と必要なら円弧（外側）
                  (push `(:line-to ,(xy+ ptM2 (- r1 w/2) (- w/2))) lst)
                  (unless (zerop r1)
                    (push `(:arc-to ,r1 ,r1 0 0 0 ,(xy+ ptM2 (- w/2) (- r1 w/2))) lst))))
            ;; ptB 側の端点（必要なら矢印も描画）
            (if (not endB)
                (progn
                  (push `(:line-to ,(xy+ ptB (- w/2) (- margin))) lst)
                  (push `(:line-to ,(xy+ ptB    w/2  (- margin))) lst))
                (progn
                  (push `(:line-to ,(xy+ ptB (- w/2) (- 0 margin length))) lst)
                  (push `(:line-to ,(xy+ ptB (- s/2) (- 0 margin length))) lst)
                  (push `(:line-to ,(xy+ ptB      0  (- 0 margin)       )) lst)
                  (push `(:line-to ,(xy+ ptB    s/2  (- 0 margin length))) lst)
                  (push `(:line-to ,(xy+ ptB    w/2  (- 0 margin length))) lst)))
            ;; T->B で見て右に曲がるクランクか左に曲がるクランクかで場合分け
            (if right-p
                (progn
                  ;; 上に向かう直線と必要なら円弧（外側）
                  (push `(:line-to ,(xy+ ptM2 w/2 (- r1 w/2))) lst)
                  (unless (zerop r1)
                    (push `(:arc-to ,r1 ,r1 0 0 0 ,(xy+ ptM2 (- w/2 r1) (- w/2))) lst))
                  ;; 右に向かう直線と必要なら円弧（内側）
                  (push `(:line-to ,(xy+ ptM1 (+ w/2 r2) (- w/2))) lst)
                  (unless (zerop r2)
                    (push `(:arc-to ,r2 ,r2 0 0 1 ,(xy+ ptM1 w/2 (- 0 w/2 r2))) lst)))
                (progn
                  ;; 上に向かう直線と必要なら円弧（内側）
                  (push `(:line-to ,(xy+ ptM2 w/2 (+ w/2 r2))) lst)
                  (unless (zerop r2)
                    (push `(:arc-to ,r2 ,r2 0 0 1 ,(xy+ ptM2 (+ w/2 r2) w/2)) lst))
                  ;; 左に向かう直線と必要なら円弧（外側）
                  (push `(:line-to ,(xy+ ptM1 (- w/2 r1) w/2)) lst)
                  (unless (zerop r1)
                    (push `(:arc-to ,r1 ,r1 0 0 0 ,(xy+ ptM1 w/2 (- w/2 r1))) lst))))
            (push :close-path lst)
            (values (nreverse lst) `(,ptA ,ptM1 ,ptM2 ,ptB))))))

(defun block-arrow-make-points (ptA ptB style width both radius margin length size spacing)
  (multiple-value-bind (pts1 pts2)
      (ecase style
        (:CC (block-arrow-make-points-CC ptA ptB width radius margin both t length size))
        ;; L type
        (:TL (block-arrow-make-points-TL ptA ptB width radius margin both t length size))
        (:LT (block-arrow-make-points-TL ptB ptA width radius margin t both length size))
        (:RB (block-arrow-make-points-RB ptA ptB width radius margin both t length size))
        (:BR (block-arrow-make-points-RB ptB ptA width radius margin t both length size))
        (:TR (block-arrow-make-points-TR ptA ptB width radius margin both t length size))
        (:RT (block-arrow-make-points-TR ptB ptA width radius margin t both length size))
        (:LB (block-arrow-make-points-LB ptA ptB width radius margin both t length size))
        (:BL (block-arrow-make-points-LB ptB ptA width radius margin t both length size))
        ;; U type
        (:TT (block-arrow-make-points-TT ptA ptB width radius margin both t length size spacing))
        (:BB (block-arrow-make-points-BB ptA ptB width radius margin both t length size spacing))
        (:LL (block-arrow-make-points-LL ptA ptB width radius margin both t length size spacing))
        (:RR (block-arrow-make-points-RR ptA ptB width radius margin both t length size spacing))
        ;; Z type
        (:LR (block-arrow-make-points-RL ptB ptA width radius margin t both length size spacing))
        (:RL (block-arrow-make-points-RL ptA ptB width radius margin both t length size spacing))
        (:TB (block-arrow-make-points-BT ptB ptA width radius margin t both length size spacing))
        (:BT (block-arrow-make-points-BT ptA ptB width radius margin both t length size spacing)))
    (if (null pts1)
        (throw-exception "Invalid points ~A & ~A for (block-arrow :style :~A)." ptA ptB style)
        (values pts1 pts2))))

(defun block-arrow-draw-controls (debug debug-points clip-path writer)
  (labels ((format-points (pts st)
             (when pts
               (let ((pt (car pts)))
                 (format st " ~A,~A"
                         (coerce (point-x pt) 'single-float)
                         (coerce (point-y pt) 'single-float)))
               (format-points (cdr pts) st)))
           (draw-points (pts)
             (when pts
               (let ((pt (car pts)))
                 (writer-write writer "<circle "
                                      "cx='" (point-x pt) "' "
                                      "cy='" (point-y pt) "' "
                                      "r='2' stroke='none' "
                                      (to-property-strings (make-fill :color debug))
                                      (write-when clip-path "clip-path='url(#" it ")' ")
                                      "/>")
                 (draw-points (cdr pts))))))
    (writer-write writer
                  "<polyline fill='none' "
                  (to-property-strings (make-stroke :color debug :dasharray '(2 4)))
                  "points='" (with-output-to-string (st)
                               (format-points debug-points st)) "' "
                  (write-when clip-path "clip-path='url(#" it ")' ")
                  "/>")
    (draw-points debug-points)))

;; returns multi-value. x, y, and sin/cos in point (x, y).
(defun block-arrow-get-center (ent)
  (labels ((make-lengths (lst acc)
             (if (null lst)
                 (nreverse acc)
                 (let ((pt (car lst)))
                   (setf lst (cdr lst))
                   (when lst
                     (push (point-distance pt (car lst)) acc))
                   (make-lengths lst acc)))))
    (let ((points (slot-value ent 'debug-points)))
      (unless (<= 2 (length points))
        (throw-exception "Can't get center of line."))
      (let* ((lengths (make-lengths points nil))
             (half    (/ (apply #'+ lengths) 2))
             (acc (do ((acc (car lengths)
                            (+ acc (car lengths))))
                      ((< half acc) acc)
                    (setf points  (cdr points))
                    (setf lengths (cdr lengths))))
             (prev (- acc (car lengths)))
             (ratio (/ (- half prev) (- acc prev))))
#|
#|DBG|#  (dolist (pt points)
#|DBG|#    (class:with-access pt
#|DBG|#      (format t "(~A, ~A).~%" pt.x pt.y)))
#|DBG|#  (format t "lengths : ~A~%" lengths)
#|DBG|#  (format t "half    : ~A~%" half)
#|DBG|#  (format t "prev    : ~A~%" prev)
#|DBG|#  (format t "acc     : ~A~%" acc)
#|DBG|#  (format t "ratio   : ~A~%" ratio)
 |#
        (let ((pt1 (car  points))
              (pt2 (cadr points)))
          (values (+ (point-x pt1) (* ratio (- (point-x pt2) (point-x pt1))))
                  (+ (point-y pt1) (* ratio (- (point-y pt2) (point-y pt1))))
                  (math/sin2 pt1 pt2)
                  (math/cos2 pt1 pt2)))))))

;;------------------------------------------------------------------------------
;;
;; class block-arrow
;;
;;------------------------------------------------------------------------------
(defclass block-arrow (path)
  ((pt1          :initform nil :initarg :pt1)            ; point
   (pt2          :initform nil :initarg :pt2)            ; point
   (width        :initform nil :initarg :width)          ; number
   (style        :initform :CC :initarg :style)          ; :CC etc.
   (both         :initform nil :initarg :both)           ; boolean
   (radius       :initform nil :initarg :radius)         ; number
   (length       :initform nil :initarg :length)         ; number
   (size         :initform nil :initarg :size)           ; number
   (margin       :initform nil :initarg :margin)         ; number
   (spacing      :initform nil :initarg :spacing)        ; number
   (debug        :initform nil :initarg :debug)          ; boolean
   (debug-points :initform nil :initarg :debug-points))) ; list of points


(defmethod initialize-instance :after ((ent block-arrow) &rest initargs)
  (declare (ignore initargs))
  (with-slots (style radius width margin size length debug) ent
    (setf style  (or style          :CC))
    (setf width  (or width           10))
    (setf radius (or radius          10))
    (setf margin (or margin           0))
    (setf size   (or size   (* width 2)))
    (setf length (or length        size))
    (when debug
      (setf debug (if (keywordp debug) debug :red))))
  ent)

(defmethod check ((ent block-arrow) canvas dict)
  (declare (ignorable dict))
  (with-slots (data pt1 pt2 width style both radius
               length size margin spacing debug debug-points) ent
    ;; path::check において sub canvas を 考慮する処理が行なわれるため、ここでは canvas-fix-point はしない
    ;(setf pt1 (canvas-fix-point canvas pt1))
    ;(setf pt2 (canvas-fix-point canvas pt2))
    (check-member width    :nullable nil :types number)
    (check-member radius   :nullable nil :types number)
    (check-member length   :nullable nil :types number)
    (check-member size     :nullable nil :types number)
    (check-member margin   :nullable nil :types number)
    (check-member spacing  :nullable nil :types number)
    (check-member debug    :nullable   t :types keyword)
    (multiple-value-bind (points1 points2)
        (block-arrow-make-points pt1 pt2 style width both radius margin length size spacing)
      (setf data points1)
      ;; しかし debug-points は path に渡らないため、ここで canvas-fix-point する
      (setf debug-points (mapcar (lambda (pt)
                                   (canvas-fix-point canvas pt)) points2))))
  ;; this method must call super class' one.
  (call-next-method))


(defmethod entity-composition-p ((ent block-arrow))
  (with-slots (debug) ent
    (not (null debug))))

(defmethod draw-entity ((ent block-arrow) writer)
  (with-slots (debug debug-points clip-path) ent
    ;; block-arrow 本体の描画
    (call-next-method)
    ;; debug mode なら中心と始点、終点、およびそれらの間の線を描く
    (when debug
      (block-arrow-draw-controls debug debug-points clip-path writer))))


(defmethod attribute-center ((ent block-arrow))
  (multiple-value-bind (x y) (block-arrow-get-center ent)
    (make-point x y)))

(defmethod attribute-end1 ((ent block-arrow))
  (slot-value ent 'pt1))

(defmethod attribute-end2 ((ent block-arrow))
  (slot-value ent 'pt2))


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro block-arrow
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{block-arrow}} pt1 pt2 ${KEY} style both width radius length size margin spacing debug fill stroke filter layer id
;;
;;<!-- stack:pop li -->
;;
;;${ARGS_AND_VALS}
;;
;;* `pt1` ---- 始点を指定します。コネクタとは異なり、点での指定のみが可能です。詳細は「[](#座標と位置)」を参照してください。
;;* `pt2` ---- 終点を指定します。コネクタとは異なり、点での指定のみが可能です。詳細は「[](#座標と位置)」を参照してください。
;;* `style` ---- ブロック矢印の引き方を指定します。 `pt1 pt2` が点のみでの指定となるため、`style` は `:CC` または `T B L R` を使った2文字のキーワードだけが利用可能です。たとえば `:BL` であれば「Bottom to Left」という意味になります。
;;* `both` ---- 始点と終点の両方に矢印を描画するか否かを真偽値で指定します。nil の場合、終点側にのみ矢印が描画されます。省略した場合は with-block-arrow-options マクロでの指定値が使用され、それも未指定の場合はデフォルト値として nil が使用されます。
;;* `width` ---- 矢印の幅を数値で指定します。省略した場合は with-block-arrow-options マクロでの指定値が使用され、それも未指定の場合はデフォルト値として 10 が使用されます。
;;* `radius` ---- ブロック矢印が途中で折れ曲がる場合の外側の角の丸め半径を数値で指定します。省略した場合は with-block-arrow-options マクロでの指定値が使用され、それも未指定の場合はデフォルト値として 0 が使用されます。
;;* `length` ---- 矢印部分の長さを数値で指定します。省略した場合は with-block-arrow-options マクロでの指定値が使用され、それも未指定の場合はデフォルト値として `size` と同じ値が使用されます。
;;* `size` ---- 矢印部分の大きさを数値で指定します。省略した場合は with-block-arrow-options マクロでの指定値が使用され、それも未指定の場合はデフォルト値として `width` の２倍の値が使用されます。
;;* `margin` ---- 始点・終点とブロック矢印の間にあける隙間を数値で指定します。省略した場合は with-block-arrow-options マクロでの指定値が使用され、それも未指定の場合はデフォルト値として 0 が使用されます。
;;* `spacing` ---- 2 回折れ曲がるブロック矢印における「自由な線分」の位置を調整するためのパラメータです。詳細は [$@ 節](#コネクタ)を参照してください。ただし、ブロック矢印では最大でも２回しか折れ曲がらないため、ここで指定できるのは数値のみとなります。
;;* `debug` ---- 補助線を描画する場合、`t` または色名を指定します。
;;* `fill` ---- 内部の塗り潰しを指定します。
;;* `stroke` ----  外枠を描画する線を指定します。
;;* `filter` ---- フィルタを適用したい場合、その ID をキーワードシンボルで指定します
;;* `layer` ---- レイヤーを指定する場合、その ID をキーワードシンボルで指定します
;;* `id` ---- ID を付与したい場合、その名前をキーワードシンボルで指定します
;;
;;${DESCRIPTION}
;;
;;　ブロック矢印を描画します。複数のブロック矢印でスタイルを統一したい場合、
;;with-block-arrow-options マクロを使うことができます。
;;
;;${SEE_ALSO}
;;
;;* ブロック矢印
;;* with-block-arrow-options マクロ
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :block-arrow
 |#
(defmacro block-arrow (pt1 pt2 &key (style   :CC)
                                    (both    nil both-p)
                                    (width   nil width-p)
                                    (radius  nil radius-p)
                                    (length  nil length-p)
                                    (size    nil size-p)
                                    (margin  nil margin-p)
                                    (spacing 0)
                                    (debug   nil)
                                    fill stroke filter layer id)
  `(register-entity (make-instance 'kaavio::block-arrow
                                   :pt1          ,pt1
                                   :pt2          ,pt2
                                   :style        ,style
                                   :both         (if ,both-p   ,both   *default-block-arrow-both*)
                                   :width        (if ,width-p  ,width  *default-block-arrow-width*)
                                   :radius       (if ,radius-p ,radius *default-block-arrow-radius*)
                                   :length       (if ,length-p ,length *default-block-arrow-length*)
                                   :size         (if ,size-p   ,size   *default-block-arrow-size*)
                                   :margin       (if ,margin-p ,margin *default-block-arrow-margin*)
                                   :spacing      ,spacing
                                   :debug        ,debug
                                   :debug-points nil
                                   ;;path
                                   :data         nil
                                   :fill         (or ,fill   *default-block-arrow-fill*)
                                   :stroke       (or ,stroke *default-block-arrow-stroke*)
                                   :clip-path    *current-clip-path*
                                   :filter       (or ,filter
                                                     *default-block-arrow-filter*
                                                     *default-filter*)
                                   ;;entity
                                   :id           ,id
                                   :layer        (or ,layer
                                                     *default-block-arrow-layer*
                                                     *default-layer*))))

