#|
#|ASD|#             (:file "binutil"                   :depends-on ("kaavio"))
#|EXPORT|#              ;binutil.lisp
 |#

(in-package :kaavio)


#+little-endian (defparameter *default-endian* :little)
#+big-endian    (defparameter *default-endian* :big)


;;------------------------------------------------------------------------------
;;
;; base class bin/read-stream
;;
;;------------------------------------------------------------------------------
(defclass bin/read-stream () ())

(defgeneric bin/create-read-stream (source))  ; returns read-stream instance.
(defgeneric bin/get-byte (rs))                ; returns byte
(defgeneric bin/seek-relative (rs offset))    ; integer value
(defgeneric bin/seek-absolute (rs pos))       ; :start|:end|integer value
(defgeneric bin/stream-length (rs))
(defgeneric bin/stream-position (rs))
(defgeneric bin/close-stream (rs))



;;------------------------------------------------------------------------------
;;
;; class bin/file-read-stream
;;
;;------------------------------------------------------------------------------
(defclass bin/file-read-stream (bin/read-stream)
  ((handle :initform nil
           :initarg  :handle
           :accessor __file-read-stream-handle)))


(defmethod bin/create-read-stream ((source string))
  (make-instance 'bin/file-read-stream
                 :handle (open (pathname source)
                               :direction :input
                               :element-type '(unsigned-byte 8))))


(defmethod bin/create-read-stream ((source pathname))
  (make-instance 'bin/file-read-stream
                 :handle (open source
                               :direction :input
                               :element-type '(unsigned-byte 8))))

; return byte
(defmethod bin/get-byte ((frs bin/file-read-stream))
  (cl:read-byte (__file-read-stream-handle frs)))

; integer value
(defmethod bin/seek-relative ((frs bin/file-read-stream) offset)
  (let* ((handle (__file-read-stream-handle frs))
         (pos    (file-position handle)))
    (cl:file-position handle (+ pos offset))))

; :start|:end|integer value
(defmethod bin/seek-absolute ((frs bin/file-read-stream) pos)
  (cl:file-position (__file-read-stream-handle frs) pos))

(defmethod bin/stream-length ((frs bin/file-read-stream))
  (cl:file-length (__file-read-stream-handle frs)))

(defmethod bin/stream-position ((frs bin/file-read-stream))
  (cl:file-position (__file-read-stream-handle frs)))

(defmethod bin/close-stream ((frs bin/file-read-stream))
  (cl:close (__file-read-stream-handle frs))
  (setf (__file-read-stream-handle frs) nil))



;;------------------------------------------------------------------------------
;;
;; macro bin/with-read-stream
;;
;;------------------------------------------------------------------------------
(defmacro bin/with-read-stream ((stream source) &rest body)
  `(let ((,stream (bin/create-read-stream ,source)))
     (unwind-protect
          (progn
            ,@body)
       (bin/close-stream ,stream))))



;;------------------------------------------------------------------------------
;;
;; method bin/read-value
;;
;;------------------------------------------------------------------------------
(defgeneric bin/read-value (type stream &key))

(defmethod bin/read-value ((type (eql :uint8)) (stream bin/read-stream) &key)
  (bin/get-byte stream))

(defmethod bin/read-value ((type (eql :uint16))
                           (stream bin/read-stream) &key (endian *default-endian*))
  (let ((value 0))
    (ecase endian
      ((:little)
       (setf (ldb (byte 8  0) value) (bin/get-byte stream))
       (setf (ldb (byte 8  8) value) (bin/get-byte stream)))
      ((:big)
       (setf (ldb (byte 8  8) value) (bin/get-byte stream))
       (setf (ldb (byte 8  0) value) (bin/get-byte stream))))
    value))

(defmethod bin/read-value ((type (eql :uint32))
                           (stream bin/read-stream) &key (endian *default-endian*))
  (let ((value 0))
    (ecase endian
      ((:little)
       (setf (ldb (byte 8  0) value) (bin/get-byte stream))
       (setf (ldb (byte 8  8) value) (bin/get-byte stream))
       (setf (ldb (byte 8 16) value) (bin/get-byte stream))
       (setf (ldb (byte 8 24) value) (bin/get-byte stream)))
      ((:big)
       (setf (ldb (byte 8 24) value) (bin/get-byte stream))
       (setf (ldb (byte 8 16) value) (bin/get-byte stream))
       (setf (ldb (byte 8  8) value) (bin/get-byte stream))
       (setf (ldb (byte 8  0) value) (bin/get-byte stream))))
    value))

(defmethod bin/read-value ((type (eql :uint64))
                           (stream bin/read-stream) &key (endian *default-endian*))
  (let ((value 0))
    (ecase endian
      ((:little)
       (setf (ldb (byte 8  0) value) (bin/get-byte stream))
       (setf (ldb (byte 8  8) value) (bin/get-byte stream))
       (setf (ldb (byte 8 16) value) (bin/get-byte stream))
       (setf (ldb (byte 8 24) value) (bin/get-byte stream))
       (setf (ldb (byte 8 32) value) (bin/get-byte stream))
       (setf (ldb (byte 8 40) value) (bin/get-byte stream))
       (setf (ldb (byte 8 48) value) (bin/get-byte stream))
       (setf (ldb (byte 8 56) value) (bin/get-byte stream)))
      ((:big)
       (setf (ldb (byte 8 56) value) (bin/get-byte stream))
       (setf (ldb (byte 8 48) value) (bin/get-byte stream))
       (setf (ldb (byte 8 40) value) (bin/get-byte stream))
       (setf (ldb (byte 8 32) value) (bin/get-byte stream))
       (setf (ldb (byte 8 24) value) (bin/get-byte stream))
       (setf (ldb (byte 8 16) value) (bin/get-byte stream))
       (setf (ldb (byte 8  8) value) (bin/get-byte stream))
       (setf (ldb (byte 8  0) value) (bin/get-byte stream))))
    value))

