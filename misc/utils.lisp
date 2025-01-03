(require :kaavio)

(in-package :kaavio)

(defun i-option-p (param)
  (and (< 2 (length param))
       (string= "-I" (subseq param 0 2))))

(defun fix-path (param)
  (let ((path (subseq param 2)))
    (if (char= #\/ (char path (1- (length path))))
        path
        (concatenate 'string path "/"))))

(defun retrieve-include-paths (args &optional paths)
  (if (null args)
      (values (nreverse paths) nil)
      (let ((param (car args)))
        (if (not (i-option-p param))
            (values (nreverse paths) args)
            (retrieve-include-paths (cdr args)
                                    (cons (fix-path param) paths))))))

