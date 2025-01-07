#|
#|ASD|#                (:file "pathutil"                  :depends-on ("kaavio"))
#|EXPORT|#                ;pathutil.lisp
 |#


(in-package :kaavio)

;;;----------------------------------------------------------------
;;; internal functions
;;;----------------------------------------------------------------
;;#+clisp
;;(defun clisp-subdirectories-wildcard (wildcard)
;;  (make-pathname
;;   :directory (append (pathname-directory wildcard) (list :wild))
;;   :name nil
;;   :type nil
;;   :defaults wildcard))
;;
;;
;;
;;;----------------------------------------------------------------
;;; exported functions
;;;----------------------------------------------------------------
(defun path/is-directory-name (p)
  "Is the given pathname the name of a directory? This function canfile-exists-p
usefully be used to test whether a name returned by LIST-DIRECTORIES
or passed to the function in WALK-DIRECTORY is the name of a directory
in the file system since they always return names in `directory normal
form'."
  (flet ((component-present-p (value)
           (and value (not (eql value :unspecific)))))
    (and 
     (not (component-present-p (pathname-name p)))
     (not (component-present-p (pathname-type p)))
     p)))


(defun path/is-file-name (p)
  (unless (path/is-directory-name p) p))

(defun path/is-exists (pathname)
  "Similar to CL:PROBE-FILE except it always returns directory names
in `directory normal form'. Returns truename which will be in
`directory form' if file named is, in fact, a directory."

  #+(or sbcl lispworks openmcl)
  ;; These implementations do "The Right Thing" as far as we are
  ;; concerned. They return a truename of the file or directory if it
  ;; exists and the truename of a directory is in directory normal
  ;; form.
  (probe-file pathname)

  #+(or allegro cmu)
  ;; These implementations accept the name of a directory in either
  ;; form and return the name in the form given. However the name of a
  ;; file must be given in file form. So we try first with a directory
  ;; name which will return NIL if either the file doesn't exist at
  ;; all or exists and is not a directory. Then we try with a file
  ;; form name.
  (or (probe-file (get-as-directory-name pathname))
      (probe-file pathname))

  #+clisp
  ;; Once again CLISP takes a particularly unforgiving approach,
  ;; signalling ERRORs at the slightest provocation.

  ;; pathname in file form and actually a file      -- (probe-file file)      ==> truename
  ;; pathname in file form and doesn't exist        -- (probe-file file)      ==> NIL
  ;; pathname in dir form and actually a directory  -- (probe-directory file) ==> truename
  ;; pathname in dir form and doesn't exist         -- (probe-directory file) ==> NIL

  ;; pathname in file form and actually a directory -- (probe-file file)      ==> ERROR
  ;; pathname in dir form and actually a file       -- (probe-directory file) ==> ERROR
  (or (ignore-errors
        ;; PROBE-FILE will return the truename if file exists and is a
        ;; file or NIL if it doesn't exist at all. If it exists but is
        ;; a directory PROBE-FILE will signal an error which we
        ;; ignore.
        (probe-file (get-as-file-name pathname)))
      (ignore-errors
        ;; PROBE-DIRECTORY returns T if the file exists and is a
        ;; directory or NIL if it doesn't exist at all. If it exists
        ;; but is a file, PROBE-DIRECTORY will signal an error.
        (let ((directory-form (get-as-directory-name pathname)))
          (when (ext:probe-directory directory-form)
            directory-form))))


    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented"))

(defun path/is-existing-file (name)
  "Is `name' the name of an existing file, i.e. not a directory."
  (let ((truename (path/is-exists name)))
    (and truename (path/is-file-name name))))


; example (path/get-time "foo.txt")         => "2012/11/14 18:34:35"
; example (path/get-time "foo.txt" :string) => "2012/11/14 18:34:35"
; example (path/get-time "foo.txt" :value)  => 3561874475
; example (path/get-time "foo.txt" :values) => <multiple values>
; example (path/get-time "foo.txt" '(:year :month ...) => '(2012 11 ...)
(defun path/get-time (pathname &optional (type :string))
  (let ((file-time (file-write-date pathname)))
    (if (eq type :value)
        file-time
        (multiple-value-bind (second minute hour date month year day daylight-p zone)
            (decode-universal-time file-time)
          (if (eq type :string)
              (format nil
                      "~4,'0D/~2,'0D/~2,'0D ~2,'0D:~2,'0D:~2,'0D"
                      year month date hour minute second)
              (if (eq type :values)
                  (values second minute hour date month year day daylight-p zone)
                  (if (not (listp type))
                      file-time
                      (labels ((imp (lst acc)
                                 (if (null lst)
                                     (nreverse acc)
                                     (progn
                                       (case (car lst)
                                         ((:second)     (push second acc))
                                         ((:minute)     (push minute acc))
                                         ((:hour)       (push hour acc))
                                         ((:date)       (push date acc))
                                         ((:month)      (push month acc))
                                         ((:year)       (push year acc))
                                         ((:day)        (push day acc))
                                         ((:daylight-p) (push daylight-p acc))
                                         ((:zone)       (push zone acc)))
                                       (imp (cdr lst) acc)))))
                        (imp type nil)))))))))


(defun path/get-current-directory ()
  #+sbcl
  (truename (sb-posix:getcwd))
  #+clisp
  (ext:default-directory)
;  #+cmucl
;  (extensions:default-directory)
;  #-(or sbcl clisp cmucl)
  #-(or sbcl clisp)
  (error "path/get-current-directory not implemented."))

(defun path/set-current-directory (new-pathname)
  #+sbcl
  (sb-posix:chdir new-pathname)
  #+clisp
  (ext:cd new-pathname)
;  #+cmucl
;  (setf (extensions:default-directory) new-pathname)
;  #-(or sbcl clisp cmucl)
  #-(or sbcl clisp)
  (error "set-current-directory not implemented."))

