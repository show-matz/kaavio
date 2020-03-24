#|
#|ASD|#				(:file "pathutil"                  :depends-on ("cl-diagram"))
#|EXPORT|#				;pathutil.lisp
 |#


(in-package :cl-diagram)

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

;;(defun get-as-directory-name (name)
;;  "Return a pathname reperesenting the given pathname in
;;`directory normal form', i.e. with all the name elements in the
;;directory component and NIL in the name and type components. Can
;;not be used on wild pathnames because there's not portable way to
;;convert wildcards in the name and type into a single directory
;;component. Returns its argument if name and type are both nil or
;;:unspecific."
;;  (let ((pathname (pathname name)))
;;    (when (wild-pathname-p pathname)
;;      (error "Can't reliably convert wild pathnames."))
;;    (if (not (path/is-directory-name name))
;;      (make-pathname 
;;       :directory (append (or (pathname-directory pathname) (list :relative))
;;                          (list (file-namestring pathname)))
;;       :name      nil
;;       :type      nil
;;       :defaults pathname)
;;      pathname)))
;;
;;(defun get-as-file-name (name)
;;  "Return a pathname reperesenting the given pathname in `file form',
;;i.e. with the name elements in the name and type component. Can't
;;convert wild pathnames because of problems mapping wild directory
;;component into name and type components. Returns its argument if
;;it is already in file form."
;;  (let ((pathname (pathname name)))
;;    (when (wild-pathname-p pathname)
;;      (error "Can't reliably convert wild pathnames."))
;;    (if (path/is-directory-name name)
;;      (let* ((directory (pathname-directory pathname))
;;             (name-and-type (pathname (first (last directory)))))
;;        (make-pathname 
;;         :directory (butlast directory)
;;         :name (pathname-name name-and-type)
;;         :type (pathname-type name-and-type)
;;         :defaults pathname))
;;      pathname)))

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

;;(defun is-existing-directory (name)
;;  "Is `name' the name of an existing directory."
;;  (let ((truename (path/is-exists name)))
;;    (and truename (path/is-directory-name name))))

(defun path/is-existing-file (name)
  "Is `name' the name of an existing file, i.e. not a directory."
  (let ((truename (path/is-exists name)))
    (and truename (path/is-file-name name))))

;;(defun add-wildcard-to-directory (dirname)
;;  (make-pathname :name :wild
;;				 :type #-clisp :wild #+clisp nil
;;				 :defaults (get-as-directory-name dirname)))
;;
;;(defun list-directory (dirname)
;;  "Return a list of the contents of the directory named by dirname.
;;Names of subdirectories will be returned in `directory normal
;;form'. Unlike CL:DIRECTORY, LIST-DIRECTORY does not accept
;;wildcard pathnames; `dirname' should simply be a pathname that
;;names a directory. It can be in either file or directory form."
;;  (when (wild-pathname-p dirname)
;;    (error "Can only list concrete directory names."))
;;
;;  (let ((wildcard (add-wildcard-to-directory dirname)))
;;
;;    #+(or sbcl cmu lispworks)
;;    ;; SBCL, CMUCL, and Lispworks return subdirectories in directory
;;    ;; form just the way we want.
;;    (directory wildcard)
;;    
;;    #+openmcl
;;    ;; OpenMCl by default doesn't return subdirectories at all. But
;;    ;; when prodded to do so with the special argument :directories,
;;    ;; it returns them in directory form.
;;    (directory wildcard :directories t)
;;            
;;    #+allegro
;;    ;; Allegro normally return directories in file form but we can
;;    ;; change that with the :directories-are-files argument.
;;    (directory wildcard :directories-are-files nil)
;;            
;;    #+clisp
;;    ;; CLISP has a particularly idiosyncratic view of things. But we
;;    ;; can bludgeon even it into doing what we want.
;;    (nconc 
;;     ;; CLISP won't list files without an extension when :type is
;;     ;; wild so we make a special wildcard for it.
;;     (directory wildcard)
;;     ;; And CLISP doesn't consider subdirectories to match unless
;;     ;; there is a :wild in the directory component.
;;     (directory (clisp-subdirectories-wildcard wildcard)))
;;
;;    #-(or sbcl cmu lispworks openmcl allegro clisp)
;;    (error "list-directory not implemented")))
;;
;;; in Visual Basic...
;;;Public Function OnNotifyEnumFile(pathName As String, _
;;;                                 fileName As String, _
;;;                                 ByRef opaque As Variant) As Boolean
;;;End Function
;;
;;(defun walk-directories (dirname notify-fnc &key directories (depth -1) (pred-fnc (constantly t)))
;;  "Walk a directory invoking `notify-fnc' on each pathname found.
;;If `pred-fnc' is supplied notify-fnc is invoked only on pathnames
;;for which `pred-fnc' returns true. If `directories' is t invokes 
;;`pred-fnc' and `notify-fnc' on directory pathnames as well."
;;  (labels ((imp (name level)
;;			 (if (and (<= 0 depth) (< depth level))
;;				 t
;;				 (if (not (path/is-directory-name name))
;;					 (if (funcall pred-fnc name)
;;						 (funcall notify-fnc name))
;;					 (progn
;;					   (when (and directories (funcall pred-fnc name))
;;						 (funcall notify-fnc name))
;;					   (dolist (x (list-directory name))
;;						 (imp x (1+ level))))))))
;;    (imp (get-as-directory-name dirname) -1)))


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
										 ((:second)		(push second acc))
										 ((:minute)		(push minute acc))
										 ((:hour)		(push hour acc))
										 ((:date)		(push date acc))
										 ((:month)		(push month acc))
										 ((:year)		(push year acc))
										 ((:day)		(push day acc))
										 ((:daylight-p)	(push daylight-p acc))
										 ((:zone)		(push zone acc)))
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

;;(defun set-current-directory (new-pathname)
;;  #+sbcl
;;  (sb-posix:chdir new-pathname)
;;  #+clisp
;;  (ext:cd new-pathname)
;;;  #+cmucl
;;;  (setf (extensions:default-directory) new-pathname)
;;;  #-(or sbcl clisp cmucl)
;;  #-(or sbcl clisp)
;;  (error "set-current-directory not implemented."))
;;
;;
;;  
;;
;;;(let ((sb-impl::*default-external-format* :sjis)
;;;	  (sb-alien::*default-c-string-external-format* :sjis))
;;;	(walk-directories "D:/workspace/scrapbook"
;;;					(lambda (path) (format t "~A~%" path))
;;;					:directories t))
;;
;;;(let ((sb-impl::*default-external-format* :utf-8)
;;;	  (sb-alien::*default-c-string-external-format* :utf-8))
;;;	(load "./test.lisp"))
;;
;;;(walk-directories "D:/workspace/scrapbook"
;;;				(lambda (path) (format t "~A~%" path))
;;;				:directories t))
;;
;;
;;(defun copy-file (from to &key overwrite)
;;  (let ((element-type '(unsigned-byte 8)))
;;	(with-open-file (in from :element-type element-type)
;;	  (with-open-file (out to :element-type element-type
;;							  :direction :output
;;							  :if-exists (if overwrite :supersede :error))
;;		(let* ((buffer-size 8192)
;;			   (buf (make-array buffer-size :element-type element-type)))
;;		  (loop
;;			(let ((pos #-(or :clisp :cmu) (read-sequence buf in)
;;					   #+:clisp           (ext:read-byte-sequence buf in :no-hang nil)
;;					   #+:cmu             (sys:read-n-bytes in buf 0 buffer-size nil)))
;;			  (when (zerop pos)
;;				(return))
;;			  (write-sequence buf out :end pos)))))))
;;  (values))
;;
