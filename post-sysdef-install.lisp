;;; -*- Mode: Lisp; Package: COMMON-LISP-CONTROLLER -*-
;;;
;;; Copyright (C) 2000,2004  Peter Van Eynde and Kevin M. Rosenberg
;;; Licensed under the LLGPL, see debian/copyright file

(in-package #:common-lisp-controller)

(eval-when (:load-toplevel :execute :compile-toplevel)
  (unless (find-package :asdf)
    (error "You need to load the asdf system before loading or compiling this file!")))

(defun calculate-fasl-root  ()
  "Inits common-lisp controller for this user"
  (unless *fasl-root*
    (setf *fasl-root*
	  ;; set it to the username of the user:
	  (let* (#-cmu
		 (homedir (pathname-directory
			   (user-homedir-pathname)))
		 ;; cmucl has searchlist home (!)
		 #+cmu
		 (homedirs (extensions:search-list "home:"))
		 #+cmu
		 (homedir (when homedirs
			    (pathname-directory
			     (first homedirs))))
		 #-cmu
		 (homepath (user-homedir-pathname))
		 #+cmu
		 (homepaths (extensions:search-list "home:"))
		 #+cmu
		 (homepath (when homepaths
			     (first homepaths))))
	    (unless (and homedir homepath)
	      (error "cannot determine homedir?"))
	    ;; strip off :re or :abs
	    (when (or (eq (first homedir)
			  :relative)
		      (eq (first homedir)
			  :absolute))
	      (setf homedir (rest homedir)))
	    ;; if it starts with home, nuke it
	    (when (string= (first homedir)
			   "home")
	      (setf homedir (rest homedir)))
	    ;; now append *implementation-name*
	    (let ((new-root (append homedir
				    (list *implementation-name*))))
	      ;; this should be able to cope with
	      ;; homedirs like /home/p/pv/pvaneynd ...
	      (let ((target (merge-pathnames
			     (make-pathname
			      :directory `(:relative ,@new-root))
			     #p"/var/cache/common-lisp-controller/"))
		    (target-root (merge-pathnames
				  (make-pathname
				   :directory `(:relative ,@homedir))
				  #p"/var/cache/common-lisp-controller/")))
		;; now check if we are the owner of that directory
		;; otherwise another person could pre-load that with
		;; 'bad' fasls
		(ensure-directories-exist target-root :verbose t)
		(unless (string= (file-author homepath)
				 (file-author target-root))
		  (error "security problem: the owner of ~A is not as expected ~A but ~A"
			 target-root
			 (file-author homepath)
			 (file-author target-root)))
		target))))))3

(defun asdf-system-compiled-p (system)
  "Returns T is an ASDF system is already compiled" 
  (notany #'(lambda (op) (and (typep (car op) 'asdf:compile-op)
			      (not (asdf:operation-done-p (car op) (cdr op)))))
	  (asdf::traverse (make-instance 'asdf:compile-op) system)))



(defun beneath-source-root? (c)
  "Returns T if component's directory below *source-root*"
  (when c
    (let ((root-dir (pathname-directory (asdf::resolve-symlinks *source-root*)))
	  (comp-dir (pathname-directory (asdf:component-pathname c))))
      (and (>= (length comp-dir)
	       (length root-dir))
	   (equalp root-dir (subseq comp-dir 0 (length root-dir)))))))

(defun source-root-path-to-fasl-path (source)
  "Converts a path in the source root into the equivalent path in the fasl root"
  (merge-pathnames 
   (enough-namestring source (asdf::resolve-symlinks *source-root*))
   *fasl-root*))

(defmethod asdf:output-files :around ((op asdf:operation) (c asdf:component))
  "Method to rewrite output files to fasl-root"
  (let ((orig (call-next-method)))
    (cond
      ((beneath-source-root? c)
       (calculate-fasl-root)
       (mapcar #'source-root-path-to-fasl-path orig))
      (t
       orig))))

(defun system-in-source-root? (c)
  "Returns T if component's directory is the same as *source-root* + component's name"
  ;; asdf::resolve-symlinks gives an error for non-existent pathnames
  ;; on lispworks
  (ignore-errors
    (and c
	 (equalp (pathname-directory (asdf:component-pathname c))
		 (pathname-directory
		  (asdf::resolve-symlinks
		   (merge-pathnames
		    (make-pathname
		     :directory (list :relative (asdf:component-name c)))
		    *source-root*)))))))

(defun find-system-def (module-name)
  "Looks for name of system. Returns :asdf if found asdf file."
  (cond
   ;; probing is for weenies: just do
   ;; it and ignore the errors :-)
   ((ignore-errors
	  (let ((system (asdf:find-system module-name)))
		(when system
		  :asdf))))
   (t
	nil)))

(defun require-asdf (module-name)
  (let ((system (asdf:find-system module-name)))
    (when system
      (if (asdf-system-compiled-p system)
		  (asdf:oos 'asdf:load-op module-name)
		(progn
		  (format t "~&;;; Please wait, recompiling library...")
		  (asdf:oos 'asdf:compile-op module-name)
		  (terpri)
		  (asdf:oos 'asdf:load-op module-name)))
      t)))

(defun clc-require (module-name &optional (pathname 'clc::unspecified))
  (if (not (eq pathname 'clc::unspecified))
      (common-lisp:require module-name pathname)
	(let ((system-type (find-system-def module-name)))
	  (case system-type
		(:asdf
		 (require-asdf module-name))
		;; Don't call original-require with SBCL since we are called by that function
		#-sbcl 
		(otherwise
		 (common-lisp:require module-name))))))


(defun clc-build-all-packages (&optional (ignore-errors nil))
  "Tries to build all known packages.
Looks in /usr/share/commmon-lisp/systems/ for .asd files
If IGNORE-ERRORS is true ignores all errors while rebuilding"
  (loop for registry-object in asdf:*central-registry*
	for registry-location = (eval registry-object)
	with failed-packages = ()
	finally (when failed-packages
		  (format t "~&~%Failed the following packages failed: ~{~A~^, ~}"
			  failed-packages))
	
	do
	(loop for pathname in (directory
			       (merge-pathnames
				(make-pathname
				 :name :wild
				 :type "asd")
				registry-location))
	      for package-name = (pathname-name pathname)
	      do
	      (restart-case
		  (handler-case
		      (asdf:oos 'asdf:compile-op package-name)
		    (error (error)
		      (cond
			(ignore-errors
			  (format t "~&Ignoring error: ~A~%"
				  error)
			  nil)
			(t
			 (error error)))))
		(skip-package ()
		  (push package-name failed-packages)
		  nil)))))

