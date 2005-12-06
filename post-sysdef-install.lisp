;;; -*- Mode: Lisp; Package: COMMON-LISP-CONTROLLER -*-
;;;
;;; Copyright (C) 2000,2004  Peter Van Eynde and Kevin M. Rosenberg
;;; Licensed under the LLGPL, see debian/copyright file

(in-package #:common-lisp-controller)

(eval-when (:load-toplevel :execute :compile-toplevel)
  (unless (find-package :asdf)
    (error "You need to load the asdf system before loading or compiling this file!")))

(defun get-homedir ()
  #-cmu
  (user-homedir-pathname)
  #+cmu
  (let ((dirs (extensions:search-list "home:")))
    (when dirs
      (first dirs))))

#+clisp
(defun check-spooldir-security (target)
  ;; does target exist?
  (cond
   ;; probe-directory will fail if we are not the owner!
   ((ignore-errors
     (ext:probe-directory target))
    ;; check who owns it
    (let* ((stat (posix:file-stat target))
	   (mode (posix:file-stat-mode stat))
	   (owner (posix:file-stat-uid stat))
	   (me (ext:getenv "USER"))
	   (uid (posix:user-info-uid
		 (posix:user-data me))))
      (unless (= owner uid)
	(error "Security problem: The owner of ~S is not ~S as I wanted"
	       target
	       me))
      (when (or (member :RWXO mode)
		(member :WOTH mode))
	(error "Security problem: the cache directory ~S is writable for other users"
	       target))
      (when (or (member :RWXG mode)
		(member :WGRP mode))
	(error "Security problem: the cache directory ~S is writable for a group, for better security we do not allow this"
	       target))))
   ((not
     (ignore-errors
       (not (ext:probe-directory target))))
    ;; check who owns it
    (error "Security problem: The owner of ~S is incorrect"
	   target))
   (t
    (let ((old-umask (posix:umask #o077)))
      (unwind-protect
	  (ensure-directories-exist target)
	(posix:umask old-umask)))))
  (values))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

#+allegro
(require :osi)

#+sbcl
(defun get-uid-mode-and-my-uid (directory)
  (when (eq :directory
	    (sb-unix:unix-file-kind (namestring directory)))
    ;; check who owns it
    (multiple-value-bind (res dev ino mode nlink uid gid rdev size atime mtime)
	(sb-unix:unix-stat (namestring directory))
      
      (declare (ignore res dev ino nlink gid rdev size atime mtime))

      (values uid mode (sb-unix:unix-getuid)))))

#+cmu
(defun get-uid-mode-and-my-uid (directory)
  (when (eq :directory
	    (unix:unix-file-kind (namestring directory)))
    ;; check who owns it
    (multiple-value-bind (res dev ino mode nlink uid gid rdev size atime mtime)
	(unix:unix-stat (namestring directory))

      (declare (ignore res dev ino nlink gid rdev size atime mtime))
      
      (values uid mode (unix:unix-getuid)))))

#+allegro
(defun get-uid-mode-and-my-uid (directory)
  (when (excl:probe-directory directory)
    ;; check who owns it
    (let* ((stat (excl.osi:stat (namestring directory)))
	   (mode (excl.osi:stat-mode stat))
	   (uid  (excl.osi:stat-uid stat))
	   (my-uid (excl.osi:getuid)))

      (values uid mode my-uid))))

#+sbcl
(defun make-secure-cache-directory (directory)
  (let ((old-umask (sb-posix:umask #o077)))
    (unwind-protect
	(ensure-directories-exist directory)
      (sb-posix:umask old-umask)))
  (values))

#+cmu
(defun make-secure-cache-directory (directory)
  (let ((old-umask (unix::unix-umask #o077)))
    (unwind-protect	   
	(ensure-directories-exist directory)
      (unix::unix-umask old-umask)))
  (values))


#+allegro
(defun make-secure-cache-directory (directory)
  (let ((old-umask (excl.osi:umask #o077)))
      (unwind-protect	   
	  (ensure-directories-exist directory)
	(excl.osi:umask old-umask)))
  (values))

#+(or sbcl cmu allegro)
(defun check-spooldir-security (target)
  ;; does target exist?
  (multiple-value-bind (uid mode my-uid)
      (get-uid-mode-and-my-uid (namestring target))

    (cond
     (uid
      (unless (= uid my-uid)
	(error "Security problem: The owner of ~S is not ~S as I wanted"
	       target
	       my-uid))
      (unless (= 0
		 (logand mode #o002))
	(error "Security problem: the cache directory ~S is writable for other users"
	       target))
      (unless (= 0
		 (logand mode #o020))
	(error "Security problem: the cache directory ~S is writable for a group, for better security we do not allow this"
	       target)))
     (t
      ;; does not exist, make it
      (make-secure-cache-directory target))))
  (values))

;; sucks but is portable ;-(
#-(or cmu sbcl clisp allegro)
(defun check-spooldir-security (target)
  #+(or)
  (cerror "I have checked this"
	  "The security of the directory ~A cannot be checked.
Please check if you are the owner of that directory and
that the permissions are such that nobody can write to it."
	  target)
  (let ((result
	 (asdf:run-shell-command "perl -W -e 'use File::stat; use User::pwent;
umask 077;
if (! -d \"~A~:*\") {
  mkdir \"~A~:*\";
}~
my $stat=stat(\"~A~:*\") || exit 42;
if (($stat->mode & 0040000) == 0) {
   exit 43;
}
if (! -O \"~A~:*\" ) {
   exit 44;
}
if (($stat->mode & 002) != 0) {
   exit 45;
}
if (($stat->mode & 020) != 0) {
   exit 46;
}
exit 0;' 2>&1 3>&1"
				     target)))
    (case result
      (0 nil)
      (42 (error "Security problem: Could not stat ~A" target))
      (43 (error "Security problem: ~A is not a directory" target))
      (44 (error "Security problem: ~A is not a owned by you" target))
      (45 (error "Security problem: ~A is world writable" target))
      (46 (error "Security problem: ~A is writable by a group, we do not allow this" target)))))


(defun calculate-fasl-root  ()
  "Inits common-lisp controller for this user"
  (or *fasl-root*
    (setf *fasl-root*
	  ;; set it to the username of the user:
	  (let* ((homedir (pathname-directory
			   (get-homedir))))
	    (unless homedir
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
	    

	    ;; this should be able to cope with
	    ;; homedirs like /home/p/pv/pvaneynd ...
	    (let ((target (merge-pathnames
			   (make-pathname
			    :directory `(:relative ,@homedir
						   ;; now append *implementation-name*
						   ,*implementation-name*))
			   #p"/var/cache/common-lisp-controller/"))
		  (target-root (merge-pathnames
				(make-pathname
				 :directory `(:relative ,@homedir))
				#p"/var/cache/common-lisp-controller/")))

	      (check-spooldir-security target-root)
	      
	      target)))))

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
		  (unless *clc-quiet*
		    (format t "~&;;; Please wait, recompiling library..."))
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

(defun list-systems ()
  (let ((systems (make-hash-table :test #'equal)))
    (loop :for item :in asdf:*central-registry*
	  :for location = (eval item)
	  :for files = (when location
			 (directory
			  (merge-pathnames
			   (make-pathname
			    :version :newest
			    :name :wild
			    :type "asd"
			    :case :local)
			   location)))
	  :when  files
	  :do
	  (loop :for filename :in files
		:for system = (when filename
				(pathname-name filename))
		:do
		(setf (gethash system systems)
		      system)))
    (format t
	    "~&Known systems:~%~@<~;:~A~_ ~;~:>~%"
	    (sort 
	     (loop :for system :being :the :hash-key :of systems
		   :collect system)
	     #'string<))
    (values)))
