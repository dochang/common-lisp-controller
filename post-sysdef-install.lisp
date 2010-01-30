;;; -*- Mode: Lisp; Package: COMMON-LISP-CONTROLLER -*-
;;;
;;; Copyright (C) 2000,2004  Peter Van Eynde and Kevin M. Rosenberg
;;; Licensed under the LLGPL, see debian/copyright file

(in-package #:common-lisp-controller)

(eval-when (:load-toplevel :execute :compile-toplevel)
  (unless (find-package :asdf)
    (error "You need to load the asdf system before loading or compiling this file!")))

;;; ECL has the asdf::build-op operation
#+ecl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf asdf:*enable-asdf-binary-locations* t
	asdf:*centralize-lisp-binaries* t
	asdf:*default-toplevel-directory* #p"/var/cache/common-lisp-controller/"
	asdf:*include-per-user-information* t))
	
(defun find-system-def (module-name)
  "Looks for name of system. Returns :asdf if found asdf file."
  (when (asdf:find-system module-name nil) :asdf))

(defun asdf-system-compiled-p (system)
  "Returns T is an ASDF system is already compiled"
  (notany #'(lambda (op) (and (typep (car op) 'asdf:compile-op)
                             (not (asdf:operation-done-p (car op) (cdr op)))))
         (asdf::traverse (make-instance 'asdf:compile-op) system)))

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
  "Loads the given MODULE-NAME ADSF package, using CLC and with ASDF native as a fallback"
  (asdf:ensure-source-registry)
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
#-ecl
(defun load-component (system)
  (asdf:operate 'asdf:compile-op system)
  (asdf:operate 'asdf:load-op system)
  nil)

#+ecl
(defun load-component (system)
  (asdf:operate 'asdf:compile-op system)
  (asdf:operate 'asdf:load-op system)
  (asdf::get-object-files system))

(defun load-user-image-components ()
  (with-open-file (components (merge-pathnames
			       *image-preferences*
			       *implementation-name*)
			      :direction :input :if-does-not-exist nil)
     (when components
       (let ((asdf:*central-registry*
	      (append asdf:*central-registry* (list *systems-root*))))
	 (loop for component = (read-line components nil)
	       while component nconc
	       (let ((system (asdf:find-system component nil)))
		 (if system (load-component system)
		   (warn "System ~S not found, not loading it into implementation image" component))))))))
  
