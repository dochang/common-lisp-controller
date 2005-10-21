;;; -*- Mode: Lisp; Package: COMMON-LISP-CONTROLLER -*-
;;;
;;; Copyright (C) 2000,2004  Peter Van Eynde and Kevin M. Rosenberg
;;; Licensed under the LLGPL, see debian/copyright file


(in-package #:cl-user)

(defpackage #:common-lisp-controller
  (:use #:common-lisp)
  (:export #:init-common-lisp-controller
	   #:init-common-lisp-controller-v4
	   #:clc-require
	   #:clc-build-all-packages
	   #:*clc-quiet*
	   ;; depricated:
	   #:make-clc-send-command-string
	   #:send-clc-command)
  (:nicknames #:clc
	      ; depricated:
	      #:c-l-c))


(in-package #:common-lisp-controller)

(defvar *clc-quiet* nil
  "If true then clc prints no messages")

;; Some general utilities to make the
;; descriptions shorter

(defvar *fasl-type*
  (load-time-value
   (pathname-type
    (compile-file-pathname "foo.lisp")))
  "This is the type of compiled lisp files.")

(defvar *fasl-root* nil "Root of implementation's directories of binary files")
(defvar *source-root* #p"/usr/share/common-lisp/source/"
	"Root of source directories")
(defvar *systems-root* #p"/usr/share/common-lisp/systems/"
        "Root of systems directory")
(defvar *implementation-name* nil "The name of the implementation,
used to name the directory in /var/cache/common-lisp-controller")

(define-modify-macro appendf (&rest lists) append)

(defun init-common-lisp-controller (fasl-root
                                    &key
                                    (source-root "/usr/share/common-lisp/")
                                    (version 2))
  (declare (ignore source-root version))
  ;; vodoo: extract the name of the implementation
  ;; from the old fasl directory... 
  (init-common-lisp-controller-v4
   (first
    (last
     (pathname-directory
      (parse-namestring
       fasl-root))))))

(defun init-common-lisp-controller-v4 (implementation-name)
  "configures common-lisp-controller. IMPLEMENTATION-NAME
is the name of this implementation.
Fasl's will be created in /var/cache/common-lisp-controller/<userid>/<implementation>"

  (setf *implementation-name* implementation-name)

  ;; force both parameters to directories...
  (let* ((fasl-root (merge-pathnames
		     (make-pathname
		      :directory
		      `(:relative "root" ,*implementation-name*))
		     #p"/var/cache/common-lisp-controller/")))
    (flet ((compile-and-load (package-name filename)
	     (let* ((file (parse-namestring filename))
		    (file-path
		     (merge-pathnames
		      (make-pathname :name (pathname-name file)
				     :type (pathname-type file)
				     :directory (list :relative package-name))
		      *source-root*))
		    (output-path
		     (merge-pathnames
		      (make-pathname :name (pathname-name file)
				     :type (pathname-type file)
				     :directory (list :relative package-name))
		      fasl-root))
		    (compiled-file-pathname
		     (compile-file-pathname output-path)))
	       ;; first make the target directory:
	       (ensure-directories-exist compiled-file-pathname)
	       ;; now compile it:
	       (compile-file file-path
			     :output-file compiled-file-pathname
			     :print nil
			     :verbose nil)
	       ;; then load it:
	       (load compiled-file-pathname
		     :verbose nil
		     :print nil))))
      ;; first ourselves:
      (compile-and-load  "common-lisp-controller"
			 "common-lisp-controller.lisp")
      ;; then asdf:
      ;; For SBCL, take advantage of it's REQUIRE/contrib directories integration
      #+sbcl
      (when (boundp 'sb-ext::*module-provider-functions*)
	(pushnew :sbcl-hooks-require cl:*features*))
      (compile-and-load  "asdf" "asdf.lisp")
      (compile-and-load  "asdf" "wild-modules.lisp")
      ;; now patch it::
      (compile-and-load "common-lisp-controller"
			"post-sysdef-install.lisp")
      #+sbcl
      (setq cl:*features* (delete :sbcl-hooks-require  cl:*features*))

      ;; register the systems root:
      (appendf 
           (symbol-value (intern (symbol-name :*central-registry*)
                                 (find-package :asdf)))
           (list *systems-root*))

      (appendf 
            (symbol-value (intern (symbol-name :*central-registry*)
                                  (find-package :asdf)))
            (list '(merge-pathnames ".clc/systems/" 
                             (user-homedir-pathname))))))
  (values))




