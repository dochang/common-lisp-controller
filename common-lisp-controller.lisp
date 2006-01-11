;;; -*- Mode: Lisp; Package: COMMON-LISP-CONTROLLER -*-
;;;
;;; Copyright (C) 2000,2004  Peter Van Eynde and Kevin M. Rosenberg
;;; Licensed under the LLGPL, see debian/copyright file


(in-package #:cl-user)

(defpackage #:common-lisp-controller
  (:use #:common-lisp)
  (:export #:init-common-lisp-controller
	   #:init-common-lisp-controller-v4
	   #:compile-common-lisp-controller-v5
	   #:init-common-lisp-controller-v5
	   #:clc-require
	   #:clc-build-all-packages
	   #:*clc-quiet*
	   #:calculate-fasl-root
	   #:list-systems
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

(defun init-common-lisp-controller-v5 (implementation-name)
  ;; register the systems root:
  (setf *implementation-name* implementation-name)
  
  (pushnew :common-lisp-controller *features*)
  (pushnew :clc-os-debian *features*)

  ;; put the central registry at the *end*
  ;; of the search list
  (appendf 
   (symbol-value (intern (symbol-name :*central-registry*)
			 (find-package :asdf)))
   (list *systems-root*))
  
  ;; put the users asdf files at the FRONT
  ;; of the same search list
  (pushnew '(merge-pathnames ".clc/systems/"
			     (user-homedir-pathname))
	   (symbol-value (intern (symbol-name :*central-registry*)
				 (find-package :asdf)))
	   :test #'equalp))

(defun compile-common-lisp-controller-v5 (implementation-name)
  "Compiles the clc files. Returns a list of fasls
that should be loaded in the list to enable clc"
  (setf *implementation-name* implementation-name)
  
  (pushnew :common-lisp-controller *features*)
  (pushnew :clc-os-debian *features*)
  
  (let* ((fasl-root (merge-pathnames
		     (make-pathname
		      :directory
		      `(:relative "root" ,*implementation-name*))
		     #p"/var/cache/common-lisp-controller/")))
    (labels ((source-filename (package-name filename)
			      (let* ((file (parse-namestring filename))
				     (file-path
				      (merge-pathnames
				       (make-pathname :name (pathname-name file)
						      :type (pathname-type file)
						      :directory (list :relative package-name))
				       *source-root*)))
				file-path))
	     (fasl-filename (package-name filename)
			    ;; this is complex because ecl
			    ;; should produce system fasls,
			    ;; and they have .o extension
			    (let* ((file (parse-namestring filename))
				   (output-path
				    (merge-pathnames
				     (make-pathname :name (pathname-name file)
						    :type #-ecl (pathname-type file)
						          #+ecl "o"
						    :directory (list :relative package-name))
				     fasl-root))
				   (compiled-file-pathname
				    (#-ecl compile-file-pathname
				     #+ecl compile-file-pathname
					   output-path
					   #+ecl #+ecl
					   :output-file :o)))
			      compiled-file-pathname))
	     (compile-and-load (package-name filename)
			       (let* ((file-path (source-filename package-name filename))
				      (compiled-file-pathname
				       (progn
					 ;; first make the target directory:
					 (ensure-directories-exist
					  (fasl-filename package-name filename))
					 ;; now compile it:
					 (compile-file file-path
						       :output-file (fasl-filename package-name filename)
						       :print nil
						       :verbose nil
						       ;; make 'linkable object files' for ecl:
						       #+ecl #+ecl
						       :system-p t))))
				 ;; then load it:
				 (load #-ecl compiled-file-pathname
				       ;; for ecl, first _really compile_ the file, then load it
				       #+ecl (compile-file file-path
							   :output-file
							   (compile-file-pathname
							    (fasl-filename package-name filename))
							   :print nil
							   :verbose nil)
				       :verbose nil
				       :print nil)
				 ;; return fasl filename
				 compiled-file-pathname)))
      ;; then asdf:
      ;; For SBCL, take advantage of it's REQUIRE/contrib directories integration
      #+sbcl
      (when (boundp 'sb-ext::*module-provider-functions*)
	(pushnew :sbcl-hooks-require cl:*features*))

      ;; return a list
      (prog1
	  (list 
	   ;; first ourselves:
	   (compile-and-load  "common-lisp-controller"
			      "common-lisp-controller.lisp")
	   ;; asdf
	   (compile-and-load  "asdf" "asdf.lisp")
	   (compile-and-load  "asdf" "wild-modules.lisp")
	   ;; now patch it::
	   (compile-and-load "common-lisp-controller"
			     "post-sysdef-install.lisp"))
	#+sbcl
	(setq cl:*features* (delete :sbcl-hooks-require  cl:*features*))))))

(defun init-common-lisp-controller-v4 (implementation-name)
  "configures common-lisp-controller. IMPLEMENTATION-NAME
is the name of this implementation.
Fasl's will be created in /var/cache/common-lisp-controller/<userid>/<implementation>"
  (compile-common-lisp-controller-v5 implementation-name)
  ;; no need to load them as they are already loaded
  (init-common-lisp-controller-v5 implementation-name))

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

