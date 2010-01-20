(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
 (when (find-package :common-lisp-controller)
   ;; let's avoid loading this in the installation
   (load "/var/lib/common-lisp-controller/lisp-config.lisp")))
