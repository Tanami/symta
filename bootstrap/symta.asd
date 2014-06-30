(defpackage :symta-system (:use :cl :asdf))

(in-package :symta-system)

;;(eval-when (:load-toplevel :execute)
;;  (asdf:operate 'asdf:load-op '#:cffi-grovel))

(defsystem :symta
  :name "symta"
  :description "Symta Lisp-system"
  :license "MIT"
  :author "snv"
  :version "0.3"
  :serial t
  :components ((:file "package")
               (:file "common")
               (:file "symta")
               ))
