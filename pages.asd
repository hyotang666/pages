; vim: ft=lisp et
(in-package :asdf)

(defsystem "pages"
  :version "0.1.3"
  :pathname "src"
  :depends-on
  ("cl-who" "3bmd" "3bmd-ext-code-blocks" "3bmd-ext-tables" "cl-css")
  :components
  ((:file "pages")))
