; vim: ft=lisp et
(in-package :asdf)
(load-system "dynamic-package")
(defsystem "pages"
  :default-component-class dynamic-package:file
  :pathname "src"
  :depends-on
  ("cl-who" "3bmd" "3bmd-ext-code-blocks" "3bmd-ext-tables" "cl-css")
  :components
  ((:file "util")
   (:file "markdown")
   (:file "css" :depends-on ("util"))
   (:file "pages" :depends-on ("util" "markdown" "css"))))
