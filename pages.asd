; vim: ft=lisp et
(in-package :asdf)

(defsystem "pages"
  :version "0.2.0"
  :pathname "src"
  :depends-on
  (
   "markup-functions" ; HTML templates.
   "3bmd" "3bmd-ext-code-blocks" "3bmd-ext-tables" "cl-css")
  :components
  ((:file "pages")))
