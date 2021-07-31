; vim: ft=lisp et
(in-package :asdf)

(defsystem "pages"
  :version "1.2.14"
  :pathname "src"
  :depends-on
  (
   "markup-functions" ; HTML templates.
   "plump" ; HTML parser.
   "clss" ; CSS like selector for plump-dom.
   "3bmd" "3bmd-ext-code-blocks" "3bmd-ext-tables" ; Markdown parser.
   "cl-css" ; CSS compiler.
   )
  :components
  ((:file "pages")))
