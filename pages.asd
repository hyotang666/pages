; vim: ft=lisp et
(in-package :asdf)

(defsystem "pages"
  :version "1.2.17"
  :pathname "src"
  :author "SATO Shinichi"
  :license "MIT"
  :source-control (:git "git@github.com:hyotang666/pages")
  :bug-tracker "https://github.com/hyotang666/pages/issues"
  :description "Static site generator especially for github pages."
  :depends-on
  (
   "markup-functions" ; HTML templates.
   "plump" ; HTML parser.
   "clss" ; CSS like selector for plump-dom.
   "3bmd" "3bmd-ext-code-blocks" "3bmd-ext-tables" ; Markdown parser.
   "cl-css" ; CSS compiler.
   "cl-ppcre" ; Perl Compatible Regular Expressions.
   "uiop" ; Utilities.
   )
  :components
  ((:file "pages")))
