; vim: ft=lisp et
(in-package :asdf)

(defsystem "pages"
  :version "1.3.1"
  :pathname "src"
  :author "SATO Shinichi"
  :license "MIT"
  :source-control (:git "git@github.com:hyotang666/pages")
  :bug-tracker "https://github.com/hyotang666/pages/issues"
  :description "Static site generator especially for github pages."
  :depends-on
  (
   "uiop"               ; Utilities, implicitly depends on via asdf.
   "3bmd" "3bmd-ext-code-blocks" "3bmd-ext-tables" ; Markdown parser.
   "markup-functions"   ; HTML templates.
   "plump"              ; HTML parser.
   "clss"               ; CSS like selector for plump-dom.
   "cl-css"             ; CSS compiler.
   "colorize"           ; Syntax highlighting, implicitly depends on via 3bmd-ext-code-blocks.
   "query-repl"         ; User query.
   )
  :components
  ((:file "pages")))
