(dynamic-package:control
  (:import-from :pages.util #:with-output-to)
  (:export #:compile-css))
(defun compile-css()
  (ensure-directories-exist "css/")
  (with-output-to("css/css.css")
    (funcall #'css-thunk)))

(defun css-thunk()
  (princ (cl-css:css `((h1 :padding 5% :border-bottom #:solid
			   :text-align #:center)
		       (h2 :padding 20px :background-color #:ghostwhite)
		       (body :padding-left 7% :padding-right 7%)
		       ;; Codes
		       (pre :padding 10px :background-color #:whitesmoke)
		       (code :color #:rebeccapurple)
		       ;; Tables
		       (table :background-color #:azure)
		       ("tbody tr:nth-of-type(odd)" :background-color #:aqua)
		       (,(format nil "~{~A~^,~}"'(td th))
			 :padding 10px)
		       ;; archives
		       (.archive :border #:solid :border-width #:thin
				 :padding 10px :border-color #:gray)
		       ;; footer
		       (footer :border-top #:solid :border-width #:thin)
		       )))
  (values))
