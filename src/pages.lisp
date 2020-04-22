(in-package :cl-user)

(defpackage :pages (:use :cl)
  (:shadow compile)
  (:export))

(in-package :pages)

(defmacro with-output-to((pathname)&body body)
  `(WITH-OPEN-FILE(*STANDARD-OUTPUT* ,pathname
				     :DIRECTION :OUTPUT
				     :IF-DOES-NOT-EXIST :CREATE
				     :IF-EXISTS :SUPERSEDE)
     ,@body))

(defun collect-file(directory pattern)
  (uiop:directory-files (merge-pathnames directory (uiop:getcwd))
			pattern))

(defmacro with-html-compiler(&body body)
  `(LAMBDA()
     (CL-WHO:WITH-HTML-OUTPUT(*STANDARD-OUTPUT* NIL :INDENT T)
       ,@body)
     (VALUES)))

(defun date(time)
  (multiple-value-bind(s m h day month year)(decode-universal-time time)
    (declare(ignore s m h))
    (format nil "~D/~D/~D"year month day)))

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

(defun markdown(pathname)
  (lambda()
    (let((3bmd-code-blocks:*code-blocks* T)
	 (3bmd-tables:*tables* T)
	 (3bmd:*smart-quotes* T))
      (3bmd:parse-and-print-to-stream pathname *standard-output*))))

(defvar *author*)

(defun author()
  (let((namestring(car(last(pathname-directory(uiop:getcwd))))))
    (subseq namestring 0 (position #\. namestring))))

(defvar *compiler*)
(defvar *pattern*)

(defun compile(&key ((:author *author*)(author))
		    ((:pattern *pattern*)"*.md")
		    ((:compiler *compiler*)#'Markdown)
		    (css #'Compile-css))
  (if(not(uiop:string-suffix-p (namestring(uiop:getcwd))".github.io/"))
    (warn "Current directory is not github.io repository.~S"(uiop:getcwd))
    (progn (unless(probe-file "css/css.css")
	     (funcall css))
	   (if(probe-file "index.html")
	     (update)
	     (With-output-to("index.html")
	       (funcall (html :title "index")))))))

(defun html(&key (title "")
		 ((:author *author*)(author))
		 (body #'initial-body)
		 (style-sheet(style-sheet "css/css.css")))
  (lambda()
    (cl-who:with-html-output(*standard-output* nil :prologue t :indent t)
      (:html
	(:head(:title (princ title))
	  (:meta :http-equiv "content-type" :content "text/html; charset=UTF-8")
	  (:meta :name "auhtor" :content *author*)
	  (:meta :name "generator" :content "pages")
	  (funcall style-sheet)
	  )
	(funcall body)))
    (values)))

(defun initial-body()
  (With-html-compiler (:body "Hello world.")))

(defun footer(archives)
  (With-html-compiler (:footer((:a :href archives)"Index"))))

(defun body(&key(main (constantly ""))(footer (constantly "")))
  (With-html-compiler (:body (funcall main)
			     (funcall footer))))

(defun update()
  (let((date(uiop:safe-file-write-date "index.html")))
    (if date
      (%update date)
      (error "No index.html in curret directory.~&~S"(uiop:getcwd)))))

(defun %update(date)
  (multiple-value-bind(targets ignored)(should-be-updated date)
    (when targets
      (%%update targets ignored))))

(defun should-be-updated(date)
  (mapc #'ensure-directories-exist '("src/" "archives/" "img/"))
  (flet((SORT-BY-STAMP(list)
	  (sort list (complement #'uiop:timestamp<) :key #'file-write-date))
	)
    (loop :for pathname :in (Collect-file "src/" *pattern*)
	  :when (or (uiop:timestamp< date (uiop:safe-file-write-date pathname))
		    (not(probe-file(archives pathname))))
	  :collect pathname :into targets
	  :else :collect pathname :into ignored
	  :finally (return (values (SORT-BY-STAMP targets)
				   (SORT-BY-STAMP ignored))))))

(defun archives(pathname)
  (make-pathname :type "html"
		 :directory (substitute "archives" "src" (pathname-directory pathname)
					:test #'equal)
		 :defaults pathname))

(defun %%update(targets ignored)
  (labels((%COMPILE(pathname)
	    (With-output-to((archives pathname))
	      (funcall (compiler pathname))))
	  )
    (mapc #'%COMPILE targets)
    (With-output-to("index.html")
      (funcall(archives-updater targets ignored)))))

(defun compiler(pathname)
  (html :title (pathname-name pathname)
	:style-sheet(style-sheet "../css/css.css")
	:body (body :main (funcall *compiler* pathname)
		    :footer (footer "../index.html"))))

(defun archives-updater(updated ignored)
  (html :title "Index"
	:body(archives-body updated ignored)))

(defun archives-body(updated ignored)
  (With-html-compiler
    (:body
      (:footer
	(:nav (dolist(pathname (append updated ignored))
		(cl-who:htm ((:p :class "index")
			     ((:a :href (enough-namestring(archives pathname)(uiop:getcwd)))
			      (princ(pathname-name pathname)))
			     (princ (Date(file-write-date pathname)))
			     (when(find pathname updated :test #'equal)
			       (princ " updated!"))))))))))

(defun style-sheet(path)
  (With-html-compiler (:link :rel "stylesheet" :href path :type "text/css")))
