(defvar *author*)

(defun author()
  (let((namestring(car(last(pathname-directory(uiop:getcwd))))))
    (subseq namestring 0 (position #\. namestring))))

(defvar *compiler*)
(defvar *pattern*)

(dynamic-package:control
  (:import-from :pages.markdown #:markdown)
  (:import-from :pages.css #:compile-css)
  (:import-from :pages.util #:with-output-to)
  (:shadow #:compile)
  (:export #:compile))
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

(dynamic-package:control
  (:import-from :pages.util #:with-html-compiler))
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

(dynamic-package:control
  (:import-from :pages.util #:collect-file))
(defun should-be-updated(date)
  (mapc #'ensure-directories-exist '("src/" "archives/" "img/"))
  (flet((SORT-BY-STAMP(list)
	  (sort list (complement #'uiop:stamp<) :key #'file-write-date))
	)
    (loop :for pathname :in (Collect-file "src/" *pattern*)
	  :when (or (uiop:stamp< date (uiop:safe-file-write-date pathname))
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

(dynamic-package:control
  (:shadowing-import-from :pages.util #:date))
(defun archives-body(updated ignored)
  (With-html-compiler
    (:body
      (:footer
	(:nav (dolist(pathname (append updated ignored))
		(cl-who:htm ((:p :class "index")
			     ((:a :href (enough-namestring(archives pathname)))
			      (princ(pathname-name pathname)))
			     (princ (Date(file-write-date pathname)))
			     (when(find pathname updated :test #'equal)
			       (princ " updated!"))))))))))

(defun style-sheet(path)
  (With-html-compiler (:link :rel "stylesheet" :href path :type "text/css")))
