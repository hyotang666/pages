(in-package :cl-user)

(defpackage :pages
  (:use :cl :htmf)
  (:shadow compile)
  (:export))

(in-package :pages)

;;;; SPECIAL VARIABLES

(defvar *author*)

(defvar *compiler*)

(defvar *pattern*)

;;;; AUTHOR

(defun author ()
  (let ((namestring (car (last (pathname-directory (uiop:getcwd))))))
    (subseq namestring 0 (position #\. namestring))))

;;;; COMPILER
;;; MARKDOWN

(defun markdown (pathname)
  (lambda ()
    (let ((3bmd-code-blocks:*code-blocks* t)
          (3bmd-tables:*tables* t)
          (3bmd:*smart-quotes* t))
      (with-output-to-string (out)
        (3bmd:parse-and-print-to-stream pathname out)))))

;;; CSS

(defmacro with-output-to ((pathname) &body body)
  `(with-open-file (*standard-output* ,pathname :direction :output
                    :if-does-not-exist :create
                    :if-exists :supersede)
     (write-string ,@body)))

(defun compile-css ()
  (ensure-directories-exist "css/")
  (with-output-to ("css/css.css")
    (css-thunk)))

(defun css-thunk ()
  (cl-css:css
    `((h1 :padding 5% :border-bottom #:solid :text-align #:center)
      (h2 :padding 20px :background-color #:ghostwhite)
      (body :padding-left 7% :padding-right 7%)
      ;; Codes
      (pre :padding 10px :background-color #:whitesmoke)
      (code :color #:rebeccapurple)
      ;; Tables
      (table :background-color #:azure)
      ("tbody tr:nth-of-type(odd)" :background-color #:aqua)
      (,(format nil "~{~A~^,~}" '(td th)) :padding 10px)
      ;; archives
      (.archive :border #:solid :border-width #:thin :padding 10px
       :border-color #:gray)
      ;; footer
      (footer :border-top #:solid :border-width #:thin))))

(defun collect-file (directory pattern)
  (uiop:directory-files (merge-pathnames directory (uiop:getcwd)) pattern))

(defun date (time)
  (multiple-value-bind (s m h day month year)
      (decode-universal-time time)
    (declare (ignore s m h))
    (format nil "~D/~D/~D" year month day)))

(defun compile
       (
        &key ((:author *author*) (author)) ((:pattern *pattern*) "*.md")
        ((:compiler *compiler*) #'markdown) (css #'compile-css))
  (if (not (uiop:string-suffix-p (namestring (uiop:getcwd)) ".github.io/"))
      (warn "Current directory is not github.io repository.~S" (uiop:getcwd))
      (progn
       (unless (probe-file "css/css.css")
         (funcall css))
       (if (probe-file "index.html")
           (update)
           (with-output-to ("index.html")
             (template :title "index"))))))

(defun template
       (
        &key (title "") ((:author *author*) (author))
        (body (body () "Hello world."))
        (style-sheet (style-sheet "css/css.css")))
  (html5 nil
         (head ()
           (title () title)
           (meta :charset "UTF-8")
           (meta :name "auhtor" :content *author*)
           (meta :name "generator" :content "pages")
           style-sheet)
         body))

(defun update ()
  (let ((date (uiop:safe-file-write-date "index.html")))
    (if date
        (%update date)
        (error "No index.html in curret directory.~&~S" (uiop:getcwd)))))

(defun %update (date)
  (multiple-value-bind (targets ignored)
      (should-be-updated date)
    (when targets
      (%%update targets ignored))))

(defun should-be-updated (date)
  (mapc #'ensure-directories-exist '("src/" "archives/" "img/"))
  (flet ((sort-by-stamp (list)
           (sort list (complement #'uiop:timestamp<) :key #'file-write-date)))
    (loop :for pathname :in (collect-file "src/" *pattern*)
          :when (or (uiop:timestamp< date (uiop:safe-file-write-date pathname))
                    (not (probe-file (archives pathname))))
            :collect pathname :into targets
          :else
            :collect pathname :into ignored
          :finally (return
                    (values (sort-by-stamp targets) (sort-by-stamp ignored))))))

(defun archives (pathname)
  (make-pathname :type "html"
                 :directory (substitute "archives" "src"
                                        (pathname-directory pathname)
                                        :test #'equal)
                 :defaults pathname))

(defun %%update (targets ignored)
  (labels ((%compile (pathname)
             (with-output-to ((archives pathname))
               (compiler pathname))))
    (mapc #'%compile targets)
    (with-output-to ("index.html")
      (archives-updater targets ignored))))

(defun compiler (pathname)
  (template :title (pathname-name pathname)
            :style-sheet (style-sheet "../css/css.css")
            :body (body ()
                    (main () (funcall *compiler* pathname))
                    (footer () (a (list :href "../index.html") "Index")))))

(defun archives-updater (updated ignored)
  (template :title "Index" :body (archives-body updated ignored)))

(defun archives-body (updated ignored)
  (body ()
    (footer ()
      (nav ()
        (loop :for pathname :in (append updated ignored)
              :collect (p '(:class "index")
                         (a (list :href (enough-namestring (archives pathname)
                                                           (uiop:getcwd)))
                           (pathname-name pathname))
                         (date (file-write-date pathname))
                         (when (find pathname updated :test #'equal)
                           " updated!")))))))

(defun style-sheet (path) (link :rel "stylesheet" :href path :type "text/css"))
