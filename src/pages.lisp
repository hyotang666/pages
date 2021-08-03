(in-package :cl-user)

(defpackage :pages
  (:use :cl :htmf)
  (:shadow compile)
  (:export #:compile))

(in-package :pages)

(declaim (optimize speed))

;;;; SPECIAL VARIABLES

(declaim (type simple-string *author* *pattern*))

(defvar *author*)

(defvar *pattern*)

(declaim (type function *compiler*))

(defvar *compiler*)

(declaim (type (unsigned-byte 8) *max-contents*))

(defparameter *max-contents* 8)

;;;; AUTHOR

(defun author ()
  (let ((namestring (car (last (pathname-directory (uiop:getcwd))))))
    (declare (type simple-string namestring))
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

(defun css-thunk ()
  (concatenate 'string
               (cl-css:css
                 `((:h1 :padding-bottom #:1% :border-bottom #:solid
                    :font-family #:gothic)
                   (:h2 :padding-bottom #:0.5% :padding-top #:0.5%
                    :background-color #:ghostwhite :font-family #:gothic
                    :margin-top #:2%)
                   (:h3 :background-color #:ghostwhite :display #:table
                    :font-family #:gothic)
                   (:body :padding-left #:7% :padding-right #:7% :font-family
                    "\"Noto Sans CJK jp\", \"Liberation Mono\", monospace"
                    :font-size #:1rem :font-weight #:lighter :max-width #:64rem
                    :margin #:auto :background-image
                    "url('../img/lisplogo_alien.svg')" :background-repeat
                    #:no-repeat :background-position #:256rem)
                   ;; Codes
                   (:pre :padding #:10px :background-color #:whitesmoke
                    :overflow #:auto)
                   (:code :color #:rebeccapurple)
                   ;; Tables
                   (:table :background-color #:azure)
                   ("tbody tr:nth-of-type(odd)" :background-color #:aqua)
                   (,(format nil "~{~A~^,~}" '(td th)) :padding #:10px)
                   ;; archives
                   (:.archive :border #:solid :border-width #:thin :padding
                    #:10px :border-color #:gray)
                   ;; footer
                   (:footer :border-top #:solid :border-width #:thin)
                   (:ul :padding-left #:1.5rem)))
               (ppcre:regex-replace "FFBAFF"
                                    (ppcre:regex-replace "CAFFCA"
                                                         (ppcre:regex-replace
                                                           "CACAFF"
                                                           (ppcre:regex-replace
                                                             "FFFFBA"
                                                             (ppcre:regex-replace
                                                               "FFCACA"
                                                               (ppcre:regex-replace
                                                                 "BAFFFF"
                                                                 colorize:*coloring-css*
                                                                 "EEFFFF")
                                                               "FFEEFF")
                                                             "FFFFEE")
                                                           "EAEAFF")
                                                         "EAFFEA")
                                    "FFDAFF")))

(defmacro with-output-to ((pathname) &body body)
  `(with-open-file (*standard-output* ,pathname :direction :output
                    :if-does-not-exist :create
                    :if-exists :supersede)
     (write-string ,@body)))

(defun compile-css ()
  (ensure-directories-exist "css/")
  (with-output-to ("css/css.css")
    (css-thunk)))

;;;; TEMPLATE

(defun style-sheet (path)
  (link `(:rel "stylesheet" :href ,path :type "text/css")))

(defun template
       (&key (title "") ((:author *author*) (author))
        (body (body () "Hello world."))
        (style-sheet (style-sheet "../css/css.css")))
  (html5 ()
    (head ()
      (title () title)
      (meta '(:charset "UTF-8"))
      (meta `(:name "auhtor" :content ,*author*))
      (meta '(:name "generator" :content "pages"))
      style-sheet)
    body))

;;;; UPDATE
;;; COMPILER

(defun compiler (pathname)
  (template :title (pathname-name pathname)
            :style-sheet (style-sheet "../css/css.css")
            :body (body ()
                    (main () (funcall *compiler* pathname))
                    (footer ()
                      (a (list :href "../indexes/index.html") "Index")))))

;;; %UPDATE
;; SHOULD-BE-UPDATED

(defun collect-file (directory pattern)
  (uiop:directory-files (merge-pathnames directory (uiop:getcwd)) pattern))

(defun date (time)
  (multiple-value-bind (s m h day month year)
      (decode-universal-time time)
    (declare (ignore s m h))
    (format nil "~D/~D/~D" year month day)))

(defun archives (pathname)
  (make-pathname :type "html"
                 :directory (substitute "archives" "src"
                                        (pathname-directory pathname)
                                        :test #'equal)
                 :defaults pathname))

(declaim (ftype (function (list) (values list &optional)) sort-by-stamp))

(defun sort-by-stamp (list)
  (sort list (lambda (a b) (not (uiop:timestamp< a b))) :key #'file-write-date))

(defun should-be-updated (date)
  (mapc #'ensure-directories-exist '("src/" "archives/" "img/" "indexes/"))
  (loop :for pathname :in (collect-file "src/" *pattern*)
        :when (or (uiop:timestamp< date (uiop:safe-file-write-date pathname))
                  (not (probe-file (archives pathname))))
          :collect pathname :into targets
        :else
          :collect pathname :into ignored
        :finally (return
                  (values (sort-by-stamp targets) (sort-by-stamp ignored)))))

;; ARCHIVES-UPDATER

(declaim
 (ftype (function ((unsigned-byte 8) (unsigned-byte 8))
         (values (or null function) &optional))
        page-nav))

(defun page-nav (count page)
  (flet ((list-item (page label)
           (a (list :href (format nil "index~:[~D~;~].html" (zerop page) page)
                    :class "page-link")
             label))
         (max-page (page)
           (min 5
                (multiple-value-bind (num rem)
                    (floor (- count (* *max-contents* page)) *max-contents*)
                  (if (zerop rem)
                      (1- num)
                      num)))))
    (unless (<= count *max-contents*)
      (footer ()
        (nav '(:aria-label "Pagination.")
          (loop :for i :of-type (mod #.most-positive-fixnum) :upfrom (1+ page)
                :repeat (max-page page)
                :collect (list-item i i) :into as
                :finally (return
                          (if (zerop page)
                              as
                              (cons (list-item (1- page) "<") as)))))))))

(defun archives-updater (contents count page)
  (template :title "Index"
            :body (body () (ul () contents) (page-nav count page))))

;; INDEX-LINK

(declaim
 (ftype (function (list (integer 3 #.most-positive-fixnum))
         (values list &optional))
        lines-truncate))

(defun lines-truncate (lines max)
  (loop :for line :of-type simple-string :in lines
        :for length := (length line)
        :for count :of-type (mod #.most-positive-fixnum) := length
             :then (+ count length)
        :if (= count max)
          :collect line
          :and :do (loop-finish)
        :else :if (< count max)
          :collect line
          :and :collect (br nil)
        :else :if (<= (- max (- count length)) 3)
          :collect "..."
          :and :do (loop-finish)
        :else
          :collect (let ((temp
                          (make-string (- max (- count length))
                                       :initial-element #\.)))
                     ;; due to not simple-base-string
                     (declare (optimize (speed 1)))
                     (replace temp line :end2 (- max (- count length) 3))
                     temp)
          :and :do (loop-finish)))

(defun index-link (pathname &optional updated)
  (let* ((dom
          (plump:parse (funcall (the function (funcall *compiler* pathname))))))
    (li ()
      (div ()
        (header ()
          (h2 ()
            (a (list :href (enough-namestring
                             (make-pathname :directory '(:relative :up
                                                         "archives")
                                            :defaults (archives pathname))))
              (plump:text (elt (clss:select "h1" dom) 0)))))
        (main ()
          (p ()
            (lines-truncate
              (locally
               ;; Due to plump return fill pointer array.
               (declare (optimize (speed 1)))
               (loop :for element :across (plump:children dom)
                     :if (not
                           (or (plump:comment-p element)
                               (and (plump:element-p element)
                                    (find (plump:tag-name element)
                                          '("h1" "h2" "h3" "ul" "ol")
                                          :test #'equal))
                               (and (plump:text-node-p element)
                                    (every #'ppcre::whitespacep
                                           (plump:text element)))))
                       :nconc (remove ""
                                      (uiop:split-string (plump:text element)
                                                         :separator #.(string
                                                                        #\Newline)))))
              64)))
        (footer ()
          (date (file-write-date pathname))
          (when updated
            " Updated!"))))))

(defun %%update (targets ignored &optional force)
  (labels ((%compile (pathname)
             (with-output-to ((archives pathname))
               (compiler pathname))))
    (mapc #'%compile targets)
    (when force
      (mapc #'%compile ignored))
    (loop :for number :of-type (integer 0 #.most-positive-fixnum) :upfrom 0
          :for contents
               :on (nconc
                     (mapcar (lambda (pathname) (index-link pathname t))
                             targets)
                     (mapcar #'index-link ignored))
               :by #'(lambda (list) (nthcdr *max-contents* list))
          :with count := (length contents)
          :do (with-output-to ((format nil "indexes/index~:[~D~;~].html"
                                       (zerop number) number))
                (archives-updater
                  (loop :for content :in contents
                        :repeat *max-contents*
                        :collect content)
                  count number)))))

(defun update (&optional force)
  (let ((date (uiop:safe-file-write-date "indexes/index.html")))
    (assert date () "No index.html in curret directory.~&~S" (uiop:getcwd))
    (multiple-value-bind (targets ignored)
        (should-be-updated date)
      (when (or targets force)
        (%%update targets ignored force)))))

;;;; COMPILE

(defun compile
       (&key ((:author *author*) (author)) ((:pattern *pattern*) "*.md")
        ((:compiler *compiler*) #'markdown) (css #'compile-css) force)
  (if (not (uiop:string-suffix-p (namestring (uiop:getcwd)) ".github.io/"))
      (warn "Current directory is not github.io repository.~S" (uiop:getcwd))
      (progn
       (when (or (not (probe-file "css/css.css")) force)
         (funcall (coerce css 'function)))
       (if (probe-file "indexes/index.html")
           (update force)
           (with-output-to ((ensure-directories-exist "indexes/index.html"))
             (template :title "index"))))))