(dynamic-package:control
  (:export #:with-output-to))
(defmacro with-output-to((pathname)&body body)
  `(WITH-OPEN-FILE(*STANDARD-OUTPUT* ,pathname
				     :DIRECTION :OUTPUT
				     :IF-DOES-NOT-EXIST :CREATE
				     :IF-EXISTS :SUPERSEDE)
     ,@body))

(dynamic-package:control
  (:export #:collect-file))
(defun collect-file(directory pattern)
  (uiop:directory-files (merge-pathnames directory (uiop:getcwd))
			pattern))

(dynamic-package:control
  (:export #:with-html-compiler))
(defmacro with-html-compiler(&body body)
  `(LAMBDA()
     (CL-WHO:WITH-HTML-OUTPUT(*STANDARD-OUTPUT* NIL :INDENT T)
       ,@body)
     (VALUES)))

(dynamic-package:control
  (:export #:date))
(defun date(time)
  (multiple-value-bind(s m h day month year)(decode-universal-time time)
    (declare(ignore s m h))
    (format nil "~D/~D/~D"year month day)))
