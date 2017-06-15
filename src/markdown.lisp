(dynamic-package:control
  (:export #:markdown))
(defun markdown(pathname)
  (lambda()
    (let((3bmd-code-blocks:*code-blocks* T)
	 (3bmd-tables:*tables* T)
	 (3bmd:*smart-quotes* T))
      (3bmd:parse-and-print-to-stream pathname *standard-output*))))

