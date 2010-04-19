(in-package :creole)

;;;;;;;;;;;
;;; declaim
(declaim (inline external-format-filename external-format-key))

;;;;;;;;;;;;;;;;;;;;
;;; special variable
(defvar *external-formats*
  (loop FOR path IN (remove-if-not #'pathname-name 
				   (directory (merge-pathnames "name/*" *data-dir*)))
    COLLECT
      (with-open-file (in path)
        (read in))))

(defvar *default-external-format* :utf-8)

(defvar *external-format=>filename-map*
  (let ((map (make-hash-table :test #'eq)))
    (loop FOR path IN (remove-if-not #'pathname-name 
				     (directory (merge-pathnames "name/*" *data-dir*))) DO
      (with-open-file (in path)
        (dolist (external-format (read in))
          (setf (gethash external-format map) (intern (pathname-name path) :keyword)))))
    map))

;;;;;;;;;;;;;;;;;;;;;
;;; internal function
(defun external-format-filename (external-format)
  (assert #1=(gethash external-format *external-format=>filename-map*)
	  (external-format) "Undefined external-format ~S" external-format)
  (values (symbol-name #1#)))

(defun external-format-key (external-format)
  (values (gethash external-format *external-format=>filename-map*)))
