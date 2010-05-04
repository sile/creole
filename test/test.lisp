;;; usage: (load "test")

(in-package :creole)

;; UTF-8, UTF-16le, UTF-16be以外のエンコード・デコードテスト(正常系)
(loop FOR (external-format) IN *external-formats* 
      UNLESS (member external-format '(:utf-16be :utf-16le :utf-8)) DO
  (format t "; === ~A ===~%" external-format)
  (let ((octets (coerce (loop FOR char-octets ACROSS (load-encode-table external-format)
			      APPEND (coerce char-octets 'list))
			'(vector octet))))
    (multiple-value-bind (s win) 
			 (octets-to-string octets :external-format external-format)
      (format t "; encode ~A string: ~:[FAILD !!!~;SUCCEEDED~]~%"
	      (length s)
	      (equalp octets (string-to-octets s :external-format external-format)))
      (format t "; decode ~A bytes: ~:[FAILD !!!~;SUCCEEDED~]~%" (length octets) win)))
  (terpri))

;; UTF-8, UTF-16le, UTF-16beのエンコード・デコードテスト(正常系)
(let ((unicode-charset (coerce 
			(loop FOR i FROM 0 BELOW char-code-limit 
			      UNLESS (<= #xD800 i #xDFFF)
			      #+CCL WHEN (code-char i)
			      COLLECT (code-char i))
			'string)))
  (loop FOR external-format IN '(:utf-16be :utf-16le :utf-8) DO
    (format t "; === ~A ===~%" external-format)
    (multiple-value-bind (octets win1) 
			 (string-to-octets unicode-charset :external-format external-format)
      (multiple-value-bind (s win2)
			   (octets-to-string octets :external-format external-format)
        (format t "; encode ~A string: ~:[FAILD !!!~;SUCCEEDED~]~%" 
		(length unicode-charset) win1)
	(format t "; decode ~A bytes: ~:[FAILD !!!~;SUCCEEDED~]~%" 
		(length octets) (and win2 (string= s unicode-charset)))))
    (terpri)))