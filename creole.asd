(in-package :asdf)

(defsystem creole
  :name "creole"
  :version "0.1.0"
  :author "Takeru Ohta"
  :description "Convert between multibyte charset and unicode string"
  :depends-on (:charseq)
  
  :serial t
  :components ((:file "package")
	       (:file "util")
	       (:file "constant")
	       (:file "trie")
	       (:file "external-format")
	       (:file "utf16")
	       (:file "utf8")
	       (:file "string-to-octets")
	       (:file "octets-to-string")))