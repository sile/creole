(in-package :asdf)

(defsystem creole
  :name "creole"
  :version "0.0.3"
  :author "Takeru Ohta"
  :description "Convert between multibyte charset and unicode string"

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
