(in-package :asdf)

(defsystem creole
  :name "creole"
  :version "0.0.1"
  :author "Takeru Ohta"
  :description "Convert between multibyte charset and unicode string"

  :serial t
  :components ((:file "package")
	       (:file "util")
	       (:file "trie")
	       (:file "external-format")
	       (:file "utf8")
	       (:file "utf16")
	       (:file "string-to-octets")
	       (:file "octets-to-string")))