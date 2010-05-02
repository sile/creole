(in-package :creole)

(defconstant +UNKNOWN-CHAR+ #\?)
(defconstant +UNKNOWN-CODE+ (char-code #\?))
(defconst-onceonly +UNKNOWN-OCTETS+ (coerce `(,(char-code #\?)) '(vector octet)))