(defpackage :jvm-binary-output
  (:use :cl)
  (:export #:type-length #:render-value-of-type
           #:define-type #:define-record #:define-enum
           #:write-io-list
           #:signed-byte #:byte #:signed-short #:short
           #:signed-int #:int #:signed-long #:long))
