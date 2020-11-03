(defpackage :jvm-binary-output
  (:use :cl)
  (:export #:type-length #:render-value-of-type
           #:define-type #:define-enum
           #:define-record #:define-record*
           #:write-io-list #:io-list-length
           #:signed-byte #:byte #:signed-short #:short
           #:signed-int #:int #:signed-long #:long))
