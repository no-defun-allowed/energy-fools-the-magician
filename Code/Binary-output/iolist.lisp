(in-package :jvm-binary-output)

(defun write-io-list (iolist stream)
  (etypecase iolist
    (sequence (map 'nil (lambda (element)
                          (write-io-list element stream))
                   iolist))
    (integer (write-byte iolist stream))))
