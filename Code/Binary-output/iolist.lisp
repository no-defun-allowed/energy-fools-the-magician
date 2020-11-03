(in-package :jvm-binary-output)

;;; An iolist...IO list? We'll say io-list. An io-list is a fairly nice way of
;;; building up sequences in an immutable manner without appending.
;;; An io-list is either a sequence of io-lists, or a byte which we will write.

(defun write-io-list (iolist stream)
  "Write the elements of an IO list to a binary stream."
  (etypecase iolist
    (sequence (map 'nil (lambda (element)
                          (write-io-list element stream))
                   iolist))
    (integer (write-byte iolist stream))))

(defun io-list-length (iolist)
  "The number of elements in an IO list."
  (etypecase iolist
    (string (length iolist))
    (sequence (reduce #'+ iolist :key #'io-list-length))
    (integer 1)))
