(defvar *catch-tags* '())

(defmacro catch (tag &body body)
  (let ((block-name (gensym "CATCH-BLOCK")))
    `(block ,block-name
       (let ((*catch-tags* (acons ',tag
                                  (lambda (value)
                                    (return-from ,block-name value))
                                  *catch-tags*)))

(define-condition no-catch-tag (control-error)
  ((tag :initarg :tag :reader no-catch-tag-tag)))

(defun throw (tag value)
  (let ((tag (assoc tag *catch-tags*)))
    (if (null tag)
        (error 'no-catch-tag :tag tag)
        (funcall (cdr tag) value))))
