(in-package :jvm-opcodes)

(defun render-constant (c)
  (%render-constant (first c) (rest c)))
(defgeneric %render-constant (name parts))

(defmethod render-value-of-type (c (type-name (eql 'constant-info)) arguments)
  (declare (ignore arguments))
  (render-constant c))

(defmacro define-constant-type (name tag &rest arguments)
  (let ((argument-names (mapcar #'first  arguments))
        (type-names     (mapcar #'second arguments)))
    `(progn
       (defun ,name ,argument-names
         (list ',name ,@argument-names))
       (defmethod %render-constant ((name (eql ',name)) parts)
         (destructuring-bind ,argument-names parts
           (list ,tag
                 ,@(loop for argument-name in argument-names
                         for type-name in type-names
                         collect `(render-value-of-type ,argument-name
                                                        ',type-name))))))))

(define-constant-type constant-utf8    1 (data constant-string))
(define-constant-type constant-integer 3 (value s4))
(define-constant-type constant-float   4 (value float))
(define-constant-type constant-long    5 (value s8))
(define-constant-type constant-double  6 (value double))
(define-constant-type constant-class   7 (name constant))
(define-constant-type constant-string 8 (value string)) 
(define-constant-type constant-field   9 (class constant) (name-and-type constant))
(define-constant-type constant-method  10 (class constant) (name-and-type constant))
(define-constant-type constant-interface-method 11 (class constant) (name-and-type constant))
(define-constant-type constant-name-and-type 12 (name constant) (descriptor constant))
(define-constant-type constant-method-handle 15 (kind u1) (index constant))
(define-constant-type constant-method-type   16 (descriptor constant))
