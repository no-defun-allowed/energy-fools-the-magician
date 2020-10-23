(in-package :jvm-opcodes)

(defclass instruction ()
  ()
  (:documentation "The base class of an instruction."))

(defgeneric instruction-length (instruction))
(defgeneric render-instruction (instruction))

(defmacro define-instruction (name number &rest arguments)
  `(progn
     (export ',name)
     (defclass ,name (instruction)
       ,(loop for (name type) in arguments
              collect `(,name :initarg ,name :reader ,name)))
     (defun ,name ,(mapcar #'first arguments)
       (make-instance ',name
                      ,@(loop for (name type) in arguments
                              appending `(',name ,name))))
     (defmethod instruction-length ((instruction ,name))
       (+ 1 ,@(loop for (name type) in arguments
                    collect `(type-length ',type '()))))
     (defmethod render-instruction ((instruction ,name))
       (append (list ,number)
               ,@(loop for (name type) in arguments
                       collect `(render-value-of-type (,name instruction) ',type '()))))))

(defmacro define-instructions (&body instructions)
  "Define multiple instructions, with syntax like DEFINE-INSTRUCTION.
A 'template' syntax is also supported, where names can be of the form
 (variable name start end base), which defines instructions like
 (DEFINE-INSTRUCTION <NAME with VARIABLE swapped for N> <BASE + N> ...)
for N from START to END."
  `(progn
     ,@(loop for (name . rest-of-instruction) in instructions
             if (listp name)
               collect (destructuring-bind (place template low high base) name
                         `(progn
                            ,@(loop for n from low to high
                                    for name = (intern (str:replace-all
                                                        (symbol-name place)
                                                        (write-to-string n
                                                                         :base 10)
                                                        (symbol-name template)))
                                    collect `(define-instruction ,name ,(+ n base)
                                               ,@rest-of-instruction))))
             else
               collect `(define-instruction ,name ,@rest-of-instruction))))

(define-type signed-byte 1 (x) (list (logand x #xFF)))
(define-type byte 1 (x) (list x))
(define-type short 2 (x)
  (list (ldb (byte 8 8) x)
        (ldb (byte 8 0) x)))
(define-type signed-short 2 (x)
  (list (ldb (byte 8 8) x)
        (ldb (byte 8 0) x)))
(define-type instruction 2 (i)
  (render-value-of-type (- (instruction-position i *compiler*)
                           *this-position*)
                        'signed-short))

(define-instructions
  (nop         #x00)
  (aconst/null #x01)
  ((@ iconst/@ -1 5 #x03))
  ((@ lconst/@  0 1 #x09))
  ((@ fconst/@  0 2 #x0b))
  ((@ dconst/@  0 1 #x0e))
  
  (constant-byte  #x10 (value byte))
  (constant-short #x11 (value short))
  ; (constant       #x12 (value constant))
  (constant      #x13 (value constant))
  (long-constant  #x14 (value constant))
  
  (int-local    #x15 (index byte))
  (long-local   #x16 (index byte))
  (float-local  #x17 (index byte))
  (double-local #x18 (index byte))
  (object-local #x19 (index byte))
  ((@ int-local/@    0 3 #x1a))
  ((@ long-local/@   0 3 #x1e))
  ((@ float-local/@  0 3 #x22))
  ((@ double-local/@ 0 3 #x26))
  ((@ object-local/@ 0 3 #x2a))

  (int-aref    #x2e)
  (long-aref   #x2f)
  (float-aref  #x30)
  (double-aref #x31)
  (object-aref #x32)
  (byte-aref   #x33)
  (char-aref   #x34)
  (short-aref  #x35)

  (int-set    #x36 (index byte))
  (long-set   #x37 (index byte)) 
  (float-set  #x38 (index byte))
  (double-set #x39 (index byte))
  (object-set #x3a (index byte))

  ((@ int-set/@    #x3b 0 3))
  ((@ load-set/@   #x3f 0 3))
  ((@ float-set/@  #x43 0 3))
  ((@ double-set/@ #x47 0 3))
  ((@ object-set/@ #x4b 0 3))
  
  (int-aset    #x4f)
  (long-aset   #x50)
  (float-aset  #x51)
  (double-aset #x52)
  (object-aset #x53)
  (byte-aset   #x54)
  (char-aset   #x55)
  (short-aset  #x56)

  (drop      #x57)
  (drop-long #x58)
  (dup       #x59)
  (dup-long  #x5c)
  (swap      #x5f)

  (integer-add #x60)
  (long-add    #x61)
  (float-add   #x62)
  (double-add  #x63)
  (integer-subtract #x64)
  (long-subtract    #x65)
  (float-subtract   #x66)
  (double-subtract  #x67)
  (integer-multiply #x68)
  (long-multiply    #x69)
  (float-multiply   #x6a)
  (double-multiply  #x6b)
  (integer-divide #x6c)
  (long-divide    #x6d)
  (float-divide   #x6e)
  (double-divide  #x6f)
  (integer-remainder #x70)
  (long-remainder    #x71)
  (float-remainder   #x72)
  (double-remainder  #x73)
  (integer-negate #x74)
  (long-negate    #x75)
  (float-negate   #x76)
  (double-negate  #x77)
  (integer-shift-left  #x78)
  (long-shift-left     #x79)
  (integer-shift-right #x7a)
  (long-shift-right    #x7b)
  (integer-logical-shift-right #x7c)
  (long-logical-shift-right    #x7d)
  
  (integer-and #x7e)
  (long-and    #x7f)
  (integer-or  #x80)
  (long-or     #x81)
  (integer-xor #x82)
  (long-xor    #x83)
  (integer-incf #x84 (index byte) (change signed-byte))

  (long-compare    #x94)
  (float-compare   #x95)                ; -1 on NaN
  (float-compare*  #x96)                ; 1 on NaN
  (double-compare  #x97)                ; -1 on NaN
  (double-compare* #x98)                ; 1 on NaN

  (if-=  #x99 (target instruction))
  (if-/= #x9a (target instruction))
  (if-<  #x9b (target instruction))
  (if->= #x9c (target instruction))
  (if->  #x9d (target instruction))
  (if-<= #x9e (target instruction))
  (if-integer-=  #x9f (target instruction))
  (if-integer-/= #xa0 (target instruction))
  (if-integer-<  #xa1 (target instruction))
  (if-integer->= #xa2 (target instruction))
  (if-integer->  #xa3 (target instruction))
  (if-integer-<= #xa4 (target instruction))
  (if-object-=   #xa5 (target instruction))
  (if-object-/=  #xa6 (target instruction))
  (goto #xa7 (target instruction))

  (integer-return #xac)
  (long-return    #xad)
  (float-return   #xae)
  (double-return  #xaf)
  (object-return  #xb0)
  (void-return    #xb1)

  (static-ref #xb2 (field constant))
  (static-set #xb3 (field constant))
  (field-ref  #xb4 (field constant))
  (field-set  #xb5 (field constant))
  (invoke-virtual   #xb6 (method constant))
  (invoke-special   #xb7 (method constant))
  (invoke-static    #xb8 (method constant))

  (new       #xbb (index short))
  (new-array #xbc (element-type byte))
  (new-object-array #xbd (index short))
  (array-length #xbe)
  (throw-exception #xbf)
  (check-cast #xc0 (index short))
  (instance-of #xc1 (index short))

  (if-null #xc6 (target instruction))
  (if-not-null #xc7 (target instruction)))
