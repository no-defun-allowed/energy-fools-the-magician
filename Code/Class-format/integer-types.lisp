(in-package :jvm-opcodes)

(macrolet ((define-integer-type (name type-specifier width)
             `(define-type ,name ,width (x)
                (check-type x ,type-specifier)
                (list ,@(loop for position from (1- width) downto 0
                              collect `(ldb (byte 8 ,(* 8 position)) x)))))
           (types (&body specifiers)
             `(progn
                ,@(loop for (signed unsigned width) in specifiers
                        for bits = (* width 8)
                        collect `(progn
                                   (define-integer-type ,signed (signed-byte ,bits) ,width)
                                   (define-integer-type ,unsigned (unsigned-byte ,bits) ,width))))))
  (types (signed-byte  byte  1)
         (signed-short short 2)
         (signed-int   int   4)
         (signed-long  long  8)))
