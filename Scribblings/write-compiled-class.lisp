(jvm-constants:with-constant-pool ()
  (let* ((file (make-class-file (assemble-hir (expression-to-hir '1 *an-environment*)
                                              :name "TestFunction")))
         (iolist (jvm-binary-output:render-value-of-type file 'jvm-class-format:class-file '())))
    (with-open-file (s "/tmp/TestFunction.class"
                       :direction :output
                       :element-type '(unsigned-byte 8))
      (jvm-binary-output:write-io-list iolist s))))

#|
$ java TestTestFunction
[Ljava.lang.Object;@2a139a55
[1]
|#
