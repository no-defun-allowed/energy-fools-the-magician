(asdf:defsystem :hir-to-jvm
  :depends-on (:concrete-syntax-tree :cleavir2-hir :cleavir2-cst-to-ast
               :sicl-ast-to-hir :sicl-boot :str)
  :components ((:file "package")
               (:file "make-hir")
               (:file "instructions")
               (:file "basic-block")
               (:file "assemble")
               (:file "emit-instruction")))
