(asdf:defsystem :hir-to-jvm
  :depends-on (:concrete-syntax-tree :cleavir2-hir :cleavir2-cst-to-ast
               :sicl-ast-to-hir :sicl-boot :str)
  :components ((:module "Binary-output"
                :serial t
                :components ((:file "package")
                             (:file "output-types")
                             (:file "define-record")))
               (:module "Class-format"
                :serial t
                :components ((:file "package")
                             (:file "instructions")
                             (:file "constants")))
               (:file "package")
               (:file "make-hir")
               (:file "unwind-protect")
               (:file "basic-block")
               (:file "assemble")
               (:file "emit-instruction")))
