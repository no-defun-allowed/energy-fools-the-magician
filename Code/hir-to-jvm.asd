(asdf:defsystem :hir-to-jvm
  :depends-on (:concrete-syntax-tree :cleavir2-hir :cleavir2-cst-to-ast
               :sicl-ast-to-hir :sicl-boot :str :babel)
  :components ((:module "Binary-output"
                :serial t
                :components ((:file "package")
                             (:file "output-types")
                             (:file "define-record")
                             (:file "integer-types")
                             (:file "iolist")))
               (:module "Class-format"
                :serial t
                :components ((:file "package")
                             (:file "instructions")
                             (:file "constants")
                             (:file "methods")
                             (:file "attributes")
                             (:file "class-file")))
               (:file "package")
               (:file "make-hir")
               (:file "unwind-protect")
               (:file "basic-block")
               (:file "assemble")
               (:file "rebuild-constants")
               (:file "emit-instruction")
               (:file "emit-load")
               (:file "emit-save")
               (:file "build-class")))
