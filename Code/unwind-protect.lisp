(in-package :hir-to-jvm)

(defclass unwind-protect-instruction (cleavir-ir:instruction
                                      cleavir-ir:multiple-successors-mixin)
  ())

(defclass continue-unwinding-instruction (cleavir-ir:instruction
                                          cleavir-ir:no-successors-mixin)
  ())

(defmethod cleavir-ast-to-hir:compile-ast ((client jvm-client)
                                           (ast cleavir-ast:unwind-protect-ast)
                                           context)
  (let* ((dynenv-out  (cleavir-ir:make-lexical-location '#:unwind-protect-dynenv))
         (cleanup-ast (cleavir-ast:body-ast (cleavir-ast:cleanup-thunk-ast ast)))
         (values-location (cleavir-ir:make-lexical-location '#:values))
         (normal-cleanup-context
           (cleavir-ast-to-hir:clone-context
            context
            :results (list (cleavir-ir:make-lexical-location '#:nowhere))
            :successors (loop for succ in (cleavir-ast-to-hir:successors context)
                              collect (make-instance 'cleavir-ir:restore-values-instruction
                                                     :dynamic-environment-location values-location
                                                     :successor succ))))
         ;; We compile the "normal" path of unwind-protect as
         ;; unwind-protect -> [protected] -> save-values -> [cleanup] -> restore-values
         ;; and the unwinding path as
         ;;                -> save-values -> [cleanup] -> restore-values -> continue-unwinding
         (normal-cleanup (cleavir-ast-to-hir:compile-ast client cleanup-ast
                                                         normal-cleanup-context))
         (unwind-values-location (cleavir-ir:make-lexical-location '#:unwind-values))
         (unwind-context (cleavir-ast-to-hir:clone-context context
                          :results (list (cleavir-ir:make-lexical-location '#:nowhere))
                          :successors (list (make-instance 'cleavir-ir:restore-values-instruction
                                             :dynamic-environment-location unwind-values-location
                                             :successor (make-instance 'continue-unwinding-instruction)))))
         (unwind-cleanup (cleavir-ast-to-hir:compile-ast client cleanup-ast
                                                         unwind-context))
         (new-context (cleavir-ast-to-hir:clone-context context
                       :dynamic-environment-location dynenv-out
                       :successors (list (make-instance 'cleavir-ir:save-values-instruction
                                                        :output values-location
                                                        :successor normal-cleanup)))))
    (make-instance 'unwind-protect-instruction
     :successors (list (cleavir-ast-to-hir:compile-ast client
                        (cleavir-ast:protected-form-ast ast)
                        new-context)
                       (make-instance 'cleavir-ir:save-values-instruction
                                      :output unwind-values-location
                                      :successor unwind-cleanup)))))
