(in-package :hir-to-jvm)

(defclass jvm-client (sicl-boot::client)
  ())

(defvar *a-client* (make-instance 'jvm-client))
(defvar *an-environment* (sicl-boot:e6 (sicl-boot:boot)))

(defun expression-to-hir (expression environment)
  (let* ((client *a-client*)
         (cst (cst:cst-from-expression expression))
         (ast (let ((cleavir-cst-to-ast::*origin* nil))
                (handler-bind
                    ((trucler:no-function-description
                       (lambda (condition)
                         (declare (ignore condition))
                         (invoke-restart 'cleavir-cst-to-ast:consider-global)))
                     (trucler:no-variable-description
                       (lambda (condition)
                         (declare (ignore condition))
                         (invoke-restart 'cleavir-cst-to-ast:consider-special))))
                  (cleavir-cst-to-ast:cst-to-ast
                   client cst environment
                   :file-compilation-semantics t))))
         (hir (sicl-ast-to-hir:ast-to-hir client ast)))
    hir))
