(in-package :hir-to-jvm)

(defun random-name ()
  (format nil "org/appliedlanguage/energyfoolsthemagician/CompiledFunction~d"
          (random most-positive-fixnum)))

(defconstant +self+                0)
(defconstant +arguments+           1)
(defconstant +dynamic-environment+ 2)
(defconstant +return-values+       3)
(defclass compiler-state ()
  ((name :initarg :name :initform (random-name) :reader name)
   (lexicals :initform (make-array 4
                        :adjustable t
                        :fill-pointer 4
                        :initial-contents '(#:self #:arguments #:dynamic-environment #:return-values))
             :reader lexicals)
   (constants :initform (make-array 0
                         :adjustable t
                         :fill-pointer 0)
              :reader constants)
   (basic-blocks :initform (make-hash-table)
                 :reader basic-blocks)
   (basic-block-positions :initform (make-hash-table)
                          :reader basic-block-positions)
   (basic-block-ordering :initform (make-array 0
                                               :adjustable t
                                               :fill-pointer 0)
                         :reader basic-block-ordering)
   (instruction-positions :initform (make-hash-table)
                          :reader instruction-positions)))

(defun lexical-count (compiler-state)
  (length (lexicals compiler-state)))

(defun %assemble-hir (initial-instruction compiler-state)
  (let ((work-list (list (make-instance 'basic-block
                          :initial-instruction initial-instruction))))
    (loop until (null work-list)
          do (let ((basic-block (pop work-list)))
               (vector-push-extend basic-block
                                   (basic-block-ordering compiler-state))
               (setf work-list
                     (append (assemble-basic-block basic-block compiler-state)
                             work-list))))
    compiler-state))

(defun assign-basic-block-positions (compiler-state)
  (loop with positions = (basic-block-positions compiler-state)
        for basic-block across (basic-block-ordering compiler-state)
        for position = 0 then (+ position (basic-block-length basic-block))
        do (setf (gethash basic-block positions)
                 position)))

(defun assign-instruction-positions (compiler-state)
  (loop with position = 0
        with positions-table = (instruction-positions compiler-state)
        for basic-block across (basic-block-ordering compiler-state)
        do (loop for instruction across (jvm-instructions basic-block)
                 do (setf (gethash instruction positions-table)
                          position)
                    (incf position (jvm-opcodes:instruction-length instruction)))))

(defun emit-bytes (compiler-state)
  (loop for basic-block across (basic-block-ordering compiler-state)
        appending (loop for instruction across (jvm-instructions basic-block)
                        appending (jvm-opcodes:render-instruction instruction))))

(defun assemble-hir (initial-instruction &rest initargs)
  (let ((compiler-state (apply #'make-instance 'compiler-state initargs)))
    (%assemble-hir initial-instruction compiler-state)
    (assign-basic-block-positions compiler-state)
    (assign-instruction-positions compiler-state)
    compiler-state))

(defun allocate-lexical (location compiler-state)
  (or (position location (lexicals compiler-state))
      (vector-push-extend location (lexicals compiler-state))))

(defun allocate-constant (value compiler-state)
  (let* ((pair     (list value nil)) 
         (position (vector-push-extend pair
                                       (constants compiler-state))))
    (setf (second pair)
          (constant-field-reference (name compiler-state) position))))
