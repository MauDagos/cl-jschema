(in-package :cl-jschema/tests)


;;; Generic

(defvar *debug-tests* nil)


(defun call-cl-jschema-test (name body-fn)
  (declare (ignore name))
  (handler-bind ((error (lambda (e)
                          (when *debug-tests*
                            (invoke-debugger e)))))
    (let ((cl-jschema::*registry* (make-hash-table :test 'equal)))
      (funcall body-fn))))


(defmacro cl-jschema-test (name &body body)
  `(5am:test ,name
     (call-cl-jschema-test ,name (lambda () ,@body))))


(defmacro signals (condition-spec message &body body)
  (alexandria:with-gensyms (e body-fn)
    `(let ((,body-fn (lambda () ,@body)))
       (5am:signals ,condition-spec (funcall ,body-fn))
       (5am:is (equal ,message (handler-case
                                   (funcall ,body-fn)
                                 (,condition-spec (,e)
                                   (format nil "~a" ,e))))))))


;;; JSON Schema testing


(defun valid-value (json-schema &rest value-input)
  (dolist (value value-input)
    (5am:is-true (cl-jschema:validate json-schema value))))


(defun valid-json (json-schema &rest json-input)
  (apply 'valid-value json-schema (mapcar 'jzon:parse json-input)))


(defun invalid-value (json-schema value error-message)
  (signals cl-jschema:invalid-json error-message
    (if (alexandria:starts-with-subseq "JSON Schema validation found"
                                       error-message
                                       :test 'equal)
        (cl-jschema:validate json-schema value)
        (handler-case
            (cl-jschema:validate json-schema value)
          (cl-jschema:invalid-json (e)
            (let* ((errors (cl-jschema:invalid-json-errors e))
                   (count (length errors)))
              (if (= 1 count)
                  (error (first (cl-jschema:invalid-json-errors e)))
                  (5am:fail "Testing invalid JSON for one error, but got ~d.~2%~
                             Testing value ~s for message ~s"
                            count value error-message))))))))


(defun invalid-values (json-schema value-input)
  (dolist (input value-input)
    (destructuring-bind (value error-message) input
      (invalid-value json-schema value error-message))))


(defun invalid-json (json-schema json-string error-message)
  (invalid-value json-schema (jzon:parse json-string) error-message))


(defun invalid-jsons (json-schema json-input)
  (dolist (input json-input)
    (destructuring-bind (json error-message) input
      (invalid-json json-schema json error-message))))
