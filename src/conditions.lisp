(in-package #:decision-protocol)

(define-condition decision-error (error)
  ((message :initarg :message :reader decision-error-message :initform nil)
   (request :initarg :request :reader decision-error-request :initform nil)
   (question :initarg :question :reader decision-error-question :initform nil))
  (:report (lambda (c s)
             (format s "decision error~@[: ~a~]" (decision-error-message c)))))

(define-condition decision-schema-error (decision-error) ()
  (:report (lambda (c s)
             (format s "decision schema error~@[: ~a~]"
                     (decision-error-message c)))))

(define-condition decision-unavailable (decision-error) ()
  (:report (lambda (c s)
             (format s "decision backend unavailable~@[: ~a~]"
                     (decision-error-message c)))))

(define-condition decision-timeout (decision-error)
  ((limit :initarg :limit :reader decision-timeout-limit :initform nil))
  (:report (lambda (c s)
             (format s "decision timed out~@[ (~a)~]~@[: ~a~]"
                     (decision-timeout-limit c)
                     (decision-error-message c)))))

(define-condition decision-unsupported (decision-error)
  ((capability :initarg :capability :reader decision-unsupported-capability
               :initform nil))
  (:report (lambda (c s)
             (format s "decision unsupported~@[ (~a)~]~@[: ~a~]"
                     (decision-unsupported-capability c)
                     (decision-error-message c)))))

(defun skip-question (&optional condition)
  "Invoke the SKIP-QUESTION restart (omit one question, continue the batch)."
  (let ((r (find-restart 'skip-question condition)))
    (when r (invoke-restart r))))

(defun retry (&optional condition)
  "Invoke the RETRY restart at the decide boundary."
  (let ((r (find-restart 'retry condition)))
    (when r (invoke-restart r))))

(defun call-with-decision-restarts (thunk)
  "Establish RETRY / USE-VALUE around THUNK (decide boundary)."
  (tagbody
   :retry
     (return-from call-with-decision-restarts
       (restart-case (funcall thunk)
         (retry ()
           :report "Retry the decide operation"
           (go :retry))
         (use-value (value)
           :report "Use a supplied decision-result instead"
           :interactive (lambda ()
                          (format *query-io* "Decision result: ")
                          (force-output *query-io*)
                          (list (read *query-io*)))
           value)))))
