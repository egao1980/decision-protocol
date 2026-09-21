(in-package #:decision-protocol/tests)

(defun %two-questions ()
  (list (decision-protocol:make-binary-question
         :id :keep :instructions "keep me")
        (decision-protocol:make-binary-question
         :id :boom :instructions "fail me")))

(deftest skip-question-signals
  (let ((b (decision-protocol:make-mock-decision-backend
            :answers '((:keep . ((:true . 1) (:false . 0))))
            :fail-ids '(:boom))))
    (ok (signals (decision-protocol:decide
                  b
                  (decision-protocol:make-decision-request
                   :questions (%two-questions)))
                 'decision-protocol:decision-error))))

(deftest skip-question-restart
  (let* ((b (decision-protocol:make-mock-decision-backend
             :answers '((:keep . ((:true . 4/5) (:false . 1/5))))
             :fail-ids '(:boom)))
         (result (handler-bind ((decision-protocol:decision-error
                                 (lambda (c)
                                   (decision-protocol:skip-question c))))
                   (decision-protocol:decide
                    b
                    (decision-protocol:make-decision-request
                     :questions (%two-questions)
                     :model :kev-latest)))))
    (ok (decision-protocol:decision-result-p result))
    (ok (equal "kev-4b" (decision-protocol:decision-result-model result)))
    (ok (= 1 (length (decision-protocol:decision-result-answers result))))
    (ok (eq :keep
            (decision-protocol:question-id
             (decision-protocol:decision-answer-question
              (first (decision-protocol:decision-result-answers result))))))
    (ok (= 4/5
           (cdr (assoc :true
                       (decision-protocol:distribution-mass
                        (decision-protocol:decision-answer-distribution
                         (first (decision-protocol:decision-result-answers result))))))))))

(deftest decide-use-value-restart
  (let* ((fallback (decision-protocol:make-decision-result
                    :model "kev-4b"
                    :answers nil))
         (b (decision-protocol:make-mock-decision-backend :fail-ids '(:boom)))
         (got (handler-bind ((decision-protocol:decision-error
                              (lambda (c)
                                (use-value fallback c))))
                (decision-protocol:decide
                 b
                 (decision-protocol:make-decision-request
                  :questions (list (decision-protocol:make-binary-question
                                    :id :boom)))))))
    (ok (eq fallback got))))

(deftest decide-retry-restart
  (let* ((n 0)
         (b (decision-protocol:make-mock-decision-backend
             :handler (lambda (backend question)
                        (declare (ignore backend question))
                        (incf n)
                        (if (< n 2)
                            (error 'decision-protocol:decision-unavailable
                                   :message "flaky")
                            '((:true . 1) (:false . 0))))))
         (result (handler-bind ((decision-protocol:decision-unavailable
                                 (lambda (c)
                                   (declare (ignore c))
                                   (invoke-restart 'decision-protocol:retry))))
                   (decision-protocol:decide
                    b
                    (decision-protocol:make-decision-request
                     :questions (list (decision-protocol:make-binary-question
                                       :id :q)))))))
    (ok (= 2 n))
    (ok (decision-protocol:decision-result-p result))))
