;;;; Offline batch demo — noul / choice / score, isolation, concentration ≠ P(correct).
;;;;   sbcl --load examples/batch.lisp
;;;;   (asdf:load-system "decision-protocol") (load "examples/batch.lisp") (decision-protocol/demo:run)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :decision-protocol)
    (require :asdf)
    (asdf:load-system "decision-protocol")))

(defpackage #:decision-protocol/demo
  (:use #:cl #:decision-protocol)
  (:export #:run))

(in-package #:decision-protocol/demo)

(defun run (&optional (stream *standard-output*))
  "Print a packed batch + packed-vs-separate check. Returns the DECISION-RESULT."
  (let* ((backend (make-mock-decision-backend
                   :answers '((:ok . ((:true . 9/10) (:false . 1/10)))
                              (:risk . ((:allow . 4/5) (:deny . 1/5)))
                              (:sev . (("low" . 1/10) ("mid" . 3/10) ("high" . 3/5))))))
         (questions (list (make-binary-question
                           :id :ok :instructions "Proceed?")
                          (make-choice-question
                           :id :risk
                           :instructions "Allow this effect?"
                           :criteria '((:allow . "proceed") (:deny . "stop")))
                          (make-ordinal-question
                           :id :sev
                           :instructions "How severe?"
                           :criteria '("low" "mid" "high"))))
         (req (make-decision-request
               :state "tenant=acme ticket=chargeback"
               :questions questions
               :model :kev-latest))
         (result (decide backend req))
         (separate (decide-separate backend req)))
    (format stream "~&; model ~s (alias :kev-latest)~%"
            (decision-result-model result))
    (dolist (answer (decision-result-answers result))
      (let* ((q (decision-answer-question answer))
             (dist (decision-answer-distribution answer))
             (mass (distribution-mass dist))
             (conc (concentration dist)))
        (format stream "~&; ~s ~s winner=~s mass=~s concentration=~s~%"
                (question-id q) (question-wire-type q)
                (distribution-winner dist) mass conc)
        (when (eq (question-id q) :risk)
          (assert (/= conc (cdr (assoc :allow mass)))))))
    (loop for a in (decision-result-answers result)
          for b in (decision-result-answers separate)
          do (assert (equal (distribution-mass (decision-answer-distribution a))
                            (distribution-mass (decision-answer-distribution b)))))
    (format stream "~&; isolation packed=separate~%")
    result))

#+sbcl
(when (and *load-truename*
           (equal (pathname-name *load-truename*) "batch")
           (find "examples/batch.lisp" sb-ext:*posix-argv* :test #'search))
  (run)
  (uiop:quit 0))
