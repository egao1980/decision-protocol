(defpackage #:decision-protocol
  (:use #:cl)
  (:nicknames #:stack-decision)
  (:export #:decision-error
           #:decision-error-message
           #:decision-error-request
           #:decision-error-question
           #:decision-schema-error
           #:decision-unavailable
           #:decision-timeout
           #:decision-timeout-limit
           #:decision-unsupported
           #:decision-unsupported-capability
           #:skip-question
           #:retry

           #:decision-backend
           #:decision-backend-p
           #:*decision-backend*

           #:decision-question
           #:decision-question-p
           #:question-id
           #:question-instructions
           #:question-criteria
           #:question-wire-type
           #:question-options
           #:coerce-question

           #:binary-question
           #:binary-question-p
           #:make-binary-question

           #:choice-question
           #:choice-question-p
           #:make-choice-question

           #:ordinal-question
           #:ordinal-question-p
           #:make-ordinal-question

           #:decision-request
           #:decision-request-p
           #:make-decision-request
           #:coerce-decision-request
           #:decision-request-state
           #:decision-request-questions
           #:decision-request-model

           #:probability-distribution
           #:probability-distribution-p
           #:make-probability-distribution
           #:distribution-mass
           #:distribution-winner

           #:decision-answer
           #:decision-answer-p
           #:make-decision-answer
           #:decision-answer-question
           #:decision-answer-distribution
           #:decision-answer-score
           #:decision-answer-rationale

           #:decision-usage
           #:decision-usage-p
           #:make-decision-usage
           #:decision-usage-input-tokens
           #:decision-usage-output-tokens

           #:decision-result
           #:decision-result-p
           #:make-decision-result
           #:decision-result-model
           #:decision-result-answers
           #:decision-result-usage

           #:decide
           #:backend-supports-p
           #:permute
           #:decide-separate
           #:resolve-model
           #:concentration
           #:normalize-mass

           #:mock-decision-backend
           #:mock-decision-backend-p
           #:make-mock-decision-backend
           #:use-mock-decision-backend
           #:mock-decision-answers
           #:mock-decision-rule
           #:mock-decision-handler
           #:mock-decision-fail-ids))

(in-package #:decision-protocol)
