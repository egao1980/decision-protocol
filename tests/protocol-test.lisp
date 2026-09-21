(in-package #:decision-protocol/tests)

(defun %mass (answer)
  (decision-protocol:distribution-mass
   (decision-protocol:decision-answer-distribution answer)))

(defun %p (mass key)
  (cdr (assoc key mass :test #'equal)))

(defun %nonfinite ()
  :not-a-number)

(deftest-parametrize schema-errors
    ((form)
     :ids ("missing-type" "empty-choice" "mass-not-summing" "mass-empty"
           "mass-non-finite" "mass-zero")
     ((lambda ()
        (decision-protocol:coerce-question
         '(:id :q :instructions "x"))))
     ((lambda ()
        (decision-protocol:make-choice-question :id :q :criteria nil)))
     ((lambda ()
        (decision-protocol:make-probability-distribution
         :mass '((:a . 1/2) (:b . 1/10)))))
     ((lambda ()
        (decision-protocol:normalize-mass nil)))
     ((lambda ()
        (decision-protocol:normalize-mass (list (cons :a (%nonfinite))))))
     ((lambda ()
        (decision-protocol:normalize-mass '((:a . 0) (:b . 0))))))
  (ok (signals (funcall form) 'decision-protocol:decision-schema-error)))

(deftest-parametrize concentration-vectors
    ((mass expected)
     :ids ("k1" "uniform-2" "sure-2" "skew-2" "uniform-3" "sure-3" "skew-3")
     ('((:a . 1)) 1)
     ('((:a . 1/2) (:b . 1/2)) 0)
     ('((:a . 1) (:b . 0)) 1)
     ('((:a . 7/10) (:b . 3/10)) 2/5)
     ('((:a . 1/3) (:b . 1/3) (:c . 1/3)) 0)
     ('((:a . 1) (:b . 0) (:c . 0)) 1)
     ('((:a . 7/10) (:b . 1/5) (:c . 1/10)) 11/20))
  (let* ((dist (decision-protocol:make-probability-distribution :mass mass))
         (got (decision-protocol:concentration dist)))
    (ok (= expected got))))

(deftest concentration-is-not-p-correct
  (let ((dist (decision-protocol:make-probability-distribution
               :mass '((:allow . 9/10) (:deny . 1/10)))))
    (ok (= 4/5 (decision-protocol:concentration dist)))
    (ng (= (decision-protocol:concentration dist)
           (cdr (assoc :allow (decision-protocol:distribution-mass dist)))))))

(deftest-parametrize alias-resolution
    ((alias expected)
     :ids ("kev-latest" "jev-latest" "kev-keyword" "concrete")
     (:kev-latest "kev-4b")
     ("jev-latest" "jev-1.13.0")
     (:jev-latest "jev-1.13.0")
     ("kev-4b" "kev-4b"))
  (let* ((q (decision-protocol:make-binary-question
             :id :ok :instructions "yes?"))
         (b (decision-protocol:make-mock-decision-backend
             :answers '((:ok . ((:true . 1) (:false . 0))))))
         (result (decision-protocol:decide
                  b
                  (decision-protocol:make-decision-request
                   :questions (list q)
                   :model alias))))
    (ok (equal expected (decision-protocol:decision-result-model result)))
    (ok (= 1 (length (decision-protocol:decision-result-answers result))))
    (ok (decision-protocol:probability-distribution-p
         (decision-protocol:decision-answer-distribution
          (first (decision-protocol:decision-result-answers result)))))))

(deftest resolve-model-direct
  (let ((b (decision-protocol:make-mock-decision-backend)))
    (ok (equal "kev-4b" (decision-protocol:resolve-model b :kev-latest)))
    (ok (equal "jev-1.13.0" (decision-protocol:resolve-model b :jev-latest)))))

(deftest full-mass-retained
  (let* ((q (decision-protocol:make-choice-question
             :id :pick
             :instructions "pick"
             :criteria '((:a . "A") (:b . "B") (:c . "C"))))
         (b (decision-protocol:make-mock-decision-backend
             :answers '((:pick . ((:a . 1/2) (:b . 1/3) (:c . 1/6))))))
         (result (decision-protocol:decide
                  b (decision-protocol:make-decision-request
                     :questions (list q))))
         (answer (first (decision-protocol:decision-result-answers result)))
         (mass (%mass answer)))
    (ok (= 3 (length mass)))
    (ok (= 1/2 (%p mass :a)))
    (ok (= 1/3 (%p mass :b)))
    (ok (= 1/6 (%p mass :c)))
    (ok (eq :a (decision-protocol:distribution-winner
                (decision-protocol:decision-answer-distribution answer))))))

(deftest mock-supports-batch
  (let ((b (decision-protocol:make-mock-decision-backend)))
    (ok (decision-protocol:backend-supports-p b :batch))
    (ok (decision-protocol:backend-supports-p b :permute))
    (ok (decision-protocol:backend-supports-p b :separate))))

(deftest permute-keeps-name-keyed-mass
  (let* ((q (decision-protocol:make-choice-question
             :id :pick
             :instructions "pick"
             :criteria '((:a . "A") (:b . "B") (:c . "C"))))
         (b (decision-protocol:make-mock-decision-backend
             :answers '((:pick . ((:a . 1/2) (:b . 1/3) (:c . 1/6))))))
         (req (decision-protocol:make-decision-request :questions (list q)))
         (sweeps (decision-protocol:permute b req :pick :n-perm 4 :seed 7)))
    (ok (= 4 (length sweeps)))
    (dolist (answer sweeps)
      (let ((mass (%mass answer)))
        (ok (= 1/2 (%p mass :a)))
        (ok (= 1/3 (%p mass :b)))
        (ok (= 1/6 (%p mass :c)))))))

(deftest permute-default-unsupported
  (ok (signals (decision-protocol:permute
                (make-instance 'decision-protocol:decision-backend)
                (decision-protocol:make-decision-request
                 :questions (list (decision-protocol:make-binary-question
                                   :id :q)))
                :q)
               'decision-protocol:decision-unsupported)))

(deftest coerce-wire-types
  (let ((noul (decision-protocol:coerce-question
               '(:type :noul :id :b :instructions "yes?")))
        (choice (decision-protocol:coerce-question
                 '(:type :choice :id :c :criteria (:a "A" :b "B"))))
        (score (decision-protocol:coerce-question
                '(:type :score :id :s :criteria ("low" "high")))))
    (ok (decision-protocol:binary-question-p noul))
    (ok (eq :noul (decision-protocol:question-wire-type noul)))
    (ok (decision-protocol:choice-question-p choice))
    (ok (eq :choice (decision-protocol:question-wire-type choice)))
    (ok (decision-protocol:ordinal-question-p score))
    (ok (eq :score (decision-protocol:question-wire-type score)))))

(deftest use-mock-decision-backend-binds-star
  (let ((decision-protocol:*decision-backend* nil)
        (b nil))
    (setf b (decision-protocol:use-mock-decision-backend
             :answers '((:q . ((:true . 1) (:false . 0))))))
    (ok (decision-protocol:mock-decision-backend-p b))
    (ok (eq b decision-protocol:*decision-backend*))
    (let ((result (decision-protocol:decide
                   b
                   (decision-protocol:make-decision-request
                    :questions (list (decision-protocol:make-binary-question
                                      :id :q))
                    :model :kev-latest))))
      (ok (equal "kev-4b" (decision-protocol:decision-result-model result))))))
