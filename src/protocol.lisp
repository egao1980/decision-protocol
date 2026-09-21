(in-package #:decision-protocol)

;;; Typed batched probabilities. Full mass is retained; never label-only.
;;; Question ids are caller metadata — they are not model input.
;;; CONCENTRATION is a Jev/Kev display helper, not P(correct).

(defclass decision-backend () ())

(defun decision-backend-p (object)
  (typep object 'decision-backend))

(defvar *decision-backend* nil
  "Current DECISION-BACKEND, or NIL.")

(defclass decision-question ()
  ((id :initarg :id :reader question-id)
   (instructions :initarg :instructions :reader question-instructions
                 :initform "")
   (criteria :initarg :criteria :reader question-criteria :initform nil)))

(defun decision-question-p (object)
  (typep object 'decision-question))

(defclass binary-question (decision-question) ()
  (:documentation
   "Yes-probability (wire :noul / :binary). CRITERIA may hold true/false text."))

(defclass choice-question (decision-question) ()
  (:documentation
   "Named options, 1..255. CRITERIA is an alist of option → description."))

(defclass ordinal-question (decision-question) ()
  (:documentation
   "Ordered levels, 2..255 (wire :score / :ordinal). CRITERIA is level strings."))

(defun binary-question-p (object)
  (typep object 'binary-question))

(defun choice-question-p (object)
  (typep object 'choice-question))

(defun ordinal-question-p (object)
  (typep object 'ordinal-question))

(defun %schema-error (message &key request question)
  (error 'decision-schema-error
         :message message
         :request request
         :question question))

(defun %plist-p (object)
  (and (listp object)
       (evenp (length object))
       (loop for (k) on object by #'cddr
             always (keywordp k))))

(defun %option-pairs (criteria)
  "Return alist of (option . description) from an alist or plist."
  (cond
    ((null criteria) nil)
    ((and (consp criteria) (consp (car criteria)))
     (mapcar (lambda (p) (cons (car p) (cdr p))) criteria))
    ((%plist-p criteria)
     (loop for (k v) on criteria by #'cddr
           collect (cons k v)))
    (t
     (%schema-error "choice criteria must be an alist or plist of option→description"))))

(defun make-binary-question (&key id instructions criteria true-text false-text)
  (make-instance 'binary-question
                 :id id
                 :instructions (or instructions "")
                 :criteria (or criteria
                               (append (when true-text (list :true true-text))
                                       (when false-text (list :false false-text))))))

(defun make-choice-question (&key id instructions criteria)
  (let ((pairs (%option-pairs criteria)))
    (unless (<= 1 (length pairs) 255)
      (%schema-error
       (format nil "choice-question needs 1..255 options, got ~a"
               (length pairs))))
    (make-instance 'choice-question
                   :id id
                   :instructions (or instructions "")
                   :criteria pairs)))

(defun make-ordinal-question (&key id instructions criteria)
  (unless (listp criteria)
    (%schema-error "ordinal-question criteria must be a list of level strings"))
  (let ((levels (mapcar (lambda (x)
                          (etypecase x
                            (string x)
                            (symbol (string x))
                            (character (string x))))
                        criteria)))
    (unless (<= 2 (length levels) 255)
      (%schema-error
       (format nil "ordinal-question needs 2..255 levels, got ~a"
               (length levels))))
    (make-instance 'ordinal-question
                   :id id
                   :instructions (or instructions "")
                   :criteria levels)))

(defgeneric question-wire-type (question)
  (:documentation "System One wire name: :noul, :choice, or :score."))

(defmethod question-wire-type ((question binary-question))
  :noul)

(defmethod question-wire-type ((question choice-question))
  :choice)

(defmethod question-wire-type ((question ordinal-question))
  :score)

(defmethod question-wire-type (question)
  (%schema-error (format nil "missing question type: ~s" question)
                 :question question))

(defgeneric question-options (question)
  (:documentation "Option keys for QUESTION (mass keys)."))

(defmethod question-options ((question binary-question))
  (let ((pairs (%option-pairs (question-criteria question))))
    (if pairs
        (mapcar #'car pairs)
        '(:true :false))))

(defmethod question-options ((question choice-question))
  (mapcar #'car (%option-pairs (question-criteria question))))

(defmethod question-options ((question ordinal-question))
  (copy-list (question-criteria question)))

(defun %plist-sans (plist &rest keys)
  (loop for (k v) on plist by #'cddr
        unless (member k keys)
          append (list k v)))

(defun coerce-question (object)
  "Accept a DECISION-QUESTION or a plist with :TYPE (:noul/:binary/:choice/:ordinal/:score)."
  (etypecase object
    (decision-question object)
    (cons
     (unless (%plist-p object)
       (%schema-error "question must be a decision-question or keyword plist"))
     (let ((type (getf object :type)))
       (unless type
         (%schema-error "missing question :type"))
       (let ((args (%plist-sans object :type)))
         (case type
           ((:noul :binary) (apply #'make-binary-question args))
           ((:choice) (apply #'make-choice-question args))
           ((:ordinal :score) (apply #'make-ordinal-question args))
           (t (%schema-error (format nil "unknown question type: ~s" type)))))))))

(defclass decision-request ()
  ((state :initarg :state :reader decision-request-state :initform nil)
   (questions :initarg :questions :reader decision-request-questions
              :initform nil)
   (model :initarg :model :reader decision-request-model :initform nil)))

(defun decision-request-p (object)
  (typep object 'decision-request))

(defun make-decision-request (&key state questions model)
  (check-type questions list)
  (make-instance 'decision-request
                 :state state
                 :questions (mapcar #'coerce-question questions)
                 :model model))

(defun coerce-decision-request (object)
  (etypecase object
    (decision-request object)
    (cons
     (unless (%plist-p object)
       (%schema-error "request must be a decision-request or keyword plist"))
     (apply #'make-decision-request object))
    (null (make-decision-request))))

(defun %finite-real-p (x)
  (and (realp x)
       (= x x)
       (let ((y (ignore-errors (float x 1.0d0))))
         (and y
              (= y y)
              (< most-negative-double-float y most-positive-double-float)))))

(defun %finite-nonneg-p (x)
  (and (%finite-real-p x) (>= x 0)))

(defun %copy-mass (alist)
  (mapcar (lambda (p) (cons (car p) (cdr p))) alist))

(defun %mass-sum (alist)
  (reduce #'+ alist :key #'cdr :initial-value 0))

(defun normalize-mass (alist &key (epsilon 1e-6))
  "Validate and normalize ALIST so values sum to 1.
   Signals DECISION-SCHEMA-ERROR if empty, non-finite, or cannot normalize."
  (unless (and alist (consp alist))
    (%schema-error "empty mass"))
  (dolist (pair alist)
    (unless (and (consp pair) (%finite-nonneg-p (cdr pair)))
      (%schema-error
       (format nil "non-finite or invalid mass component: ~s" pair))))
  (let ((sum (%mass-sum alist)))
    (unless (and (%finite-real-p sum) (> sum 0))
      (%schema-error "cannot normalize mass"))
    (if (<= (abs (- sum 1)) epsilon)
        (%copy-mass alist)
        (mapcar (lambda (p) (cons (car p) (/ (cdr p) sum))) alist))))

(defun %winner (alist)
  (let ((best nil)
        (best-p nil))
    (dolist (p alist)
      (when (or (null best-p) (> (cdr p) best-p))
        (setf best (car p)
              best-p (cdr p))))
    best))

(defclass probability-distribution ()
  ((mass :initarg :mass :reader distribution-mass)
   (winner :initarg :winner :reader distribution-winner))
  (:documentation
   "Full probability mass (alist summing to 1±1e-6) plus WINNER.
    Never collapse this to a label-only result."))

(defun probability-distribution-p (object)
  (typep object 'probability-distribution))

(defun make-probability-distribution (&key mass winner (epsilon 1e-6))
  (unless mass
    (%schema-error "empty mass"))
  (dolist (pair mass)
    (unless (and (consp pair) (%finite-nonneg-p (cdr pair)))
      (%schema-error
       (format nil "non-finite or invalid mass component: ~s" pair))))
  (let ((sum (%mass-sum mass)))
    (unless (and (%finite-real-p sum) (<= (abs (- sum 1)) epsilon))
      (%schema-error
       (format nil "mass must sum to 1±~a, got ~a" epsilon sum))))
  (make-instance 'probability-distribution
                 :mass (%copy-mass mass)
                 :winner (or winner (%winner mass))))

(defun concentration (distribution)
  "Jev/Kev display helper: (pmax-1/k)/(1-1/k) for k>1, else 1.0.

   This is NOT P(correct). Do not store it as a correctness probability;
   policies must use the full mass (or an explicit expected-utility cut)."
  (check-type distribution probability-distribution)
  (let* ((mass (distribution-mass distribution))
         (k (length mass)))
    (if (<= k 1)
        1
        (let ((pmax (reduce #'max mass :key #'cdr :initial-value 0)))
          (/ (- pmax (/ 1 k))
             (- 1 (/ 1 k)))))))

(defclass decision-answer ()
  ((question :initarg :question :reader decision-answer-question)
   (distribution :initarg :distribution :reader decision-answer-distribution)
   (score :initarg :score :reader decision-answer-score :initform nil)
   (rationale :initarg :rationale :reader decision-answer-rationale
              :initform nil))
  (:documentation
   "Question plus full DISTRIBUTION. SCORE is a derived ordinal expectation
    when present. Do not store concentration as P(correct)."))

(defun decision-answer-p (object)
  (typep object 'decision-answer))

(defun %ordinal-expectation (question distribution)
  (when (ordinal-question-p question)
    (let ((levels (question-criteria question))
          (mass (distribution-mass distribution))
          (acc 0))
      (loop for level in levels
            for index from 0
            for cell = (assoc level mass :test #'equal)
            do (when cell
                 (incf acc (* index (cdr cell)))))
      acc)))

(defun make-decision-answer (&key question distribution score rationale)
  (when question (check-type question decision-question))
  (when distribution (check-type distribution probability-distribution))
  (make-instance 'decision-answer
                 :question question
                 :distribution distribution
                 :score (or score (%ordinal-expectation question distribution))
                 :rationale rationale))

(defclass decision-usage ()
  ((input-tokens :initarg :input-tokens :reader decision-usage-input-tokens
                 :initform 0)
   (output-tokens :initarg :output-tokens :reader decision-usage-output-tokens
                  :initform 0)))

(defun decision-usage-p (object)
  (typep object 'decision-usage))

(defun make-decision-usage (&key (input-tokens 0) (output-tokens 0))
  (make-instance 'decision-usage
                 :input-tokens input-tokens
                 :output-tokens output-tokens))

(defclass decision-result ()
  ((model :initarg :model :reader decision-result-model)
   (answers :initarg :answers :reader decision-result-answers :initform nil)
   (usage :initarg :usage :reader decision-result-usage :initform nil))
  (:documentation
   "Resolved MODEL version, full-mass ANSWERS, and USAGE."))

(defun decision-result-p (object)
  (typep object 'decision-result))

(defun make-decision-result (&key model answers usage)
  (make-instance 'decision-result
                 :model model
                 :answers (copy-list answers)
                 :usage (or usage (make-decision-usage))))

(defun %ensure-backend (&optional (backend *decision-backend*))
  (or backend
      (restart-case
          (error 'decision-unavailable
                 :message "*decision-backend* is nil — bind a backend")
        (use-value (supplied)
          :report "Use a supplied decision backend"
          :interactive (lambda ()
                         (format *query-io* "Decision backend: ")
                         (force-output *query-io*)
                         (list (read *query-io*)))
          supplied))))

(defgeneric resolve-model (backend model)
  (:documentation
   "Resolve MODEL alias to a concrete version recorded on DECISION-RESULT.
    Default returns MODEL unchanged (NIL stays NIL)."))

(defmethod resolve-model (backend model)
  (declare (ignore backend))
  model)

(defgeneric backend-supports-p (backend capability)
  (:documentation "T when BACKEND supports CAPABILITY (:batch :permute :separate)."))

(defmethod backend-supports-p (backend capability)
  (declare (ignore backend capability))
  nil)

(defun %answer-question (function question)
  "Call FUNCTION on QUESTION. Establishes SKIP-QUESTION for this item."
  (restart-case (funcall function question)
    (skip-question ()
      :report "Skip this question and continue the batch"
      nil)))

(defun map-decision-questions (questions function)
  "Apply FUNCTION to each question. FUNCTION → DECISION-ANSWER or NIL (skipped)."
  (let ((answers nil))
    (dolist (q questions)
      (let ((answer (%answer-question function q)))
        (when answer
          (push answer answers))))
    (nreverse answers)))

(defgeneric decide (backend request)
  (:documentation
   "One shared STATE, N isolated questions → DECISION-RESULT.
    Restarts at this boundary: RETRY, USE-VALUE, SKIP-QUESTION."))

(defmethod decide :around (backend request)
  (declare (ignore backend request))
  (call-with-decision-restarts (lambda () (call-next-method))))

(defmethod decide ((backend null) request)
  (decide (%ensure-backend backend) request))

(defmethod decide (backend request)
  (declare (ignore request))
  (error 'decision-unsupported
         :message (format nil "not a decision backend: ~s" backend)))

(defgeneric permute (backend request question-id &key n-perm seed)
  (:documentation
   "Option-order sweep for QUESTION-ID. → list of DECISION-ANSWER.
    Default signals DECISION-UNSUPPORTED unless the backend supports :permute."))

(defmethod permute (backend request question-id &key n-perm seed)
  (declare (ignore request question-id n-perm seed))
  (error 'decision-unsupported
         :capability :permute
         :message (format nil "backend ~s does not support :permute" backend)))

(defgeneric decide-separate (backend request)
  (:documentation
   "Answer each question in isolation (no sibling question text).
    Default splits REQUEST into singles when BACKEND supports :separate."))

(defmethod decide-separate (backend request)
  (unless (backend-supports-p backend :separate)
    (error 'decision-unsupported
           :capability :separate
           :message (format nil "backend ~s does not support :separate" backend)))
  (let* ((request (coerce-decision-request request))
         (model (resolve-model backend (decision-request-model request)))
         (answers
          (mapcan (lambda (q)
                    (copy-list
                     (decision-result-answers
                      (decide backend
                              (make-decision-request
                               :state (decision-request-state request)
                               :questions (list q)
                               :model (decision-request-model request))))))
                  (decision-request-questions request))))
    (make-decision-result
     :model model
     :answers answers
     :usage (make-decision-usage))))
