(in-package #:decision-protocol)

;;; Deterministic in-tree backend. Answers from a table keyed by question id,
;;; a handler function, or a keyword rule. Isolation: sibling instructions
;;; are never read when answering another question.

(defclass mock-decision-backend (decision-backend)
  ((answers :initarg :answers :accessor mock-decision-answers :initform nil)
   (rule :initarg :rule :accessor mock-decision-rule :initform :uniform)
   (handler :initarg :handler :accessor mock-decision-handler :initform nil)
   (fail-ids :initarg :fail-ids :accessor mock-decision-fail-ids :initform nil)
   (capabilities :initarg :capabilities :accessor mock-decision-capabilities
                 :initform '(:batch :permute :separate)))
  (:documentation
   "Fixture backend. ANSWERS is an alist/hash of question-id → mass.
    RULE is :uniform, :first, or :last when the table has no entry.
    HANDLER, when set, is (lambda (backend question) mass-or-answer).
    Question ids are lookup keys only — not treated as model text."))

(defun mock-decision-backend-p (object)
  (typep object 'mock-decision-backend))

(defun make-mock-decision-backend (&key answers (rule :uniform) handler
                                     fail-ids
                                     (capabilities '(:batch :permute :separate)))
  (make-instance 'mock-decision-backend
                 :answers answers
                 :rule rule
                 :handler handler
                 :fail-ids fail-ids
                 :capabilities capabilities))

(defun use-mock-decision-backend (&rest args &key &allow-other-keys)
  (setf *decision-backend* (apply #'make-mock-decision-backend args)))

(defun %model-key (model)
  (cond
    ((null model) "")
    ((stringp model) (string-downcase model))
    ((symbolp model) (string-downcase (symbol-name model)))
    (t (string-downcase (princ-to-string model)))))

(defmethod resolve-model ((backend mock-decision-backend) model)
  (declare (ignore backend))
  (let ((key (%model-key model)))
    (cond
      ((or (string= key "")
           (string= key "kev-latest")
           (string= key "kev"))
       "kev-4b")
      ((or (string= key "jev-latest")
           (string= key "jev"))
       "jev-1.13.0")
      ((stringp model) model)
      ((symbolp model) (string-downcase (symbol-name model)))
      (t key))))

(defmethod backend-supports-p ((backend mock-decision-backend) capability)
  (and (member capability (mock-decision-capabilities backend)) t))

(defun %answers-table (answers)
  (if (hash-table-p answers)
      answers
      (let ((table (make-hash-table :test 'equal)))
        (dolist (pair answers table)
          (setf (gethash (car pair) table) (cdr pair))))))

(defun %lookup-mass (backend question)
  (let* ((id (question-id question))
         (table (%answers-table (mock-decision-answers backend)))
         (found (or (gethash id table)
                    (gethash (if (symbolp id)
                                 (string-downcase (symbol-name id))
                                 id)
                             table))))
    (when found
      found)))

(defun %rule-mass (rule options)
  (unless options
    (%schema-error "cannot apply a mock rule to a question with no options"))
  (ecase rule
    ((:uniform)
     (let ((p (/ 1 (length options))))
       (mapcar (lambda (k) (cons k p)) options)))
    ((:first)
     (cons (cons (first options) 1)
           (mapcar (lambda (k) (cons k 0)) (rest options))))
    ((:last)
     (let ((last (car (last options))))
       (mapcar (lambda (k) (cons k (if (eql k last) 1 0))) options)))))

(defun %coerce-mass (object question)
  (cond
    ((probability-distribution-p object)
     (distribution-mass object))
    ((decision-answer-p object)
     (distribution-mass (decision-answer-distribution object)))
    ((and (consp object) (consp (car object)))
     object)
    ((%plist-p object)
     (loop for (k v) on object by #'cddr
           collect (cons k v)))
    (t
     (%schema-error (format nil "mock mass for ~s is not an alist" (question-id question))
                    :question question))))

(defun %mock-mass (backend question)
  (when (member (question-id question) (mock-decision-fail-ids backend)
                :test #'equal)
    (error 'decision-error
           :question question
           :message (format nil "mock failure for question ~s"
                            (question-id question))))
  (cond
    ((mock-decision-handler backend)
     (%coerce-mass (funcall (mock-decision-handler backend) backend question)
                   question))
    (t
     (let ((found (%lookup-mass backend question)))
       (if found
           (%coerce-mass found question)
           (%rule-mass (or (mock-decision-rule backend) :uniform)
                       (question-options question)))))))

(defun %mock-answer (backend question)
  ;; Isolation: only QUESTION is consulted. Sibling text is never read.
  (let* ((mass (normalize-mass (%mock-mass backend question)))
         (dist (make-probability-distribution :mass mass)))
    (make-decision-answer :question question :distribution dist)))

(defmethod decide ((backend mock-decision-backend) request)
  (let* ((request (coerce-decision-request request))
         (model (resolve-model backend (decision-request-model request)))
         (answers (map-decision-questions
                   (decision-request-questions request)
                   (lambda (q) (%mock-answer backend q)))))
    (make-decision-result
     :model model
     :answers answers
     :usage (make-decision-usage :input-tokens 0 :output-tokens 0))))

(defun %lcg (seed)
  (let ((state (logand (or seed 1) #x7fffffff)))
    (lambda (n)
      (setf state (logand (+ (* state 1103515245) 12345) #x7fffffff))
      (mod state (max 1 n)))))

(defun %shuffle (list rng)
  (let ((vec (coerce list 'vector)))
    (loop for i from (1- (length vec)) downto 1
          for j = (funcall rng (1+ i))
          do (rotatef (aref vec i) (aref vec j)))
    (coerce vec 'list)))

(defun %with-shuffled-criteria (question shuffled)
  (etypecase question
    (choice-question
     (make-choice-question
      :id (question-id question)
      :instructions (question-instructions question)
      :criteria shuffled))
    (ordinal-question
     (make-ordinal-question
      :id (question-id question)
      :instructions (question-instructions question)
      :criteria shuffled))
    (binary-question
     (make-binary-question
      :id (question-id question)
      :instructions (question-instructions question)
      :criteria shuffled))))

(defun %criteria-items (question)
  (etypecase question
    (choice-question (copy-list (question-criteria question)))
    (ordinal-question (copy-list (question-criteria question)))
    (binary-question
     (or (%option-pairs (question-criteria question))
         '((:true . "true") (:false . "false"))))))

(defmethod permute ((backend mock-decision-backend) request question-id
                    &key (n-perm 2) seed)
  (unless (backend-supports-p backend :permute)
    (error 'decision-unsupported
           :capability :permute
           :message "mock backend is not configured for :permute"))
  (let* ((request (coerce-decision-request request))
         (question (find question-id (decision-request-questions request)
                         :key #'question-id :test #'equal))
         (rng (%lcg seed)))
    (unless question
      (%schema-error (format nil "no question with id ~s" question-id)
                     :request request))
    (loop repeat (max 1 n-perm)
          collect
          (let* ((shuffled (%shuffle (%criteria-items question) rng))
                 (q* (%with-shuffled-criteria question shuffled))
                 (answer (%mock-answer backend q*)))
            ;; Name-keyed mass: option identity, not presentation order.
            (make-decision-answer
             :question q*
             :distribution (decision-answer-distribution answer))))))
