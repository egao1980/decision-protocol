(in-package #:decision-protocol/tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (asdf:system-relative-pathname "decision-protocol" "examples/batch.lisp")))

(deftest batch-demo-runs
  (let ((result (decision-protocol/demo:run (make-broadcast-stream))))
    (ok (decision-protocol:decision-result-p result))
    (ok (equal "kev-4b" (decision-protocol:decision-result-model result)))
    (ok (= 3 (length (decision-protocol:decision-result-answers result))))))
