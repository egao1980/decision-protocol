(in-package #:decision-protocol/tests)

(deftest batch-demo-runs
  (load (asdf:system-relative-pathname "decision-protocol" "examples/batch.lisp"))
  (let ((result (decision-protocol/demo:run (make-broadcast-stream))))
    (ok (decision-protocol:decision-result-p result))
    (ok (equal "kev-4b" (decision-protocol:decision-result-model result)))
    (ok (= 3 (length (decision-protocol:decision-result-answers result))))))
