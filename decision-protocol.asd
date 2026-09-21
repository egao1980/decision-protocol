(defsystem "decision-protocol"
  :version "0.1.0"
  :description "CLOS decision protocol for cl-stack (typed batched probabilities)"
  :author "egao1980"
  :license "MIT"
  :depends-on ()
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "protocol")
               (:file "mock"))
  :in-order-to ((test-op (test-op "decision-protocol/tests"))))

(defsystem "decision-protocol/tests"
  :depends-on ("decision-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "protocol-test")
               (:file "restarts-test")
               (:file "isolation-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
