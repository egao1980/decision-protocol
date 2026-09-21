# decision-protocol

Lispy **CLOS** decision API for [cl-stack](https://github.com/egao1980/cl-stack) — typed batched probabilities (`noul` / `choice` / `score`). Full mass is retained; never a label-only result.

| System | Role | Repo |
|--------|------|------|
| `decision-protocol` (`stack-decision`) | Protocol / API + in-tree mock | this repo |

`concentration` is a Jev/Kev display helper `(pmax-1/k)/(1-1/k)` for `k>1` (else `1`). It is **not** P(correct).

```lisp
(asdf:load-system "decision-protocol")

(let* ((backend (stack-decision:make-mock-decision-backend
                 :answers '((:risk . ((:allow . 4/5) (:deny . 1/5))))))
       (req (stack-decision:make-decision-request
             :state "tenant=acme"
             :questions (list (stack-decision:make-choice-question
                               :id :risk
                               :instructions "Allow this effect?"
                               :criteria '((:allow . "proceed")
                                           (:deny . "stop"))))
             :model :kev-latest))
       (result (stack-decision:decide backend req)))
  (stack-decision:decision-result-model result)) ; "kev-4b"
```

Question ids are caller metadata, not model input. Isolation: changing sibling instructions must not change another question's mass (except via shared `state`). Alias models (`kev-latest`, `jev-latest`) resolve onto `decision-result-model`.

`decide` establishes `retry`, `use-value`, and per-question `skip-question`. `permute` / `decide-separate` are optional (`:permute` / `:separate`); the default methods signal `decision-unsupported`.

```lisp
(asdf:test-system "decision-protocol")
```

Offline demo (mock backend, isolation + concentration ≠ P(correct)):

```bash
sbcl --load examples/batch.lisp
```

## License

MIT
