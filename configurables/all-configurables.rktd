;; Paths to implementations are relative to this file
#hash(("mutation" . #hash(("type-mistakes-in-code" . "mutation/mutate-benchmark.rkt")
                          ("type-annotation-mistakes" . "mutation/mutate-types.rkt")))
      ("mutant-sampling" . #hash(("none" . "mutant-sampling/no-sampling.rkt")
                                 ("within-mutators" . "mutant-sampling/sample-within-mutators.rkt")))
      ("blame-following" . #hash(("natural-blame" . "blame-following/natural-blame.rkt")
                                 ("natural-stack" . "blame-following/natural-stack.rkt")
                                 ("null"          . "blame-following/null.rkt")
                                 ("transient-oldest" . "blame-following/transient-oldest.rkt")
                                 ("transient-all" . "blame-following/transient-all.rkt")))
      ("module-instrumentation" . #hash(("none" . "module-instrumentation/none.rkt")
                                        ("transient" . "module-instrumentation/type-with-transient.rkt")))
      ("benchmark-runner" . #hash(("run-it" . "benchmark-runner/run-it.rkt")
                                  ("load-pre-computed-result" . "benchmark-runner/load-pre-computed-result.rkt")))
      ("configuration-sampling" . #hash(("uniform-with-replacement" . "configuration-sampling/uniform-with-replacement.rkt")
                                        ("uniform-with-replacement/typing-mod-to-mutate"
                                         .
                                         "configuration-sampling/uniform-with-replacement-typing-mod-to-mutate.rkt"))))
