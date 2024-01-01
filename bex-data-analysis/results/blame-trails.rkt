#lang at-exp racket

(provide (struct-out blame-trail)
         (contract-out
          [satisfies-BT-hypothesis?/recompute (blame-trail? . -> . boolean?)]
          [satisfies-BT-hypothesis? ((listof mutant-summary?) string? . -> . boolean?)]))

(require bex/experiment/blame-trail-data
         bex/runner/mutation-runner-data
         bex/runner/mutation-runner-data
         bex/configurables/program-instrumentation/type-interface-module-names
         "data-adapter.rkt")

(struct blame-trail (mutant-id
                     trail-id
                     mode-config-name
                     ; Note: summaries are in reverse order! Last summary of trail is first.
                     mutant-summaries
                     succeeded?)
  #:transparent)

(define-logger bt-hypoth-options)
(define (satisfies-BT-hypothesis?/recompute bt)
  (satisfies-BT-hypothesis? (blame-trail-mutant-summaries bt)
                            (blame-trail-mode-config-name bt)))
(define (satisfies-BT-hypothesis? mutant-summaries mode)
  (define end-mutant-summary (first mutant-summaries))
  (define type-interface-mod?
    (match-lambda [(or (== type-interface-file-name)
                       (== type-interface-file-rename)) #t]
                  [else #f]))
  (match end-mutant-summary
    [(mutant-summary _
                     (struct* run-status ([mutated-module mutated-mod-name]
                                          [outcome 'type-error]
                                          [blamed blamed]))
                     config)
     ;; Original definition, changed to any type error because any type error
     ;; should be good enough: see justification in notes
     #;(and (equal? (hash-ref config mutated-mod-name) 'types)
          (list? blamed)
          (member mutated-mod-name blamed))
     #t]
    [(mutant-summary _
                     (struct* run-status ([mutated-module mutated-mod-name]
                                          [outcome 'blamed]
                                          [blamed blamed]))
                     config)
     #:when (member mode '("TR.rkt" "transient-newest.rkt" "transient-oldest.rkt"))
     (match* {mode blamed}
       [{_ (list (? type-interface-mod?))}
        #t]
       [{"transient-newest.rkt" (or (list* (? type-interface-mod?) _)
                                    (list* _ (? type-interface-mod?) _))}
        #t]
       [{"transient-oldest.rkt" (or (list _ ... (? type-interface-mod?))
                                    (list _ ... (? type-interface-mod?) _))}
        #t]
       [{_ other-blamed-list}
        (when (ormap type-interface-mod? other-blamed-list)
          (log-bt-hypoth-options-info
           @~a{mode @mode, blamed contains interface somewhere in middle}))
        #f])]
    [(mutant-summary _
                     (struct* run-status ([mutated-module mutated-mod-name]
                                          [outcome (and outcome
                                                        (or 'runtime-error
                                                            'blamed))]
                                          [context-stack stack]))
                     config)
     #:when (or (member mode '("TR-stack-first.rkt"
                               "transient-stack-first.rkt"
                               "erasure-stack-first.rkt"))
                (and (member mode '("TR.rkt" "transient-newest.rkt" "transient-oldest.rkt"))
                     (equal? outcome 'runtime-error)))
     (define stack/no-typed-mods
       (filter (λ (m) (equal? (hash-ref config m 'none) 'none))
               stack))
     (match stack/no-typed-mods
       [(list* (? type-interface-mod?) _) #t]
       [else #f])]
    [else #f]))

(module+ test
  (require ruinit
           "data-adapter.rkt")
  (test-begin
    #:name satisfies-BT-hypothesis?
    (satisfies-BT-hypothesis?/recompute
     (blame-trail '#s(mutant "acquire" "board.rkt" 5659)
                  87
                  "TR.rkt"
                  (map adapt-mutant-summary
                       '(#s(mutant-summary 26565 #s(run-status "board.rkt" 5659 what-kind-of-spot type-error ("board.rkt") #f) #hash(("admin.rkt" . types) ("auxiliaries.rkt" . types) ("basics.rkt" . types) ("board.rkt" . types) ("main.rkt" . types) ("player.rkt" . types) ("state.rkt" . types) ("strategy.rkt" . types) ("tree.rkt" . types)))
                         #s(mutant-summary 26454 #s(run-status "board.rkt" 5659 what-kind-of-spot runtime-error ("board.rkt") #f) #hash(("admin.rkt" . types) ("auxiliaries.rkt" . types) ("basics.rkt" . types) ("board.rkt" . none) ("main.rkt" . types) ("player.rkt" . types) ("state.rkt" . types) ("strategy.rkt" . types) ("tree.rkt" . types)))
                         #s(mutant-summary 26345 #s(run-status "board.rkt" 5659 what-kind-of-spot runtime-error ("auxiliaries.rkt") #f) #hash(("admin.rkt" . types) ("auxiliaries.rkt" . none) ("basics.rkt" . types) ("board.rkt" . none) ("main.rkt" . types) ("player.rkt" . types) ("state.rkt" . types) ("strategy.rkt" . types) ("tree.rkt" . types)))
                         #s(mutant-summary 26256 #s(run-status "board.rkt" 5659 what-kind-of-spot runtime-error ("admin.rkt") #f) #hash(("admin.rkt" . none) ("auxiliaries.rkt" . none) ("basics.rkt" . types) ("board.rkt" . none) ("main.rkt" . types) ("player.rkt" . types) ("state.rkt" . types) ("strategy.rkt" . types) ("tree.rkt" . types)))
                         #s(mutant-summary 26167 #s(run-status "board.rkt" 5659 what-kind-of-spot runtime-error ("state.rkt") #f) #hash(("admin.rkt" . none) ("auxiliaries.rkt" . none) ("basics.rkt" . types) ("board.rkt" . none) ("main.rkt" . types) ("player.rkt" . types) ("state.rkt" . none) ("strategy.rkt" . types) ("tree.rkt" . types)))
                         #s(mutant-summary 26093 #s(run-status "board.rkt" 5659 what-kind-of-spot runtime-error ("basics.rkt") #f) #hash(("admin.rkt" . none) ("auxiliaries.rkt" . none) ("basics.rkt" . none) ("board.rkt" . none) ("main.rkt" . types) ("player.rkt" . types) ("state.rkt" . none) ("strategy.rkt" . types) ("tree.rkt" . types)))
                         #s(mutant-summary 26003 #s(run-status "board.rkt" 5659 what-kind-of-spot runtime-error ("tree.rkt") #f) #hash(("admin.rkt" . none) ("auxiliaries.rkt" . none) ("basics.rkt" . none) ("board.rkt" . none) ("main.rkt" . types) ("player.rkt" . types) ("state.rkt" . none) ("strategy.rkt" . types) ("tree.rkt" . none)))
                         #s(mutant-summary 6350 #s(run-status "board.rkt" 5659 what-kind-of-spot runtime-error ("strategy.rkt") #f) #hash(("admin.rkt" . none) ("auxiliaries.rkt" . none) ("basics.rkt" . none) ("board.rkt" . none) ("main.rkt" . types) ("player.rkt" . types) ("state.rkt" . none) ("strategy.rkt" . none) ("tree.rkt" . none)))))
                  'ignored))
    (not (satisfies-BT-hypothesis?/recompute
          (blame-trail '#s(mutant "acquire" "board.rkt" 5659)
                       87
                       "TR.rkt"
                       (map adapt-mutant-summary
                            '(#s(mutant-summary 26256 #s(run-status "board.rkt" 5659 what-kind-of-spot runtime-error ("admin.rkt") #f) #hash(("admin.rkt" . none) ("auxiliaries.rkt" . none) ("basics.rkt" . types) ("board.rkt" . none) ("main.rkt" . types) ("player.rkt" . types) ("state.rkt" . types) ("strategy.rkt" . types) ("tree.rkt" . types)))
                              #s(mutant-summary 26167 #s(run-status "board.rkt" 5659 what-kind-of-spot runtime-error ("state.rkt") #f) #hash(("admin.rkt" . none) ("auxiliaries.rkt" . none) ("basics.rkt" . types) ("board.rkt" . none) ("main.rkt" . types) ("player.rkt" . types) ("state.rkt" . none) ("strategy.rkt" . types) ("tree.rkt" . types)))
                              #s(mutant-summary 26093 #s(run-status "board.rkt" 5659 what-kind-of-spot runtime-error ("basics.rkt") #f) #hash(("admin.rkt" . none) ("auxiliaries.rkt" . none) ("basics.rkt" . none) ("board.rkt" . none) ("main.rkt" . types) ("player.rkt" . types) ("state.rkt" . none) ("strategy.rkt" . types) ("tree.rkt" . types)))
                              #s(mutant-summary 26003 #s(run-status "board.rkt" 5659 what-kind-of-spot runtime-error ("tree.rkt") #f) #hash(("admin.rkt" . none) ("auxiliaries.rkt" . none) ("basics.rkt" . none) ("board.rkt" . none) ("main.rkt" . types) ("player.rkt" . types) ("state.rkt" . none) ("strategy.rkt" . types) ("tree.rkt" . none)))
                              #s(mutant-summary 6350 #s(run-status "board.rkt" 5659 what-kind-of-spot runtime-error ("strategy.rkt") #f) #hash(("admin.rkt" . none) ("auxiliaries.rkt" . none) ("basics.rkt" . none) ("board.rkt" . none) ("main.rkt" . types) ("player.rkt" . types) ("state.rkt" . none) ("strategy.rkt" . none) ("tree.rkt" . none)))))
                       'ignored)))
    (satisfies-BT-hypothesis?/recompute (blame-trail
                               #s(mutant "quadU" "type-interface.rkt" 5)
                               34
                               "transient-oldest.rkt"
                               '(#s(mutant-summary
                                    4560
                                    #s(run-status
                                       "type-interface.rkt"
                                       5
                                       Quad
                                       blamed
                                       ("type-interface.rkt"
                                        "type-interface.rkt"
                                        "quick-sample.rkt"
                                        "type-interface.rkt"
                                        "type-interface.rkt")
                                       ("main.rkt" "main.rkt" "main.rkt")
                                       ("main.rkt" "main.rkt")
                                       #f)
                                    #hash(("hyphenate.rkt" . types)
                                          ("main.rkt" . types)
                                          ("measure.rkt" . types)
                                          ("ocm-struct.rkt" . none)
                                          ("ocm.rkt" . types)
                                          ("penalty-struct.rkt" . none)
                                          ("quad-main.rkt" . types)
                                          ("quads.rkt" . none)
                                          ("quick-sample.rkt" . types)
                                          ("render.rkt" . none)
                                          ("sugar-list.rkt" . none)
                                          ("utils.rkt" . none)
                                          ("world.rkt" . types)
                                          ("wrap.rkt" . none)))
                                 #s(mutant-summary
                                    323
                                    #s(run-status
                                       "type-interface.rkt"
                                       5
                                       Quad
                                       blamed
                                       ("type-interface.rkt"
                                        "type-interface.rkt"
                                        "quick-sample.rkt"
                                        "original-type-interface.rkt")
                                       ("main.rkt" "main.rkt" "main.rkt")
                                       ("main.rkt" "main.rkt")
                                       #f)
                                    #hash(("hyphenate.rkt" . types)
                                          ("main.rkt" . types)
                                          ("measure.rkt" . types)
                                          ("ocm-struct.rkt" . none)
                                          ("ocm.rkt" . types)
                                          ("penalty-struct.rkt" . none)
                                          ("quad-main.rkt" . types)
                                          ("quads.rkt" . none)
                                          ("quick-sample.rkt" . none)
                                          ("render.rkt" . none)
                                          ("sugar-list.rkt" . none)
                                          ("utils.rkt" . none)
                                          ("world.rkt" . types)
                                          ("wrap.rkt" . none))))
                               'ignored))))
