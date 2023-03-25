#lang typed/racket
(require require-typed-check)
(require/typed/check "middle.rkt"
                     [#:struct State ([call : Any])])
(provide (struct-out State))
