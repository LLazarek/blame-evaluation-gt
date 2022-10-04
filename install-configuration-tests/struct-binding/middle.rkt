#lang typed/racket
(require/typed "A.rkt"
  [#:struct State ([call : Any])])
(provide (struct-out State))
