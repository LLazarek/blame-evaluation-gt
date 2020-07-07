# Code mutations
## `arithmetic-op-swap`
Swaps an arithmetic operator with another one of the same (or greater) arity.

Example: `*` -> `/`

## `boolean-op-swap`
Swaps `and` and `or`.

Example: `or` -> `and`

## `class:publicity`
Makes a public method private or vice versa in a class.

Example: `(class (define/public (f x) x))` -> `(class (define/private (f x) x))`

## `class:super-new`
Removes the `super-new` expression from a class body.

Example: `(class (super-new))` -> `(class (void))`

## `data-accessor-swap`
Swaps `car` and `cdr`.

Example: `cdr` -> `car`

## `constant-swap`
Swaps literal constants with other constants.

Example: `5.6` -> `5.6+0.0i`

## `begin-result-deletion`
Deletes the expression in the result position of `begin` and `begin0`.

Example: `(begin x y z)` -> `(begin x y)`

## `negate-conditional`
Negates conditional test expressions.

Example: `(cond [(= x 0) 42] [else 32])` -> `(cond [(not (= x 0)) 42] [else 32])`

## `class:parent-swap`
Replaces the parent of classes with `object%`.

Example: `(class foo% (field x))` -> `(class object% (field x))` 

## `class:initializer-swap`
Swaps default values for class fields.

Example: `(class object% (field [a 5] [b "hello"]))` -> `(class object% (field [a "hello"] [b 5]))`

## `position-swap`
Swaps subexpression positions.

Example: `(f a 42 "b" 0)` -> `(f a 42 0 "b")`

## `class:add-extra-method`
Adds an extra public method to classes that returns its lone argument.

Example: `(class object%)` -> `(class object% (define/public (extra x) x))`

## `top-level-id-swap`
Swaps a use of an identifier defined with the same module at the top level with other top level-defined identifiers in the module.

Example:
```
(define a 42)
(define b 0)
(define choice-of-a-or-b a)
```
->
```
(define a 42)
(define b 0)
(define choice-of-a-or-b b)
```



# Type mutations
## `known-type-generalization`
Replaces a known base value type with a supertype.

Example: `Integer` -> `Real`

## `known-type-restriction`
Replaces a known base value type with a subtype.

Example: `Real` -> `Integer`

## `position-swap`
Swaps the position of two types in a compound type.

Example: `(-> A B C)` -> `(-> B A C)`

## `drop-ho-function-arg`
Drops one argument in a function type which is directly nested in another function type.

Example: `(-> (-> A B) C)` -> `(-> (-> B) C)`

## `drop-union-branch`
Drops one branch of a union type.

Example: `(U A B C D)` -> `(U A B D)`

## `class:drop-method`
Drops one method from a class type.

Example: `(Class [f (-> A B)] [g (-> C D)])` -> `(Class [f (-> A B)])`

## `class:swap-implements`
Swaps the class type identified as a parent by `#:implements` with `ClassTop`.

Example: `(Class #:implements Foo)` -> `(Class #:implements ClassTop)`

## `optional-args-mandatory`
Makes all optional arguments of a function type that accepts them into mandatory arguments.

Example: `(->* {A B} {C} D)` -> `(-> A B C D)`

## `replace-with-Any`
Swaps any type with `Any`.

Example: `(-> (-> Something) (U Very (-> (-> Complex))))` -> `Any`

## `drop-case->-case`
Drops a case from a `case->`.

Example: `(case-> (-> C) (-> A C) (-> A B C))` -> `(case-> (->C) (-> A B C))`

