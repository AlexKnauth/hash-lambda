#lang scribble/doc @(require scribble/manual)
@(require scribble/eval racket/sandbox)
@(require racket/base)
@(require hash-lambda)
@(require (for-label hash-lambda
                     racket/base
                     racket/match
                     racket/format
                     racket/local
                     racket/contract
                     racket/list
                     racket/function
                     racket/bool
                     racket/math))
@(define (make-hash-lambda-evaluator)
   (parameterize ([sandbox-namespace-specs (list make-base-namespace)]
                  [sandbox-output 'string]
                  [sandbox-error-output 'string])
     (make-evaluator '(begin (require hash-lambda
                                      racket/match
                                      racket/format
                                      racket/local
                                      racket/contract
                                      racket/list
                                      racket/function
                                      racket/bool
                                      racket/math)))))

@;displayln{starting}

@title{hash-lambda}

@defmodule[hash-lambda #:packages ("hash-lambda")]{

These functions allow you to create a
"@hyperlink["http://docs.racket-lang.org/guide/lambda.html?q=rest-argument#%28part._rest-args%29" #:underline? #f]{rest argument}"
that includes keyword arguments.  
Instead of storing the arguments in a list, @racket[hash-lambda] stores them in a hash table, 
where you can use @racket[(hash-ref args-hash 0)], etc. to access by-position arguments, 
and @racket[(hash-ref args-hash '#:<kw>)], etc. to access keyword arguments, or you can use
@racket[hash-lambda/match] and use @racket[match] patterns for the arguments.
You can also use @racket[apply/hash] to apply an args-hash to a function.  

@examples[
  #:eval
  (make-hash-lambda-evaluator)
  (define my+
    (hash-lambda/match
     [(hash-table [0 a] [1 b])
      (+ a b)]))
  (my+ 1 2)
  (define/contract kinetic-energy (#:mass number? #:velocity number? . -> . number?)
    (hash-lambda/match
     [(hash-table ['#:mass mass] ['#:velocity velocity])
      (* 1/2 mass (sqr velocity))]))
  (kinetic-energy #:mass 2 #:velocity 1)
]}

@;displayln{hash-lambda}

@section{hash-lambda}

@defform*[((hash-lambda args-hash-id body ...+)
           (hash-lambda [args-hash-id args-hash-contract] body ...+))]{
like @racket[(lambda args-list-id body ...+)], except that it takes all keywords, and it puts its arguments into a hash table instead of a list.  

In the hash table, the by-position arguments have their position as the key, 
and the keyword arguments have the (@racket[quote]d) keyword as the key.  

The second form is like the first form except that it applies @racket[args-hash-contract] to @racket[args-hash-id].  

@examples[
  #:eval
  (make-hash-lambda-evaluator)
  (define return-args-hash
    (hash-lambda args-hash
                 args-hash))
  (return-args-hash "0" "1" #:keyword "keyword-argument" "2")
  (define my+
    (hash-lambda args-hash
                 (+ (hash-ref args-hash 0)
                    (hash-ref args-hash 1))))
  (my+ 1 2)
  (define/contract kinetic-energy (#:mass number? #:velocity number? . -> . number?)
    (hash-lambda args-hash
                 (let ([mass     (hash-ref args-hash '#:mass)]
                       [velocity (hash-ref args-hash '#:velocity)])
                   (* 1/2 mass (sqr velocity)))))
  (kinetic-energy #:mass 2 #:velocity 1)
]}

@defform[(hash-lambda/match match-clause ...)
         #:grammar ([match-clause [pat body ...+]
                                  [pat (=> id) body ...+]
                                  [pat #:when cond-expr body ...+]])]{

allows you to use pattern matching instead of putting lots of @racket[hash-ref]s everywhere, 
and allows the procedure to do different things based on which of the @racket[pat]s match, 
whether the the @racket[#:when] test passes, and whether @racket[(=> id)] or @racket[(failure-cont)] is used. 

Equivalent to:
@(racketblock
  (hash-lambda args-hash
    (match args-hash
      match-clause ...)))

@examples[
  #:eval
  (make-hash-lambda-evaluator)
  (define my+
    (hash-lambda/match
     [(hash-table [0 a] [1 b])
      (+ a b)]))
  (my+ 1 2)
  (define/contract kinetic-energy (#:mass number? #:velocity number? . -> . number?)
    (hash-lambda/match
     [(hash-table ['#:mass mass] ['#:velocity velocity])
      (* 1/2 mass (sqr velocity))]))
  (kinetic-energy #:mass 2 #:velocity 1)
]}

@;displayln{args-hashes and apply/hash}

@section{args-hashes and apply/hash}

@defproc[(apply/hash [proc procedure?] [args-hash args-hash?] [#:<kw> kw-arg any/c] ...) any]{
like @racket[apply], except that it takes an @racket[args-hash?] instead of a @racket[list], 
and it doesn't take by-position arguments before the last argument like @racket[apply] does.  

@examples[
  #:eval
  (make-hash-lambda-evaluator)
  (apply/hash list (hash 0 "0" 1 "1"))
  (apply/hash list (hash 1 "1" 0 "0"))
  (define (kinetic-energy #:mass m #:velocity v)
    (* 1/2 m (sqr v)))
  (apply/hash kinetic-energy (hash '#:mass 2 '#:velocity 1))
]}

@defproc[(args-hash? [x any/c]) boolean?]{determines whether @racket[x] is a valid args-hash for use in @racket[apply/hash]}

@defproc[(make-args-hash [stuff any/c] ... [#:<kw> more-stuff any/c] ...) args-hash?]{equivalent to @racket[(hash-lambda args-hash args-hash)].

@examples[
  #:eval
  (make-hash-lambda-evaluator)
  (make-args-hash 1 2 3 #:kw-1 'kw-arg-1 #:kw-2 'kw-arg-2)
]
@racket[make-args-hash] is also bound as a match expander to be used with @racket[match].
@examples[
  #:eval
  (make-hash-lambda-evaluator)
  (match (make-args-hash 1 2 3 #:kw-1 'kw-arg-1 #:kw-2 'kw-arg-2)
    [(make-args-hash one two three #:kw-1 kw-arg-one #:kw-2 kw-arg-two)
     (list one two three kw-arg-one kw-arg-two)])
]}

@defproc[(args-hash-first [args-hash (and/c args-hash? (hash-has-key?/c 0))]) any/c]{
equivalent to @racket[(hash-ref args-hash 0)]
}

@defproc[(args-hash-rest [args-hash (and/c args-hash? (hash-has-key?/c 0))]) args-hash?]{
returns an args-hash without the @racket[0] key (with @racket[(hash-remove args-hash 0)]) 
and with all of the number-keys reduced by one.  

@examples[
  #:eval
  (make-hash-lambda-evaluator)
  (args-hash-rest (hash 0 "0" 1 "1" 2 "2" '#:kw "kw-arg"))
]}

@defproc[(args-hash-cons [val any/c] [args-hash args-hash?]) args-hash?]{
returns @racket[args-hash] with the number keys increased by one and with @racket[val] added with a key of @racket[0].  

@examples[
  #:eval
  (make-hash-lambda-evaluator)
  (args-hash-cons "thing" (hash 0 "0" 1 "1" 2 "2" '#:kw "kw-arg"))
]
@racket[args-hash-cons] is also bound as a match expander to be used with @racket[match].
@examples[
  #:eval
  (make-hash-lambda-evaluator)
  (match (args-hash-cons "thing" (hash 0 "0" 1 "1" 2 "2" '#:kw "kw-arg"))
    [(args-hash-cons val hash)
     (cons val hash)])
]}

@defproc[(args-hash-cons* [val any/c] ... [args-hash args-hash?]) args-hash?]{
returns @racket[args-hash] with the @racket[val]s all @racket[args-hash-cons]ed onto it, analogous to @racket[list*].  

@examples[
  #:eval
  (make-hash-lambda-evaluator)
  (args-hash-cons* "thing" "other-thing" (hash 0 "0" 1 "1" 2 "2" '#:kw "kw-arg"))
]
@racket[args-hash-cons*] is also bound as a match expander to be used with @racket[match].
@examples[
  #:eval
  (make-hash-lambda-evaluator)
  (match (args-hash-cons* "thing" "other-thing" (hash 0 "0" 1 "1" 2 "2" '#:kw "kw-arg"))
    [(args-hash-cons* val other-val hash)
     (list val other-val hash)])
]}

@defproc[(args-hash-append [args-hash args-hash?] ...) args-hash?]{
appends the @racket[args-hash]es together, with the number keys of the later @racket[args-hash]es increased.

@examples[
  #:eval
  (make-hash-lambda-evaluator)
  (args-hash-append (hash 0 "0" 1 "1" 2 "2" '#:kw "kw-arg")
                    (hash 0 "other-0" 1 "other-1" 2 "other-2" '#:other-kw "other-kw-arg"))
]}

@defproc[(args-hash->args-list [args-hash any/c]) (or/c list? #f)]{
equivalent to @racket[(apply/hash list args-hash)], 
except that if @racket[args-hash] either isn't an args-hash or contains any keywords, 
then it returns @racket[#false] instead of raising an exeption.  
}

@;displayln{misc.}

@section{misc.}

@defproc[(hash-has-key?/c [key any/c]) flat-contract?]{makes a contract that accepts hash tables that have the given key

@examples[
  #:eval
  (make-hash-lambda-evaluator)
  (define return-first-arg
    (hash-lambda [args-hash (hash-has-key?/c 0)]
                 (hash-ref args-hash 0)))
  (return-first-arg "first arg" "other-arg" #:kw "other-arg")
  (return-first-arg)
]}

@defform*[((match?/c pat)
           (match?/c pat #:when pred))]{
creates a flat-contract that accepts values that @racket[match] the given pattern.  

@examples[
  #:eval
  (make-hash-lambda-evaluator)
  (define return-first-arg
    (hash-lambda [args-hash (match?/c (hash-table [0 0-val] [keys vals] ...))]
                 (hash-ref args-hash 0)))
  (return-first-arg "first arg" "other-arg" #:kw "other-arg")
  (return-first-arg)
]}

@defproc[(make-hash-lambda-contract [args-hash-contract (or/c contract? 'any)] [range-contract (or/c contract? 'any) 'any]) contract?]{
creats a contract for a function that takes an args-hash that matches @racket[args-hash-contract], 
and produces something that matches @racket[range-contract].  

@examples[
  #:eval
  (make-hash-lambda-evaluator)
  (define/contract kinetic-energy
    (make-hash-lambda-contract (match?/c (hash-table ['#:mass (? number? m)] ['#:velocity (? number? v)])) number?)
    (hash-lambda/match [(hash-table ['#:mass (? number? m)] ['#:velocity (? number? v)])
                        (* 1/2 m (sqr v))]))
  (kinetic-energy #:mass 2 #:velocity 1)
  (kinetic-energy)
]}


@;displayln{should be done}
@;(newline)
