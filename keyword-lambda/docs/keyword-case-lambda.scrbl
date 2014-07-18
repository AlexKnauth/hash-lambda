#lang scribble/manual
@(require scribble/eval racket/sandbox)
@(require racket/base)
@(require keyword-lambda/keyword-case-lambda)
@(require (for-label keyword-lambda/keyword-case-lambda
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
     (make-evaluator '(begin (require keyword-lambda/keyword-case-lambda
                                      racket/match
                                      racket/format
                                      racket/local
                                      racket/contract
                                      racket/list
                                      racket/function
                                      racket/bool
                                      racket/math)))))

@title{keyword-case-lambda}

@defmodule[keyword-lambda/keyword-case-lambda #:packages ("hash-lambda")]

@defform[(keyword-case-lambda clause ...)
         #:grammar ([clause [kw-formals maybe-when body-expr ...+]
                            [#:args-hash match-pattern maybe-when body-expr ...+]
                            [#:rest match-pattern maybe-when body-expr ...+]
                            [#:kws match-pattern #:kw-args match-pattern #:rest match-pattern
                                   maybe-when body-expr ...+]
                            [_ maybe-when body-expr ...+]
                            [else maybe-when body-expr ...+]
                            [#:when condition-expr body-expr ...+]
                            [#:unless condition-expr body-expr ...+]]
                    [maybe-when (code:line)
                                (code:line #:when condition-expr)
                                (code:line #:unless condition-expr)]
                    [kw-formals (arg ...)
                                (arg ... . rest-id)
                                rest-id]
                    [arg id
                         [id default-expr]
                         (code:line keyword id)
                         (code:line keyword [id default-expr])])]{

like @racket[case-lambda], but accepting keyword- and optional arguments, 
and allowing args-hashes with the @racket[#:args-hash] keyword, and pattern matching with the
@racket[match-pattern]s.  

The @racket[kw-formals] work just like the @racket[kw-formals] for @racket[lambda].  

The @racket[#:rest match-pattern] clause @racket[match]es the list of arguments against
@racket[match-pattern], which can be any pattern that can be used with @racket[match] (not just an
identifier). 

The @racket[#:args-hash match-pattern] clause @racket[match]es the args-hash against
@racket[match-pattern], which again can be any pattern that can be used with @racket[match]. 

The @racket[#:kws match-pattern #:kw-args match-pattern #:rest match-pattern] clause @racket[match]es
the keywords, keyword-arguments, and by-position arguments (from @racket[keyword-lambda]) against the
3 @racket[match-pattern]s. 

The @racket[_] and @racket[else] clauses recive any arguments, including keyword-arguments, but it can
still go on to the next clause if the @racket[#:when] condition is is false (or the @racket[#:unless]
condition is true). 

The clauses starting with @racket[#:when] and @racket[#:unless] recive any arguments, (including
keyword-arguments), but can still go on to the next clause if the @racket[#:when] condition is false
(or the @racket[#:unless] condition is true).  

It also allows you to use @racket[#:when] and @racket[#:unless] in the clauses.  

@examples[
  #:eval
  (make-hash-lambda-evaluator)
  (define f
    (keyword-case-lambda))
  (procedure-arity f)
  (f)
  (define f
    (keyword-case-lambda
     [() 0]
     [(x) 1]
     [(x y) 2]
     [args (length args)]))
  (f)
  (f 1)
  (f 1 2)
  (f 1 2 3 4 5 6)
  (define pythag
    (keyword-case-lambda
     [(#:a a #:b b) (sqrt (+ (sqr a) (sqr b)))]
     [(#:c c #:a a) (sqrt (- (sqr c) (sqr a)))]
     [(#:c c #:b b) (sqrt (- (sqr c) (sqr b)))]
     [(#:a a #:b b #:c c) (= (+ (sqr a) (sqr b)) (sqr c))]))
  (pythag #:a 3 #:b 4)
  (pythag #:c 5 #:a 3)
  (pythag #:c 5 #:b 4)
  (pythag #:a 3 #:b 4 #:c 5)
  (pythag #:a 3 #:b 4 #:c 6)
  (define f
    (keyword-case-lambda
     [#:args-hash args-hash args-hash]))
  (f 0 1 2 #:kw "kw-arg")
  (define f
    (keyword-case-lambda
     [#:args-hash (hash-table ['#:m (? number? m)] ['#:v (? number? v)]) `(m: ,m v: ,v)]
     [(#:mass [m 0] #:velocity [v 0]) #:when (andmap number? (list m v)) `(mass: ,m velocity: ,v)]
     [#:rest `(m: ,m v: ,v) #:when (andmap number? (list m v)) `(m: ,m v: ,v)]
     [else (error "error")]))
  (f #:m 2 #:v 1)
  (f #:mass 2)
  (f #:mass 2 #:velocity 1)
  (f)
  (f 'm: 2 'v: 1)
  (f "something")
]}