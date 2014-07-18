#lang scribble/manual
@(require scribble/eval racket/sandbox)
@(require racket/base)
@(require keyword-lambda)
@(require (for-label keyword-lambda
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
     (make-evaluator '(begin (require keyword-lambda
                                      racket/match
                                      racket/format
                                      racket/local
                                      racket/contract
                                      racket/list
                                      racket/function
                                      racket/bool
                                      racket/math)))))

@title{keyword-lambda}

@defmodule[keyword-lambda]

@section{keyword-lambda, keyword-apply/sort, and procedure-reduce-keyword-arity/sort}

@defform[(keyword-lambda (kws kw-args . rest-args) body ...)]{
equivalent to
@(racketblock
  (make-keyword-procedure
   (lambda (kws kw-args . rest-args) body ...))
  )

@examples[
  #:eval
  (make-hash-lambda-evaluator)
  (define proc
    (keyword-lambda (kws kw-args . rest-args)
      (list kws kw-args rest-args)))
  (proc #:a 'a #:b 'b 0 1 2)
]}

@defproc[(keyword-apply/sort [f procedure?] [kws (listof keyword?)] [kw-args list?]
                             [v any/c] ... [lst list?] [#:<kw> kw-arg any/c] ...) any]{
like @racket[keyword-apply], but without the constraint that the keywords in @racket[kws] must be
sorted.  

@examples[
  #:eval
  (make-hash-lambda-evaluator)
  (define (kinetic-energy #:mass m #:velocity v)
    (* 1/2 m (sqr v)))
  (keyword-apply/sort kinetic-energy '(#:mass #:velocity) '(2 1) '())
  (keyword-apply/sort kinetic-energy '(#:velocity #:mass) '(1 2) '())
]}

@defproc[(procedure-reduce-keyword-arity/sort [proc procedure?]
                                              [arity procedure-arity?]
                                              [required-kws (listof keyword?)]
                                              [allowed-kws (or/c (listof keyword?) #f)])
         procedure?]{
like @racket[procedure-reduce-keyword-arity], but without the constraint that the keywords in
@racket[required-kws] or @racket[allowed-kws] must be sorted.
}

@section{arity+keywords}

@defstruct*[arity+keywords ([arity procedure-arity?]
                            [required-kws (listof keyword?)]
                            [allowed-kws (or/c (listof keyword?) #f)])
                           #:transparent]{
represents a procedure's arity including the keywords required and keywords allowed.

The @racket[arity] field represents the arity produced by @racket[procedure-arity].

The next 2 fields (@racket[required-kws] and @racket[allowed-kws]) represent the 2 values produced by
@racket[procedure-keywords].  
}

@defproc[(procedure-arity+keywords [proc procedure?]) arity+keywords?]{
returns an @racket[arity+keywords] instance representing the arity and keyword-arity of @racket[proc].

It is defined like this:
@(racketblock
  (define (procedure-arity+keywords proc)
    (define arity (procedure-arity proc))
    (define-values (req-kws allowed-kws)
      (procedure-keywords proc))
    (arity+keywords arity req-kws allowed-kws)))

@examples[
  #:eval
  (make-hash-lambda-evaluator)
  (define proc void)
  (procedure-arity+keywords proc)
  (procedure-arity+keywords (procedure-reduce-arity proc 5))
  (procedure-arity+keywords
   (procedure-reduce-keyword-arity/sort proc 3 '(#:kw #:other-kw) '(#:kw #:other-kw #:optional-kw)))
]}

@defproc[(procedure-reduce-arity+keywords [proc procedure?] [arity+kws arity+keywords?]) procedure?]{
like @racket[procedure-reduce-arity], except that it accepts an @racket[arity+keywords] and handles
the keyword-arity as well.  It uses @racket[procedure-reduce-keyword-arity/sort] to do this.

It is defined like this:
@(racketblock
  (define (procedure-reduce-arity+keywords proc a)
    (procedure-reduce-keyword-arity/sort
     proc
     (arity+keywords-arity a)
     (arity+keywords-required-kws a)
     (arity+keywords-allowed-kws a))))

@examples[
  #:eval
  (make-hash-lambda-evaluator)
  (define proc void)
  (procedure-arity proc)
  (procedure-keywords proc)
  (define proc-with-arity
    (procedure-reduce-arity+keywords
     proc
     (arity+keywords 5 '(#:kw #:other-kw) '(#:kw #:other-kw #:optional-kw))))
  (procedure-arity proc-with-arity)
  (procedure-keywords proc-with-arity)
]}

@defproc[(arity+keywords-combine [arity+kws arity+keywords?] ...) arity+keywords?]{
combines the @racket[arity+kws]es into one @racket[arity+keywords] instance in an or-like way.  

@examples[
  #:eval
  (make-hash-lambda-evaluator)
  (arity+keywords-combine (arity+keywords 1 '(#:kw-1)        '(#:kw-1 #:kw-2 #:kw-3))
                          (arity+keywords 2 '(#:kw-1 #:kw-2) '(#:kw-1 #:kw-2 #:kw-4)))
]}