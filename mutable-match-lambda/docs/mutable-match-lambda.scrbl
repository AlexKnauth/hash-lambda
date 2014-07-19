#lang scribble/manual
@(require scribble/eval racket/sandbox)
@(require racket/base)
@(require mutable-match-lambda)
@(require (for-label hash-lambda
                     mutable-match-lambda
                     racket/base
                     racket/match
                     racket/format
                     racket/local
                     racket/contract
                     racket/list
                     racket/function
                     racket/bool
                     racket/math
                     racket/vector))
@(define (make-hash-lambda-evaluator)
   (parameterize ([sandbox-namespace-specs (list make-base-namespace)]
                  [sandbox-output 'string]
                  [sandbox-error-output 'string])
     (make-evaluator '(begin (require mutable-match-lambda
                                      racket/match
                                      racket/format
                                      racket/local
                                      racket/contract
                                      racket/list
                                      racket/function
                                      racket/bool
                                      racket/math
                                      racket/vector)))))

@title{mutable-match-lambda}

@defmodule[mutable-match-lambda #:packages ("hash-lambda")]{

These functions allow a mutable generic procedure like this:

@examples[
  #:eval
  (make-hash-lambda-evaluator)
  (define my+ (mutable-match-lambda))
  (mutable-match-lambda-add-clause! my+ #:match-lambda* [(list (? number? ns) ...) (apply + ns)])
  (my+ 1 2)
  (mutable-match-lambda-add-clause! my+ #:match-lambda* [(list (? vector? vs) ...) (apply vector-map + vs)])
  (my+ #(1 2) #(3 4))
]}

@section{mutable-match-lambda-procedure}

@defstruct*[mutable-match-lambda-procedure ([procs (listof procedure?)]) #:mutable #:transparent]{
represents a procedure, with a @racket[prop:procedure] property that allows it to be applied as a procedure.
It tries each of its @racket[procs] in order, using @racket[mutable-match-lambda-clause-append].  
Forms like @racket[mutable-case-lambda] and @racket[mutable-match-lambda] all create instances of this.  
When they do, each clause is converted to a procedure (using @racket[clause->proc]) and the list of procedures
is stored in the @racket[procs] field.  
}

@defproc[(make-mutable-match-lambda [proc procedure?] ...) mutable-match-lambda-procedure?]{
equivalent to @racket[(mutable-match-lambda-procedure (list proc ...))].
}

@deftogether[(@defproc[(mutable-match-lambda-add-clause-proc! [proc mutable-match-lambda-procedure?] [clause-proc procedure?] ...) void?]
              @defproc[(mutable-match-lambda-add-overriding-clause-proc! [proc mutable-match-lambda-procedure?] [clause-proc procedure?] ...) void?])]{
these functions add clauses to a @racket[mutable-match-lambda-procedure].  
The difference between them is that @racket[mutable-match-lambda-procedure-add-clause-proc!] adds a clause that is only used when no other clause matches,
and @racket[mutable-match-lambda-add-overriding-clause-proc!] adds a clause that overrides the other clauses when it matches.

They are defined like this:
@(racketblock
  (define (mutable-match-lambda-add-clause-proc! proc . clause-procs)
    (set-mutable-match-lambda-procedure-procs! proc
                                               (append (mutable-match-lambda-procedure-procs proc)
                                                       clause-procs)))
  
  (define (mutable-match-lambda-add-overriding-clause-proc! proc . clause-procs)
    (set-mutable-match-lambda-procedure-procs! proc
                                               (append clause-procs
                                                       (mutable-match-lambda-procedure-procs proc))))
  )

@examples[
  #:eval
  (make-hash-lambda-evaluator)
  (define my+ (make-mutable-match-lambda))
  (mutable-match-lambda-add-clause-proc! my+ (clause->proc #:match-lambda* [(list (? number? ns) ...) (apply + ns)]))
  (my+ 1 2)
  (mutable-match-lambda-add-clause-proc! my+ (clause->proc #:match-lambda* [(list (? vector? vs) ...) (apply vector-map + vs)]))
  (my+ #(1 2) #(3 4))
  (mutable-match-lambda-add-clause-proc! my+ (lambda args 5))
  (my+ 1 2)
  (my+ #(1 2) #(3 4))
  (my+ "not a number or a vector")
  (mutable-match-lambda-add-overriding-clause-proc! my+ (lambda args 7))
  (my+ 1 2)
]}

@deftogether[(@defform*[((mutable-match-lambda-add-clause! proc-expr clause-proc-expr ...)
                         (mutable-match-lambda-add-clause! proc-expr kw clause ...))]
              @defform*[((mutable-match-lambda-add-overriding-clause! proc-expr clause-proc-expr ...)
                         (mutable-match-lambda-add-overriding-clause! proc-expr kw clause ...))])]{
these forms add clauses to a @racket[mutable-match-lambda-procedure].  
The first form (for both) adds the @racket[clause-proc-expr]s to the list of procs, and is
exactly like @racket[mutable-match-lambda-add-clause-proc!] and @racket[mutable-match-lambda-add-overriding-clause-proc!].  
The second form (for both) converts the @racket[clause]s to procedures (using @racket[(clause->proc kw clause)]),
and then adds those to the list of procs.  
The difference between them is the same as the difference between
@racket[mutable-match-lambda-add-clause-proc!] and
@racket[mutable-match-lambda-add-overriding-clause-proc!].  

@examples[
  #:eval
  (make-hash-lambda-evaluator)
  (define my+ (make-mutable-match-lambda))
  (mutable-match-lambda-add-clause! my+ #:match-lambda* [(list (? number? ns) ...) (apply + ns)])
  (my+ 1 2)
  (mutable-match-lambda-add-clause! my+ #:match-lambda* [(list (? vector? vs) ...+) (apply vector-map + vs)])
  (my+ #(1 2) #(3 4))
  (mutable-match-lambda-add-clause! my+ (lambda args 7))
  (my+ 1 2)
  (my+ #(1 2) #(3 4))
  (my+ "not a number or a vector")
  (mutable-match-lambda-add-overriding-clause! my+ (lambda args 42))
  (my+ 1 2)
]}

@defproc[(mutable-match-lambda-clause-append [proc procedure?] ...) procedure?]{
makes a new procedure that has tries all of the @racket[proc]s in order.

This is what @racket[mutable-match-lambda-procedure] uses to combine its clauses.  
}

@defproc[(mutable-match-lambda-append [proc procedure?] ...) mutable-match-lambda-procedure?]{
makes a new @racket[mutable-match-lambda-procedure] that has tries all of the @racket[proc]s in order.

The difference between this and @racket[make-mutable-match-lambda] is that if a @racket[proc] is a
@racket[mutable-match-lambda-procedure], then its procs are spliced into the resulting list.  
}

@section{mutable-match-lambda, etc}

@defform[(mutable-case-lambda case-lambda-clause ...)]{
like @racket[case-lambda], except makes a @racket[mutable-match-lambda-procedure]
that you can add functionality to with procedures such as @racket[mutable-match-lambda-add-clause!].
By the way, you can add other types of clauses than @racket[case-lambda] clauses later.

It is defined like this:
@(racketblock
  (define-syntax-rule (mutable-case-lambda clause ...)
    (make-mutable-match-lambda
     (clause->proc #:case-lambda clause) ...))
  )

@examples[
  #:eval
  (make-hash-lambda-evaluator)
  (examples)
]}

@defform[(mutable-hash-lambda/match hash-lambda/match-clause ...)]{
like @racket[hash-lambda/match], except makes a @racket[mutable-match-lambda-procedure]
that you can add functionality to with procedures such as @racket[mutable-match-lambda-add-clause!].
By the way, you can add other types of clauses than @racket[hash-lambda/match] clauses later.

It is defined like this:
@(racketblock
  (define-syntax-rule (mutable-hash-lambda/match clause ...)
    (make-mutable-match-lambda
     (clause->proc #:hash-lambda/match clause) ...))
  )

@examples[
  #:eval
  (make-hash-lambda-evaluator)
  (examples)
]}

@defform[(mutable-match-lambda match-lambda-clause ...)]{
like @racket[match-lambda], except makes a @racket[mutable-match-lambda-procedure]
that you can add functionality to with procedures such as @racket[mutable-match-lambda-add-clause!].
By the way, you can add other types of clauses than @racket[match-lambda] clauses later.

It is defined like this:
@(racketblock
  (define-syntax-rule (mutable-match-lambda clause ...)
    (make-mutable-match-lambda
     (clause->proc #:match-lambda clause) ...))
  )

@examples[
  #:eval
  (make-hash-lambda-evaluator)
  (examples)
]}

@defform[(mutable-match-lambda* match-lambda*-clause ...)]{
like @racket[match-lambda*], except makes a @racket[mutable-match-lambda-procedure]
that you can add functionality to with procedures such as @racket[mutable-match-lambda-add-clause!].
By the way, you can add other types of clauses than @racket[match-lambda*] clauses later.

It is defined like this:
@(racketblock
  (define-syntax-rule (mutable-match-lambda* clause ...)
    (make-mutable-match-lambda
     (clause->proc #:match-lambda* clause) ...))
  )

@examples[
  #:eval
  (make-hash-lambda-evaluator)
  (examples)
]}

@section{make-clause-proc and clause->proc}

@defproc[(make-clause-proc [test-proc procedure?] [then-proc procedure?]) procedure?]{
makes a procedure that @racket[mutable-match-lambda-procedure] can use as a clause-proc.  
When it is called, it calls @racket[test-proc] with it's arguments, 
and if @racket[test-proc] returns a true value, it then calls @racket[then-proc] with it's arguments.  
If @racket[test-proc] returns @racket[#false], then it raises an exeption that is caught by 
@racket[mutable-match-lambda-procedure] (or @racket[mutable-match-lambda-clause-append]) so that it moves on to the next clause.  

@examples[
  #:eval
  (make-hash-lambda-evaluator)
  (define clause-1 (make-clause-proc (λ args (andmap number? args))
                                     (λ args (apply + args))))
  (define clause-2 (make-clause-proc (λ args (andmap vector? args))
                                     (λ args (apply vector-map + args))))
  (define my+
    (make-mutable-match-lambda clause-1 clause-2))
  (my+ 1 2)
  (clause-1 1 2)
  (my+ #(1 2) #(3 4))
  (clause-2 #(1 2) #(3 4))
  (clause-2 1 2)
  (clause-1 #(1 2) #(3 4))
]}

@defform*[((clause->proc #:case-lambda case-lambda-clause)
           (clause->proc #:hash-lambda/match hash-lambda/match-clause)
           (clause->proc #:match-lambda match-lambda-clause)
           (clause->proc #:match-lambda* match-lambda*-clause))]{
makes a procedure that @racket[mutable-match-lambda-procedure] can use as a clause-proc
The keyword specifies what type of clause it is.  If the clause fails to match, it raises an exeption that is caught by
@racket[mutable-match-lambda-procedure] (or @racket[mutable-match-lambda-clause-append]) so that it moves on to the next clause.  

@examples[
  #:eval
  (make-hash-lambda-evaluator)
  (examples)
]}