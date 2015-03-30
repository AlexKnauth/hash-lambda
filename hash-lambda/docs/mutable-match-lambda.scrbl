#lang scribble/manual
@(require scribble/eval
          racket/sandbox
          (for-label hash-lambda
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

@title{mutable-match-lambda forms for hash-lambda}

@defmodule[hash-lambda/mutable-match-lambda]

@defform[(mutable-hash-lambda/match hash-lambda/match-clause ...)]{
like @racket[hash-lambda/match], except makes a @racket[mutable-match-lambda-procedure]
that you can add functionality to with procedures such as @racket[mutable-match-lambda-add-clause!].
By the way, you can add other types of clauses than @racket[hash-lambda/match] clauses later.

It is defined like this:
@(racketblock
  (define-syntax-rule (mutable-hash-lambda/match clause ...)
    (make-mutable-match-lambda/infer-name
     (clause->proc #:hash-lambda/match clause) ...))
  )

@examples[
  (require mutable-match-lambda)
  (examples)
]}

@defform[(clause->proc/hash-lambda/match clause)]{
this form is provided so that @racket[#:hash-lambda/match] can be used as a
keyword in @racket[clause->proc].

@racket[(clause->proc #:hash-lambda/match clause)] expands into
@racket[(clause->proc/hash-lambda/match clause)].
}

