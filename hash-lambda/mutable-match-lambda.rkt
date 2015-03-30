#lang racket/base

(provide clause->proc/hash-lambda/match
         mutable-hash-lambda/match
         )

(require "main.rkt"
         (only-in mutable-match-lambda
                  make-mutable-match-lambda/infer-name
                  clause->proc
                  )
         )

(define-syntax-rule (clause->proc/hash-lambda/match clause)
  (hash-lambda/match clause [_ (mutable-match-lambda-next)]))

(define-syntax-rule (mutable-hash-lambda/match clause ...)
  (make-mutable-match-lambda/infer-name
   (clause->proc #:hash-lambda/match clause) ...))

