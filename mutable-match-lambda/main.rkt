#lang racket/base

(provide (all-from-out
          "mutable-match-lambda-procedure.rkt"
          "make-clause-proc.rkt")
         mutable-case-lambda
         mutable-hash-lambda/match
         mutable-match-lambda
         mutable-match-lambda*
         mutable-match-lambda-add-clause!
         mutable-match-lambda-add-overriding-clause!
         )

(require "mutable-match-lambda-procedure.rkt"
         "make-clause-proc.rkt"
         (for-syntax racket/base
                     syntax/parse
                     (for-syntax racket/base)))

(module+ test
  (require rackunit)
  
  (define dup (mutable-match-lambda))
  (mutable-match-lambda-add-clause! dup (make-clause-proc string?  (lambda (s) (string-append s s))))
  (mutable-match-lambda-add-clause! dup #:match-lambda* [(list (? integer? n)) (list n n)])
  
  (check-equal? (dup "Hello") "HelloHello")
  (check-equal? (dup 10) '(10 10))
  
  (define my+ (mutable-match-lambda))
  (define (numbers? . args) (andmap number? args))
  (mutable-match-lambda-add-clause! my+ (make-clause-proc numbers? +))
  (define (vectors? . args) (andmap vector? args))
  (define (v+ . args) (list->vector (apply map + (map vector->list args))))
  (mutable-match-lambda-add-clause! my+ (make-clause-proc vectors? v+))
  
  (check-equal? (my+ 1 2 3) 6)
  (check-equal? (my+ #(1 2 3)
                     #(2 3 4)
                     #(3 4 5))
                #(6 9 12))
  
  )

(begin-for-syntax
  (define-syntax kw (make-rename-transformer #'keyword)))

(define-syntax-rule (mutable-case-lambda clause ...)
  (make-mutable-match-lambda
   (clause->proc #:case-lambda clause) ...))

(define-syntax-rule (mutable-hash-lambda/match clause ...)
  (make-mutable-match-lambda
   (clause->proc #:hash-lambda/match clause) ...))

(define-syntax-rule (mutable-match-lambda clause ...)
  (make-mutable-match-lambda
   (clause->proc #:match-lambda clause) ...))

(define-syntax-rule (mutable-match-lambda* clause ...)
  (make-mutable-match-lambda
   (clause->proc #:match-lambda* clause) ...))

(define-syntax mutable-match-lambda-add-clause!
  (lambda (stx)
    (syntax-parse stx
      [(mutable-match-lambda-add-clause! proc:expr clause-proc:expr ...)
       #'(mutable-match-lambda-add-clause-proc! proc clause-proc ...)]
      [(mutable-match-lambda-add-clause! proc:expr kw:kw clause:expr ...)
       #'(mutable-match-lambda-add-clause-proc! proc (clause->proc kw clause) ...)]
      [mutable-match-lambda-add-clause!:id
       #'mutable-match-lambda-add-clause-proc!]
      )))

(define-syntax mutable-match-lambda-add-overriding-clause!
  (lambda (stx)
    (syntax-parse stx
      [(mutable-match-lambda-add-overriding-clause! proc:expr clause-proc:expr ...)
       #'(mutable-match-lambda-add-overriding-clause-proc! proc clause-proc ...)]
      [(mutable-match-lambda-add-overriding-clause! proc:expr kw:kw clause:expr ...)
       #'(mutable-match-lambda-add-overriding-clause-proc! proc (clause->proc kw clause) ...)]
      [mutable-match-lambda-add-overriding-clause!:id
       #'mutable-match-lambda-add-overriding-clause-proc!]
      )))

