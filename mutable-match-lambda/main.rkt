#lang racket/base

(provide (all-from-out
          mutable-match-lambda/mutable-match-lambda-procedure
          mutable-match-lambda/make-clause-proc)
         mutable-case-lambda
         mutable-hash-lambda/match
         mutable-match-lambda
         mutable-match-lambda*
         )

(require mutable-match-lambda/mutable-match-lambda-procedure
         mutable-match-lambda/make-clause-proc)

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

