#lang racket/base (require (for-syntax racket/base))

(provide mutable-case-lambda
         mutable-hash-lambda/match
         mutable-match-lambda
         mutable-match-lambda*
         (struct-out mutable-match-lambda-procedure)
         mutable-match-lambda-append
         mutable-match-lambda-add-clause!
         mutable-match-lambda-add-overriding-clause!
         (all-from-out "make-clause-proc.rkt")
         )

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

(require (for-syntax
          syntax/parse))
(require "make-clause-proc.rkt"
         "../misc/keyword-lambda.rkt")

(begin-for-syntax
  (define-syntax-class kw
    (pattern kw:keyword)))

(define (make-mutable-match-lambda . procs)
  (mutable-match-lambda-procedure procs)) 

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



(struct mutable-match-lambda-procedure (procs)
  #:transparent #:mutable
  #:property prop:procedure
  (keyword-lambda (kws kw-args this . args)
    (let ([procs (mutable-match-lambda-procedure-procs this)])
      (define proc (apply mutable-match-lambda-append procs))
      (keyword-apply proc kws kw-args args)))
  #:methods gen:custom-write
  [(define (write-proc this out mode)
     (begin
       (display "(make-mutable-match-lambda" out)
       (for ([proc (in-list (mutable-match-lambda-procedure-procs this))])
         (display " " out) (print proc out))
       (display ")")))])



(define mutable-match-lambda-append
  (case-lambda
    [() (case-lambda)]
    [(f) f]
    [(f1 f2) (keyword-lambda (kws kw-args . args)
               (with-handlers ([exn:fail:mutable-match-lambda:no-match:next-clause?
                                (λ (e) (keyword-apply f2 kws kw-args args))])
                 (parameterize ([within-mutable-match-lambda-append? #t])
                   (keyword-apply f1 kws kw-args args))))]
    [(f1 . rst) (mutable-match-lambda-append f1 (apply mutable-match-lambda-append rst))]
    ))

(define (mutable-match-lambda-add-clause-proc! proc . clause-procs)
  (set-mutable-match-lambda-procedure-procs! proc
                                             (append (mutable-match-lambda-procedure-procs proc)
                                                     clause-procs)))

(define (mutable-match-lambda-add-overriding-clause-proc! proc . clause-procs)
  (set-mutable-match-lambda-procedure-procs! proc
                                             (append clause-procs
                                                     (mutable-match-lambda-procedure-procs proc))))

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


