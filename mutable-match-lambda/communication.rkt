#lang racket/base

(provide mutable-match-lambda-clause-append
         raise-mutable-match-lambda:no-match-error
         within-mutable-match-lambda-clause-append?
         (struct-out exn:fail:mutable-match-lambda:no-match)
         (struct-out exn:fail:mutable-match-lambda:no-match:next-clause))
         
(require hash-lambda)

(define mutable-match-lambda-clause-append
  (case-lambda
    [() (case-lambda)]
    [(f) f]
    [(f1 f2) (keyword-lambda (kws kw-args . args)
               (with-handlers ([exn:fail:mutable-match-lambda:no-match:next-clause?
                                (λ (e) (keyword-apply f2 kws kw-args args))])
                 (parameterize ([within-mutable-match-lambda-clause-append? #t])
                   (keyword-apply f1 kws kw-args args))))]
    [(f1 . rst) (mutable-match-lambda-clause-append f1 (apply mutable-match-lambda-clause-append rst))]
    ))

(define (raise-mutable-match-lambda:no-match-error args-hash)
  (define message
    (string-append
     "my-match-lambda: no clause matches" "\n"
     "  args-hash: "(args-hash->string args-hash)""))
  (define cont-marks
    (with-handlers ([exn:fail? exn-continuation-marks])
      (error message)))
  (define exn
    (cond [(within-mutable-match-lambda-clause-append?)
           (exn:fail:mutable-match-lambda:no-match:next-clause
            message cont-marks args-hash)]
          [else
           (exn:fail:mutable-match-lambda:no-match
            message cont-marks args-hash)]))
  (raise exn))

(define within-mutable-match-lambda-clause-append?
  (make-parameter #f))

(struct exn:fail:mutable-match-lambda:no-match exn:fail (args) #:transparent)
(struct exn:fail:mutable-match-lambda:no-match:next-clause exn:fail:mutable-match-lambda:no-match () #:transparent)

