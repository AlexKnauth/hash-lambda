#lang racket/base

(provide (struct-out mutable-match-lambda-procedure)
         make-mutable-match-lambda
         mutable-match-lambda-append
         mutable-match-lambda-copy
         mutable-match-lambda-add-clause-proc!
         mutable-match-lambda-add-overriding-clause-proc!
         )

(require racket/list
         kw-utils/keyword-lambda
         (only-in "communication.rkt" mutable-match-lambda-clause-append)
         (for-syntax racket/base
                     syntax/parse
                     (for-syntax racket/base)))

(begin-for-syntax
  (define-syntax kw (make-rename-transformer #'keyword)))

(define (make-mutable-match-lambda . procs)
  (mutable-match-lambda-procedure procs)) 

(struct mutable-match-lambda-procedure (procs)
  #:transparent #:mutable
  #:property prop:procedure
  (keyword-lambda (kws kw-args this . args)
    (let ([procs (mutable-match-lambda-procedure-procs this)])
      (define proc (apply mutable-match-lambda-clause-append procs))
      (keyword-apply proc kws kw-args args)))
  #:methods gen:custom-write
  [(define (write-proc this out mode)
     (begin
       (display "(make-mutable-match-lambda" out)
       (for ([proc (in-list (mutable-match-lambda-procedure-procs this))])
         (display " " out) (print proc out))
       (display ")")))])



(define (mutable-match-lambda-append . args)
  (define (proc->procs proc)
    (cond [(mutable-match-lambda-procedure? proc)
           (append*
            (map proc->procs (mutable-match-lambda-procedure-procs proc)))]
          [else (list proc)]))
  (define procs
    (append*
     (map proc->procs args)))
  (mutable-match-lambda-procedure procs))

(define (mutable-match-lambda-copy f)
  (mutable-match-lambda-append f))


(define (mutable-match-lambda-add-clause-proc! proc . clause-procs)
  (set-mutable-match-lambda-procedure-procs! proc
                                             (append (mutable-match-lambda-procedure-procs proc)
                                                     clause-procs)))

(define (mutable-match-lambda-add-overriding-clause-proc! proc . clause-procs)
  (set-mutable-match-lambda-procedure-procs! proc
                                             (append clause-procs
                                                     (mutable-match-lambda-procedure-procs proc))))



