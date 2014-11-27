#lang racket/base

(provide (struct-out mutable-match-lambda-procedure)
         make-mutable-match-lambda
         mutable-match-lambda-append
         mutable-match-lambda-copy
         mutable-match-lambda-add-clause-proc!
         mutable-match-lambda-add-overriding-clause-proc!
         )

(require racket/list
         racket/match
         racket/format
         kw-utils/keyword-lambda
         (only-in "communication.rkt" mutable-match-lambda-clause-append)
         (for-syntax racket/base
                     syntax/parse
                     (for-syntax racket/base)))

(begin-for-syntax
  (define-syntax kw (make-rename-transformer #'keyword)))

(define (make-mutable-match-lambda #:name [name #f] . procs)
  (mutable-match-lambda-procedure name procs))

(struct mutable-match-lambda-procedure (name procs)
  #:transparent #:mutable
  ;#:property prop:object-name ;if this gets added to racket (https://github.com/plt/racket/pull/729)
  ;(lambda (this) (mutable-match-lambda-procedure-name this))
  #:property prop:procedure
  (keyword-lambda (kws kw-args this . args)
    (match-define (mutable-match-lambda-procedure name procs) this)
    (define proc (apply mutable-match-lambda-clause-append procs #:name name))
    (keyword-apply proc kws kw-args args))
  #:methods gen:custom-write
  [(define (write-proc this out mode)
     (match-define (mutable-match-lambda-procedure name procs) this)
     (cond [name (fprintf out "#<procedure:~a>" name)]
           [else (display "(make-mutable-match-lambda" out)
                 (for ([proc (in-list procs)])
                   (fprintf out " ~v" proc))
                 (display ")" out)]))])



(define (mutable-match-lambda-append #:name [name #f] . args)
  (define (proc->procs proc)
    (cond [(mutable-match-lambda-procedure? proc)
           (append*
            (map proc->procs (mutable-match-lambda-procedure-procs proc)))]
          [else (list proc)]))
  (define procs
    (append*
     (map proc->procs args)))
  (mutable-match-lambda-procedure name procs))

(define (mutable-match-lambda-copy f)
  (mutable-match-lambda-append f #:name (mutable-match-lambda-procedure-name f)))


(define (mutable-match-lambda-add-clause-proc! proc . clause-procs)
  (set-mutable-match-lambda-procedure-procs! proc
                                             (append (mutable-match-lambda-procedure-procs proc)
                                                     clause-procs)))

(define (mutable-match-lambda-add-overriding-clause-proc! proc . clause-procs)
  (set-mutable-match-lambda-procedure-procs! proc
                                             (append clause-procs
                                                     (mutable-match-lambda-procedure-procs proc))))



