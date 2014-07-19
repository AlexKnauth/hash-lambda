#lang racket/base (require (for-syntax racket/base))

(provide (struct-out mutable-match-lambda-procedure)
         make-mutable-match-lambda
         mutable-match-lambda-clause-append
         mutable-match-lambda-append
         mutable-match-lambda-add-clause!
         mutable-match-lambda-add-overriding-clause!
         mutable-match-lambda-add-clause-proc!
         mutable-match-lambda-add-overriding-clause-proc!
         (all-from-out mutable-match-lambda/make-clause-proc)
         )



(require racket/list
         (for-syntax
          syntax/parse
          (for-syntax racket/base)))
(require mutable-match-lambda/make-clause-proc
         keyword-lambda)

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


