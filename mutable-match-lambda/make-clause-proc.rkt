#lang racket/base

(provide make-clause-proc
         clause->proc
         clause->proc/case-lambda
         clause->proc/hash-lambda/match
         clause->proc/match-lambda
         clause->proc/match-lambda*
         )

(require racket/match
         keyword-lambda/keyword-lambda
         keyword-lambda/arity+keywords
         (only-in hash-lambda hash-lambda/match)
         (only-in "communication.rkt" mutable-match-lambda-next)
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     mutable-match-lambda/syntax-to-string
                     ))

(define (make-clause-proc test proc)
  (define test.arity+kws (procedure-arity+keywords test))
  (define proc.arity+kws (procedure-arity+keywords proc))
  (procedure-reduce-arity+keywords
   (procedure-rename
    (keyword-lambda (kws kw-args . rest-args)
      (define n (length rest-args))
      (cond [(and (arity+keywords-matches? test.arity+kws n kws)
                  (arity+keywords-matches? proc.arity+kws n kws)
                  (keyword-apply test kws kw-args rest-args))
             (keyword-apply proc kws kw-args rest-args)]
            [else
             (mutable-match-lambda-next)]))
    (string->symbol (format "(make-clause-proc ~v ~v)" test proc)))
   (arity+keywords-combine/and test.arity+kws proc.arity+kws)))

(define-syntax clause->proc
  (lambda (stx)
    (let* ([str (syntax->string stx)]
           [sym (string->symbol str)])
      (with-syntax ([name (datum->syntax stx `(quote ,sym))])
        (syntax-parse stx
          [(clause->proc kw:keyword clause:expr)
           (define kw-str (keyword->string (syntax-e #'kw)))
           (with-syntax ([clause->proc/kw-id (format-id #'kw "clause->proc/~a" kw-str #:source #'kw)])
             #'(procedure-rename (clause->proc/kw-id clause) name))]
          )))))

(define-syntax-rule (clause->proc/case-lambda clause)
  (case-lambda clause [_ (mutable-match-lambda-next)]))

(define-syntax-rule (clause->proc/hash-lambda/match clause)
  (hash-lambda/match clause [_ (mutable-match-lambda-next)]))

(define-syntax-rule (clause->proc/match-lambda clause)
  (match-lambda clause [_ (mutable-match-lambda-next)]))

(define-syntax-rule (clause->proc/match-lambda* clause)
  (match-lambda* clause [_ (mutable-match-lambda-next)]))

