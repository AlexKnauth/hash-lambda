#lang racket/base

(provide make-clause-proc
         clause->proc
         case-lambda-clause->proc
         hash-lambda/match-clause->proc
         match-lambda-clause->proc
         match-lambda*-clause->proc
         )

(require racket/match
         hash-lambda
         (only-in "communication.rkt" mutable-match-lambda-next)
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     mutable-match-lambda/syntax-to-string
                     ))

(define (make-clause-proc test proc)
  (procedure-rename
   (keyword-lambda (kws kw-args . rest-args)
     (cond [(and (arity+keywords-matches? (procedure-arity+keywords test) (length rest-args) kws)
                 (keyword-apply test kws kw-args rest-args))
            (keyword-apply proc kws kw-args rest-args)]
           [else
            (mutable-match-lambda-next)]))
   (string->symbol (format "(make-clause-proc ~v ~v)" test proc))))

(define-syntax clause->proc
  (lambda (stx)
    (let* ([str (syntax->string stx)]
           [sym (string->symbol str)])
      (with-syntax ([name (datum->syntax stx `(quote ,sym))])
        (syntax-parse stx
          [(clause->proc kw:keyword clause:expr)
           (define kw-str (keyword->string (syntax-e #'kw)))
           (with-syntax ([kw-id-clause->proc (format-id #'kw "~a-clause->proc" kw-str #:source #'kw)])
             #'(procedure-rename (kw-id-clause->proc clause) name))]
          )))))

(define-syntax-rule (case-lambda-clause->proc clause)
  (case-lambda clause [_ (mutable-match-lambda-next)]))

(define-syntax-rule (hash-lambda/match-clause->proc clause)
  (hash-lambda/match clause [_ (mutable-match-lambda-next)]))

(define-syntax-rule (match-lambda-clause->proc clause)
  (match-lambda clause [_ (mutable-match-lambda-next)]))

(define-syntax-rule (match-lambda*-clause->proc clause)
  (match-lambda* clause [_ (mutable-match-lambda-next)]))

