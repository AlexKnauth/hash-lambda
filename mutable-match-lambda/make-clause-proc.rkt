#lang racket/base

(provide make-clause-proc
         clause->proc
         )

(require racket/match
         hash-lambda
         "communication.rkt"
         (for-syntax racket/base
                     mutable-match-lambda/syntax-to-string))

(define (make-clause-proc test proc)
  (procedure-rename
   (keyword-lambda (kws kw-args . rest-args)
     (cond [(and (arity+keywords-matches? (procedure-arity+keywords test) (length rest-args) kws)
                 (keyword-apply test kws kw-args rest-args))
            (keyword-apply proc kws kw-args rest-args)]
           [else
            (raise-mutable-match-lambda:no-match-error (keyword-apply make-args-hash kws kw-args rest-args))]))
   (string->symbol (format "(make-clause-proc ~v ~v)" test proc))))

(define-syntax clause->proc
  (lambda (stx)
    (let* ([str (syntax->string stx)]
           [sym (string->symbol str)])
      (with-syntax ([name (datum->syntax stx `(quote ,sym))])
        (syntax-case stx ()
          [(clause->proc #:case-lambda clause)       #'(procedure-rename (case-lambda-clause->proc clause) name)]
          [(clause->proc #:hash-lambda/match clause) #'(procedure-rename (hash-lambda/match-clause->proc clause) name)]
          [(clause->proc #:match-lambda clause)      #'(procedure-rename (match-lambda-clause->proc clause) name)]
          [(caluse->proc #:match-lambda* clause)     #'(procedure-rename (match-lambda*-clause->proc clause) name)]
          )))))

(define-syntax-rule (case-lambda-clause->proc clause)
  (case-lambda clause [args (raise-mutable-match-lambda:no-match-error (apply make-args-hash args))]))

(define-syntax-rule (hash-lambda/match-clause->proc clause)
  (hash-lambda/match clause [args-hash (raise-mutable-match-lambda:no-match-error args-hash)]))

(define-syntax-rule (match-lambda-clause->proc clause)
  (match-lambda clause [arg (raise-mutable-match-lambda:no-match-error (make-args-hash arg))]))

(define-syntax-rule (match-lambda*-clause->proc clause)
  (match-lambda* clause [args (raise-mutable-match-lambda:no-match-error (apply make-args-hash args))]))

