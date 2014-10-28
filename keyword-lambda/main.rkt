#lang racket/base

(provide keyword-lambda
         keyword-apply/sort
         procedure-reduce-keyword-arity/sort
         (struct-out arity+keywords)
         procedure-arity+keywords
         procedure-reduce-arity+keywords
         arity+keywords-matches?
         procedure-arity+keywords-matches?
         procedure-arity+keywords-matches?/c
         arity+keywords-combine)

(require racket/function
         racket/bool
         racket/contract
         racket/list
         (for-syntax racket/base syntax/parse syntax/name))

(module+ test
  (require rackunit racket/local racket/math))

;; (keyword-lambda (kws kw-args . rest-args) body ...+)
(define-syntax keyword-lambda
  (lambda (stx)
    (syntax-parse stx
      [(keyword-lambda (kws:id kw-args:id . rest-args) body:expr ...+)
       (define name (syntax-local-infer-name stx))
       (cond [(or (symbol? name) (identifier? name))
              (with-syntax ([name name])
                #'(make-keyword-procedure
                   (lambda (kws kw-args . rest-args) body ...)
                   (let ([name (lambda rest-args
                                 (let ([kws '()] [kw-args '()])
                                   body ...))])
                     name)))]
             [else #'(make-keyword-procedure
                        (lambda (kws kw-args . rest-args) body ...)
                        (lambda rest-args
                          (let ([kws '()] [kw-args '()])
                            body ...)))])])))

(module+ test
  (local [(define proc
            (keyword-lambda (kws kw-args . rest-args)
              (list kws kw-args rest-args)))]
    (check-equal? (proc #:a 'a #:b 'b 0 1 2)
                  (list '(#:a #:b) '(a b) '(0 1 2)))
    (check-equal? (object-name proc) 'proc)
    ))

;; like keyword-apply, but without the constraint that the kws must be sorted
(define keyword-apply/sort
  (keyword-lambda (kws kw-args f other-kws other-kw-args . rest-args)
    (let* ([kw-lop (for/list ([kw     (in-list (append kws     other-kws))]
                              [kw-arg (in-list (append kw-args other-kw-args))])
                     (cons kw kw-arg))]
           [sorted-kw-lop (sort kw-lop keyword<? #:key car)]
           [sorted-kws     (map car sorted-kw-lop)]
           [sorted-kw-args (map cdr sorted-kw-lop)])
      (keyword-apply f sorted-kws sorted-kw-args (apply list* rest-args)))))

(module+ test
  (local []
    (define (kinetic-energy #:mass m #:velocity v)
      (* 1/2 m (sqr v)))
    (check-equal? (keyword-apply/sort kinetic-energy '(#:mass #:velocity) '(2 1) '())
                  1)
    (check-equal? (keyword-apply/sort kinetic-energy '(#:velocity #:mass) '(1 2) '())
                  1)
    ))

;; like procedure-reduce-keyword-arity, but without the constraint that the kws must be sorted
(define (procedure-reduce-keyword-arity/sort proc arity required-kws allowed-kws)
  (procedure-reduce-keyword-arity
   proc
   arity
   (sort required-kws keyword<?)
   (and allowed-kws
        (sort allowed-kws keyword<?))))

(struct arity+keywords (arity required-kws allowed-kws) #:transparent
  #:guard (lambda (arity required-kws allowed-kws _)
            (define new-required-kws
              (sort required-kws keyword<?))
            (define new-allowed-kws
              (cond [(list? allowed-kws)
                     (sort (remove-duplicates (append new-required-kws allowed-kws))
                           keyword<?)]
                    [else #f]))
            (values arity new-required-kws new-allowed-kws)))

(define (procedure-arity+keywords proc)
  (define arity (procedure-arity proc))
  (define-values (req-kws allowed-kws)
    (procedure-keywords proc))
  (arity+keywords arity req-kws allowed-kws))

(define (procedure-reduce-arity+keywords proc a)
  (procedure-reduce-keyword-arity
   proc
   (arity+keywords-arity a)
   (arity+keywords-required-kws a)
   (arity+keywords-allowed-kws a)))

(define (arity+keywords-matches? arity+kws n kws)
  (let ([arity (arity+keywords-arity arity+kws)]
        [required-kws (arity+keywords-required-kws arity+kws)]
        [allowed-kws (arity+keywords-allowed-kws arity+kws)])
    (and (arity-includes? arity n)
         (or (false? allowed-kws)
             (for/and ([kw (in-list kws)])
               (member kw allowed-kws)))
         (for/and ([required-kw (in-list required-kws)])
           (member required-kw kws)))))

(define (procedure-arity+keywords-matches? proc n kws)
  (arity+keywords-matches? (procedure-arity+keywords proc) n kws))

(define (procedure-arity+keywords-matches?/c n kws)
  (flat-named-contract
   `(procedure-arity+keywords-matches?/c ,n (quote ,kws))
   (lambda (proc)
     (procedure-arity+keywords-matches? proc n kws))))

(define arity+keywords-combine
  (case-lambda
    [() (arity+keywords '() '() '())]
    [(a) a]
    [(a1 a2) (let ([a1.arity (arity+keywords-arity a1)]
                   [a1.required-kws (arity+keywords-required-kws a1)]
                   [a1.allowed-kws (arity+keywords-allowed-kws a1)]
                   [a2.arity (arity+keywords-arity a2)]
                   [a2.required-kws (arity+keywords-required-kws a2)]
                   [a2.allowed-kws (arity+keywords-allowed-kws a2)])
               (cond
                 [(andmap empty? (list a1.arity a2.arity)) (arity+keywords '() '() '())]
                 [(empty? a1.arity) a2]
                 [(empty? a2.arity) a1]
                 [else
                  (define arity
                    (normalize-arity (flatten (list a1.arity a2.arity))))
                  (define required-kws
                    (for*/list ([a1-kw (in-list a1.required-kws)]
                                [a2-kw (in-list a2.required-kws)]
                                #:when (equal? a1-kw a2-kw))
                      a1-kw))
                  (define allowed-kws
                    (and a1.allowed-kws a2.allowed-kws
                         (remove-duplicates
                          (append a1.allowed-kws
                                  a2.allowed-kws))))
                  (arity+keywords arity required-kws allowed-kws)]))]
    [(a1 . rest-args) (arity+keywords-combine a1 (apply arity+keywords-combine rest-args))]
    ))