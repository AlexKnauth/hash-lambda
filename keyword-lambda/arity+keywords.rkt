#lang racket/base

(provide (struct-out arity+keywords)
         procedure-arity+keywords
         procedure-reduce-arity+keywords
         procedure-reduce-keyword-arity/sort
         arity+keywords-matches?
         procedure-arity+keywords-matches?
         procedure-arity+keywords-matches?/c
         arity+keywords-combine/or
         arity+keywords-combine/and
         arity+keywords-combine
         )

(require racket/function
         racket/bool
         racket/contract/base
         racket/list
         )

(module+ test
  (require rackunit racket/local racket/math))

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

;; like procedure-reduce-keyword-arity, but without the constraint that the kws must be sorted
(define (procedure-reduce-keyword-arity/sort proc arity required-kws allowed-kws)
  (procedure-reduce-arity+keywords
   proc
   (arity+keywords arity required-kws allowed-kws)))

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

(define arity+keywords-combine/or
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
    [(a1 . rest-args) (arity+keywords-combine/or a1 (apply arity+keywords-combine/or rest-args))]
    ))

(define arity+keywords-combine arity+keywords-combine/or)

(define arity+keywords-combine/and
  (case-lambda
    [() (arity+keywords (arity-at-least 0) '() #f)]
    [(a) a]
    [(a1 a2) (let ([a1.arity (arity+keywords-arity a1)]
                   [a1.required-kws (arity+keywords-required-kws a1)]
                   [a1.allowed-kws (arity+keywords-allowed-kws a1)]
                   [a2.arity (arity+keywords-arity a2)]
                   [a2.required-kws (arity+keywords-required-kws a2)]
                   [a2.allowed-kws (arity+keywords-allowed-kws a2)])
               (define arity
                 (arity-combine/and a1.arity a2.arity))
               (define required-kws
                 (remove-duplicates
                  (append a1.required-kws
                          a2.required-kws)))
               (define allowed-kws
                 (cond [(not (list? a1.allowed-kws)) a2.allowed-kws]
                       [(not (list? a2.allowed-kws)) a1.allowed-kws]
                       [else
                        (for*/list ([a1-kw (in-list a1.required-kws)]
                                    [a2-kw (in-list a2.required-kws)]
                                    #:when (equal? a1-kw a2-kw))
                          a1-kw)]))
               (arity+keywords arity required-kws allowed-kws))
             ]
    [(a1 . rest-args) (arity+keywords-combine/and a1 (apply arity+keywords-combine/and rest-args))]
    ))

(define (arity-combine/and a1 a2)
  (let ([a1 (normalize-arity a1)]
        [a2 (normalize-arity a2)])
    (cond [(arity-includes? a1 a2) a2]
          [(arity-includes? a2 a1) a1]
          [(number? a1)
           (cond [(arity-includes? a2 a1) a1]
                 [else '()])]
          [(number? a2)
           (cond [(arity-includes? a1 a2) a2]
                 [else '()])]
          [(arity-at-least? a1)
           (cond [(arity-includes? a2 a1) a1]
                 [(number? a2) '()]
                 [(arity-at-least? a2)
                  (arity-at-least (max (arity-at-least-value a1)
                                       (arity-at-least-value a2)))]
                 [(list? a2)
                  (normalize-arity
                   (flatten
                    (for/list ([n (in-list a2)])
                      (arity-combine/and a1 n))))]
                 [else (error 'arity-combine/and "this should never happen")])]
          [(arity-at-least? a2)
           (cond [(arity-includes? a1 a2) a2]
                 [(number? a1) '()]
                 [(arity-at-least? a1)
                  (arity-at-least (max (arity-at-least-value a1)
                                       (arity-at-least-value a2)))]
                 [(list? a1)
                  (normalize-arity
                   (flatten
                    (for/list ([n (in-list a1)])
                      (arity-combine/and a2 n))))]
                 [else (error 'arity-combine/and "this should never happen")])]
          [(list? a1)
           (normalize-arity
            (flatten
             (for/list ([n (in-list a1)])
               (arity-combine/and a2 n))))]
          [(list? a2)
           (normalize-arity
            (flatten
             (for/list ([n (in-list a2)])
               (arity-combine/and a1 n))))]
          [else (error 'arity-combine/and "this should never happen")])))
