#lang racket/base

(provide (struct-out arity+keywords)
         procedure-arity+keywords
         arity+keywords-combine
         procedure-reduce-arity+keywords
         arity+keywords-matches?
         procedure-arity+keywords-matches?/c
         )

(require racket/match
         racket/list
         racket/function
         racket/bool
         racket/contract)

(struct arity+keywords (arity required-kws allowed-kws) #:transparent)

(define (procedure-arity+keywords proc)
  (define arity (procedure-arity proc))
  (define-values (req-kws allowed-kws)
    (procedure-keywords proc))
  (arity+keywords arity req-kws allowed-kws))

(define arity+keywords-combine
  (case-lambda
    [() '()]
    [(a) a]
    [(a1 a2) (match-define
               (arity+keywords a1.arity a1.required-kws a1.allowed-kws)
               a1)
             (match-define
               (arity+keywords a2.arity a2.required-kws a2.allowed-kws)
               a2)
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
                (arity+keywords arity required-kws allowed-kws)])]
    [(a1 . rest-args) (arity+keywords-combine a1 (apply arity+keywords-combine rest-args))]
    ))

(define (procedure-reduce-arity+keywords proc a)
  (procedure-reduce-keyword-arity
   proc
   (arity+keywords-arity a)
   (sort (arity+keywords-required-kws a) keyword<?)
   (and (arity+keywords-allowed-kws a)
        (sort (arity+keywords-allowed-kws a) keyword<?))))

(define (arity+keywords-matches? arity+kws n kws)
  (match-define (arity+keywords arity required-kws allowed-kws) arity+kws)
  (and (arity-includes? arity n)
       (or (false? allowed-kws)
           (for/and ([kw (in-list kws)])
             (member kw allowed-kws)))
       (for/and ([required-kw (in-list required-kws)])
         (member required-kw kws))))

(define (procedure-arity+keywords-matches?/c n kws)
  (flat-named-contract
   `(procedure-arity+keywords-matches?/c ,n (quote ,kws))
   (lambda (proc)
     (let ([proc.arity+keywords (procedure-arity+keywords proc)])
       (arity+keywords-matches? proc.arity+keywords n kws)))))

