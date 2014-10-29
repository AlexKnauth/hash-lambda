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

(define (arity+keywords-guard arity required-kws allowed-kws _)
  (unless (procedure-arity? arity)
    (error 'arity+keywords "expected procedure-arity? for first argument, given ~v" arity))
  (unless ((listof keyword?) required-kws)
    (error 'arity+keywords
           "expcetd (listof keyword?) for second argument, given ~v"
           required-kws))
  (unless ((or/c (listof keyword?) #t #f) allowed-kws)
    (error 'arity+keywords
           "expcetd (or/c (listof keyword?) #f) for third argument, given ~v"
           required-kws))
  (define new-arity (normalize-arity arity))
  (define new-required-kws
    (sort required-kws keyword<?))
  (define new-allowed-kws
    (cond [(list? allowed-kws)
           (sort (remove-duplicates (append new-required-kws allowed-kws))
                 keyword<?)]
          [else #f]))
  (values new-arity new-required-kws new-allowed-kws))

(struct arity+keywords (arity required-kws allowed-kws) #:transparent
  #:guard arity+keywords-guard)

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
           (member required-kw kws))
         #t)))

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
                        (for*/list ([a1-kw (in-list a1.allowed-kws)]
                                    [a2-kw (in-list a2.allowed-kws)]
                                    #:when (equal? a1-kw a2-kw))
                          a1-kw)]))
               (cond [(for/and ([req-kw (in-list required-kws)])
                        (member req-kw allowed-kws))
                      (arity+keywords arity required-kws allowed-kws)]
                     [else
                      (arity+keywords '() required-kws allowed-kws)]))]
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (check-equal? (arity+keywords (list 1 (arity-at-least 3) 2) '() #t)
                (arity+keywords (arity-at-least 1) '() #f))
  (check-equal? (arity+keywords '(1) '(#:a #:b) '(#:c))
                (arity+keywords 1 '(#:a #:b) '(#:a #:b #:c)))
  
  (local [(define proc (make-keyword-procedure void))]
    (check-equal? (procedure-arity+keywords proc)
                  (arity+keywords (arity-at-least 0) '() #f))
    (check-equal? (procedure-arity+keywords (procedure-reduce-arity proc 5))
                  (arity+keywords 5 '() '()))
    (define proc-with-arity
      (procedure-reduce-arity+keywords
       proc
       (arity+keywords 3 '(#:kw #:other-kw) '(#:kw #:other-kw #:optional-kw))))
    (check-equal? (procedure-arity+keywords proc-with-arity)
                  (arity+keywords 3 '(#:kw #:other-kw) '(#:kw #:other-kw #:optional-kw)))
    (check-equal? (procedure-arity proc-with-arity) 3)
    (check-equal? (call-with-values (λ () (procedure-keywords proc-with-arity)) list)
                  (list '(#:kw #:other-kw) '(#:kw #:optional-kw #:other-kw))))
  
  (check-true  (arity+keywords-matches? (arity+keywords 0 '() '()) 0 '()))
  (check-false (arity+keywords-matches? (arity+keywords 0 '() '()) 1 '()))
  (check-false (arity+keywords-matches? (arity+keywords '() '() #f) 0 '()))
  (check-true  (arity+keywords-matches? (arity+keywords (list 2 (arity-at-least 5))
                                                        '() '())
                                        2 '()))
  (check-false (arity+keywords-matches? (arity+keywords (list 2 (arity-at-least 5))
                                                        '() '())
                                        3 '()))
  (check-true  (arity+keywords-matches? (arity+keywords (list 2 (arity-at-least 5))
                                                        '() '())
                                        5 '()))
  (check-true  (arity+keywords-matches? (arity+keywords 0 '(#:a #:b) '(#:a #:b #:c))
                                        0 '(#:a #:b)))
  (check-true  (arity+keywords-matches? (arity+keywords 0 '(#:a #:b) '(#:a #:b #:c))
                                        0 '(#:a #:b #:c)))
  (check-false (arity+keywords-matches? (arity+keywords 0 '(#:a #:b) '(#:a #:b #:c))
                                        0 '(#:a #:c)))
  (check-true  (arity+keywords-matches? (arity+keywords 0 '() #f)
                                        0 '(#:whatever)))
  
  (check-equal? (arity+keywords-combine/or) (arity+keywords '() '() '()))
  (check-equal? (arity+keywords-combine/or (arity+keywords '(4 9 16) '(#:a #:b) '(#:a #:b #:c)))
                (arity+keywords '(4 9 16) '(#:a #:b) '(#:a #:b #:c)))
  (check-equal? (arity+keywords-combine/or (arity+keywords 1 '(#:a)     '(#:a #:b #:c))
                                           (arity+keywords 2 '(#:a #:b) '(#:a #:b #:d)))
                (arity+keywords '(1 2) '(#:a) '(#:a #:b #:c #:d)))
  
  (check-equal? (arity+keywords-combine/and) (arity+keywords (arity-at-least 0) '() #f))
  (check-equal? (arity+keywords-combine/and (arity+keywords '(4 9 16) '(#:a #:b) '(#:a #:b #:c)))
                (arity+keywords '(4 9 16) '(#:a #:b) '(#:a #:b #:c)))
  (check-equal? (arity+keywords-combine/and (arity+keywords '(1 2) '(#:a) '(#:a #:b #:c #:d))
                                            (arity+keywords '(2 3) '(#:b) '(#:a #:b #:c #:e)))
                (arity+keywords 2 '(#:a #:b) '(#:a #:b #:c)))
  (check-match  (arity+keywords-combine/and (arity+keywords 0 '(#:a) #f)
                                            (arity+keywords 0 '() '()))
                (arity+keywords '() _ _))
  
  )


