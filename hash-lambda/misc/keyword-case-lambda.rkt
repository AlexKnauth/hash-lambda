#lang racket/base (require (for-syntax racket/base))

(provide keyword-case-lambda)

(require racket/local
         racket/math
         racket/match
         racket/contract
         (for-syntax syntax/parse))

(require "../hash-lambda.rkt"
         "keyword-lambda.rkt"
         "arity+keywords.rkt")

(module+ test
  (require rackunit))

(begin-for-syntax
  (define-syntax-class kw
    (pattern kw:keyword))
  (define-syntax-class kw-formals-maybe-defaults
    (pattern rest-id:id)
    (pattern (arg:arg-maybe-default ...))
    (pattern (arg:arg-maybe-default ... . rest-id:id)))
  (define-splicing-syntax-class arg-maybe-default
    #:attributes (arg kw)
    (pattern arg:id #:attr kw (datum->syntax #'arg #f))
    (pattern [arg:id default:expr] #:attr kw (datum->syntax #'arg #f))
    (pattern (~seq kw:kw arg:id))
    (pattern (~seq kw:kw [arg:id default:expr])))
  (define-splicing-syntax-class maybe-when
    #:attributes (condition)
    (pattern (~seq #:when condition:expr))
    (pattern (~seq #:unless unless-condition:expr)
             #:attr condition (datum->syntax #'unless-condition `(not ,#'unless-condition)))
    (pattern (~seq)
             #:attr condition (datum->syntax #'condition #t)))
  (define (kwfms-replace-defaults stx)
    (syntax-parse stx
      [rest-id:id #'rest-id]
      [() #'()]
      [(arg:id . other-args)
       (with-syntax ([parsed-other-args (kwfms-replace-defaults #'other-args)])
         #'(arg . parsed-other-args))]
      [([arg:id default:expr] . other-args)
       (with-syntax ([parsed-other-args (kwfms-replace-defaults #'other-args)])
         #'([arg #f] . parsed-other-args))]
      [(kw:kw arg:id . other-args)
       (with-syntax ([parsed-other-args (kwfms-replace-defaults #'other-args)])
         #'(kw arg . parsed-other-args))]
      [(kw:kw [arg:id default:expr] . other-args)
       (with-syntax ([parsed-other-args (kwfms-replace-defaults #'other-args)])
         #'(kw [arg #f] . parsed-other-args))]
      ))
  )











(define-syntax keyword-case-lambda
  (lambda (stx)
    (syntax-parse stx
                  [(keyword-case-lambda) #'(case-lambda)]
                  [(keyword-case-lambda [(~or (~literal _) (~literal else)) maybe-when:maybe-when body:expr ...+] clause ...)
                   #'(local [(define else-proc (keyword-case-lambda clause ...))]
                       (keyword-lambda (kws kw-args . rest-args)
                                       (cond [maybe-when.condition body ...]
                                             [else (keyword-apply else-proc
                                                                  kws kw-args rest-args)])))]
                  [(keyword-case-lambda [#:when condition:expr body:expr ...+] clause ...)
                   #'(local [(define else-proc (keyword-case-lambda clause ...))]
                       (keyword-lambda (kws kw-args . rest-args)
                                       (cond [condition body ...]
                                             [else (keyword-apply else-proc
                                                                  kws kw-args rest-args)])))]
                  [(keyword-case-lambda [#:unless condition:expr body:expr ...+] clause ...)
                   #'(keyword-case-lambda [#:when (not condition) body ...] clause ...)]
                  [(keyword-case-lambda [#:args-hash args-hash:expr maybe-when:maybe-when body:expr ...+] clause ...)
                   #'(local [(define else-proc (keyword-case-lambda clause ...))]
                       (hash-lambda/match
                        [args-hash #:when maybe-when.condition body ...]
                        [args-hash_0
                         (apply/hash else-proc args-hash_0)]))]
                  [(keyword-case-lambda [#:kws kws-pat:expr #:kw-args kw-args-pat:expr #:rest rest-args-pat:expr maybe-when:maybe-when body:expr ...+] clause ...)
                   #'(local [(define else-proc (keyword-case-lambda clause ...))]
                       (keyword-lambda (kws kw-args . rest-args)
                                       (match (list kws kw-args rest-args)
                                         [(list kws-pat kw-args-pat rest-args-pat)
                                          #:when maybe-when.condition
                                          body ...]
                                         [_ (keyword-apply else-proc
                                                           kws kw-args rest-args)])))]
                  [(keyword-case-lambda [#:rest args-list:expr maybe-when:maybe-when body:expr ...+] clause ...)
                   #'(keyword-case-lambda [#:kws '() #:kw-args '() #:rest args-list #:when maybe-when.condition body ...] clause ...)]
                  [(keyword-case-lambda [kwfs:kw-formals-maybe-defaults maybe-when:maybe-when body:expr ...+] clause ...)
                   (with-syntax ([kwfms-with-same-arity (kwfms-replace-defaults #'kwfs)])
                     #'(local [(define proc-with-same-arity
                                 (lambda kwfms-with-same-arity #f))
                               (define else-proc (keyword-case-lambda clause ...))
                               (define first-arity (procedure-arity+keywords proc-with-same-arity))
                               (define else-arity (procedure-arity+keywords else-proc))]
                         (procedure-reduce-arity+keywords
                          (keyword-lambda (kws kw-args . rest-args)
                                          (cond [(arity+keywords-matches? first-arity (length rest-args) kws)
                                                 (keyword-apply (lambda kwfs
                                                                  (cond [maybe-when.condition body ...]
                                                                        [else (keyword-apply (keyword-case-lambda clause ...)
                                                                                             kws kw-args rest-args)]))
                                                                kws kw-args rest-args)]
                                                [else (keyword-apply (keyword-case-lambda clause ...)
                                                                     kws kw-args rest-args)]))
                          (arity+keywords-combine first-arity else-arity))))]
                  )))










(module+ test
  (local []
    (define f
      (keyword-case-lambda))
    (check-equal? (procedure-arity f) '())
    )
  (local []
    (define f
      (keyword-case-lambda
       [() 0]
       [(x) 1]
       [(x y) 2]
       [args (length args)]))
    (check-equal? (f) 0)
    (check-equal? (f 1) 1)
    (check-equal? (f 1 2) 2)
    (check-equal? (f 1 2 3 4 5 6) 6)
    )
  (local []
    (define pythag
      (keyword-case-lambda
       [(#:a a #:b b) (sqrt (+ (sqr a) (sqr b)))]
       [(#:c c #:a a) (sqrt (- (sqr c) (sqr a)))]
       [(#:c c #:b b) (sqrt (- (sqr c) (sqr b)))]
       [(#:a a #:b b #:c c) (= (+ (sqr a) (sqr b)) (sqr c))]))
    (check-equal? (pythag #:a 3 #:b 4) 5)
    (check-equal? (pythag #:c 5 #:a 3) 4)
    (check-equal? (pythag #:c 5 #:b 4) 3)
    (check-true (pythag #:a 3 #:b 4 #:c 5))
    (check-false (pythag #:a 3 #:b 4 #:c 6))
    (check-equal? (procedure-arity pythag) 0)
    (check-equal? (call-with-values (λ () (procedure-keywords pythag)) list)
                  (list '() '(#:a #:b #:c)))
    (check-equal? (call-with-values (λ () (procedure-keywords (keyword-case-lambda
                                                               [(#:a a #:b b) #t]
                                                               [(#:a a #:not-b not-b) #t])))
                                    list)
                  (list '(#:a) '(#:a #:b #:not-b)))
    )
  (local []
    (define f
      (keyword-case-lambda
       [#:args-hash args-hash
                    args-hash]))
    (check-match (f 0 1 2 #:kw "kw-arg")
                 (hash-table [0 0] [1 1] [2 2] ['#:kw "kw-arg"]))
    )
  (local []
    (define f
      (keyword-case-lambda
       [#:args-hash (hash-table ['#:m (? number? m)] ['#:v (? number? v)]) `(m: ,m v: ,v)]
       [(#:mass [m 0] #:velocity [v 0]) #:when (andmap number? (list m v)) `(mass: ,m velocity: ,v)]
       [#:rest `(m: ,m v: ,v) #:when (andmap number? (list m v)) `(m: ,m v: ,v)]))
    (check-equal? (f #:m 2 #:v 1) '(m: 2 v: 1))
    (check-equal? (f #:mass 2) '(mass: 2 velocity: 0))
    (check-equal? (f #:mass 2 #:velocity 1) '(mass: 2 velocity: 1))
    (check-equal? (f) '(mass: 0 velocity: 0))
    (check-equal? (f 'm: 2 'v: 1) '(m: 2 v: 1))
    )
  (local []
    (define f
      (keyword-case-lambda
       [(a b #:d d [c #f] #:e [e #f] . rest)
        (list a b c d e rest)]
       [(a [b #f] #:d [d #f] [c #f] #:e [e #f] . rest)
        #:when ((and/c number? zero?) a)
        (list 'zero: a b c d e rest)]
       [(a [b #f] #:d [d #f] [c #f] #:e [e #f] . rest)
        (list a b c d e rest)]
       [#:args-hash
        args-hash
        args-hash]))
    (check-equal? (f "a" "b" "c" #:d "d" #:e "e" "other-arg 1" "other-arg 2")
                  (list "a" "b" "c" "d" "e" (list "other-arg 1" "other-arg 2")))
    (check-equal? (f "a")
                  (list "a" #f #f #f #f '()))
    (check-equal? (f 0)
                  (list 'zero: 0 #f #f #f #f '()))
    )
  )