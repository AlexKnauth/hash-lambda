#lang racket/base

(provide hash-lambda
         hash-lambda/match
         apply/hash
         args-hash?
         args-hash->args-list
         make-args-hash
         keyword-lambda
         args-hash-first
         args-hash-rest
         args-hash-cons
         args-hash-cons*
         args-hash-append
         keyword-case-lambda
         keyword-apply/sort
         hash-has-key?/c
         match?/c
         make-hash-lambda-contract
         (struct-out arity+keywords)
         procedure-arity+keywords
         arity+keywords-combine
         procedure-reduce-arity+keywords
         arity+keywords-matches?
         )

(require racket/match
         racket/format
         racket/local
         racket/contract
         racket/list
         racket/function
         racket/bool
         racket/math)

(require (for-syntax racket/base syntax/parse racket/list))

(module+ test
  (require rackunit))



(require "keyword-lambda.rkt")



(begin-for-syntax
  (define-syntax-class kw
    (pattern kw:keyword))
  )



;; a helper function for hash-lambda
(define-for-syntax (id->string id)
  (symbol->string (syntax->datum id)))

;; (hash-lambda args-hash-id body ...+)
;; (hash-lambda [args-hash-id args-hash-contract] body ...+)
;; 
;; like (lambda args-list-id body ...+), except that it takes all keywords, and it puts its arguments into a hash table instead of a list.
;; In the hash table, the by-position arguments have their position as the key, and the keyword arguments have the (quoted) keyword as the key.
;; The second form is like the first form except that it applies args-hash-contract to args-hash-id.
(define-syntax hash-lambda
  (lambda (stx)
    (syntax-parse stx
      [(hash-lambda args-hash:id body:expr ...+)
       ;#'(hash-lambda-procedure
        ;  (#%plain-lambda (args-hash)
         ;                 body ...))
       #'(keyword-lambda (kws kw-args . rest)
           (let ([args-hash (keyword-apply make-args-hash kws kw-args rest)])
             body ...))
       ]
      [(hash-lambda [args-hash:id args-hash-contract] body:expr ...+)
       #:declare args-hash-contract (expr/c #'contract? #:name (string-append "contract for "(id->string #'args-hash)""))
       #'(local [(define/contract |#<procedure>|
                   (make-hash-lambda-contract args-hash-contract 'any)
                   (hash-lambda args-hash
                     body ...))]
           |#<procedure>|)]
      )))

(struct hash-lambda-procedure (proc)
  #:property prop:procedure
  (procedure-reduce-keyword-arity
   (keyword-lambda (kws kw-args this . rest-args)
     (let ([this.proc (hash-lambda-procedure-proc this)])
       (this.proc (keyword-apply make-args-hash kws kw-args rest-args))))
   (arity-at-least 1)
   '() #f)
  #:transparent)

(define-syntax-rule (hash-lambda/match match-clause ...)
  (hash-lambda args-hash
    (match args-hash match-clause ...)))

(module+ test
  (local []
    (define return-args-hash
      (hash-lambda args-hash
        args-hash))
    (check-match (return-args-hash "0" "1" #:keyword "keyword-argument" "2")
                 (hash-table [0 "0"] [1 "1"] [2 "2"] ['#:keyword "keyword-argument"]))
    (define my+
      (hash-lambda args-hash
        (+ (hash-ref args-hash 0)
           (hash-ref args-hash 1))))
    (check-equal? (my+ 1 2)
                  3)
    (define/contract kinetic-energy (#:mass number? #:velocity number? . -> . number?)
      (hash-lambda args-hash
        (let ([mass     (hash-ref args-hash '#:mass)]
              [velocity (hash-ref args-hash '#:velocity)])
          (* 1/2 mass (sqr velocity)))))
    (check-equal? (kinetic-energy #:mass 2 #:velocity 1)
                  1)
    )
  (local []
    (define my+
      (hash-lambda/match
       [(hash-table [0 a] [1 b])
        (+ a b)]))
    (check-equal? (my+ 1 2)
                  3)
    (define/contract kinetic-energy (#:mass number? #:velocity number? . -> . number?)
      (hash-lambda/match
       [(hash-table ['#:mass mass] ['#:velocity velocity])
        (* 1/2 mass (sqr velocity))]))
    (check-equal? (kinetic-energy #:mass 2 #:velocity 1)
                  1)
    )
  )

;; (apply/hash proc args-hash #:<kw> kw-arg ...)
;; like apply, but accepts an args-hash instead of a list
(define apply/hash
  (procedure-reduce-keyword-arity
   (hash-lambda args
                (match args 
                  [(hash-table [0 (? procedure? f)] [1 (? args-hash? hash)])
                   (let* ([kw-lop (for/list ([key (in-hash-keys hash)]
                                             [val (in-hash-values hash)]
                                             #:when (keyword? key))
                                    (cons key val))]
                          [sorted-kw-lop (sort kw-lop keyword<? #:key car)]
                          [kws     (map car sorted-kw-lop)]
                          [kw-args (map cdr sorted-kw-lop)])
                     (let* ([rest-lop (for/list ([key (in-hash-keys hash)]
                                                 [val (in-hash-values hash)]
                                                 #:when (number? key))
                                        (cons key val))]
                            [sorted-rest-lop (sort rest-lop < #:key car)]
                            [rest-args (map cdr sorted-rest-lop)])
                       (keyword-apply f kws kw-args rest-args)))]
                  [(hash-table [0 (? procedure? f)] [1 (? args-hash? hash)]
                               [(? keyword? kws) kw-args] ...)
                   (apply/hash f
                               (make-hash
                                (append (for/list ([kw     (in-list kws)]
                                                   [kw-arg (in-list kw-args)])
                                          (cons kw kw-arg))
                                        (hash->list hash))))]
                  [_ (find-|apply/hash|-error args)]
                  ))
   2
   '()
   #f))

(define (find-|apply/hash|-error args)
  (unless (args-hash? args)
    (error "apply/hash: args is not an args-hash, given:" args))
  (match args
    [(hash-table [0 f] [other-keys other-vals] ...)
     (unless (procedure? f)
       (raise-argument-error 'apply/hash "procedure?" f))]
    [_ (error "apply/hash: args does not have a key of 0, given:" args)])
  (match args
    [(hash-table [0 f] [1 hash] [other-keys other-vals] ...)
     (unless (args-hash? hash)
       (raise-argument-error 'apply/hash "args-hash?" hash))
     (for ([key (in-list other-keys)])
       (unless (keyword? key)
         (error
          (string-append
           "apply/hash: the keywords in args are not all keywords," "\n"
           "  given: "(~v key)" in: "(~v args)"" "\n"
           "  (probably hash-lambda's fault)"))))]
    [_ (error "apply/hash: args does not have a key of 1, given:" args)])
  (error "I don't know"))

(module+ test
  (local []
    (check-equal? (apply/hash list (hash 0 "0" 1 "1"))
                  (list "0" "1"))
    (check-equal? (apply/hash list (hash 1 "1" 0 "0"))
                  (list "0" "1"))
    (define (kinetic-energy #:mass m #:velocity v)
      (* 1/2 m (sqr v)))
    (check-equal? (apply/hash kinetic-energy (hash '#:mass 2 '#:velocity 1))
                  1)
    ))



(define (args-hash? x)
  (and (hash? x)
       (for/and ([key (in-hash-keys x)])
         (or (exact-nonnegative-integer? key)
             (keyword? key)))
       (local [(define number-keys
                 (for/list ([key (in-hash-keys x)] #:when (number? key))
                   key))
               (define sorted-number-keys
                 (sort number-keys <))]
         (for/and ([k (in-list sorted-number-keys)]
                   [n (in-naturals)])
           (equal? k n)))
       ))

;; (args-hash->args-list x)
;; Any -> (or/c list? #f)
;; like (apply/hash list x), but if x isn't an args-hash or has any keywords, 
;; it returns false instead of raising an exeption
(define (args-hash->args-list x)
  (cond [(not (hash? x)) #false]
        [#true
         (define lop (hash->list x))
         (cond
           [(not (for/and ([pair (in-list lop)])
                   (exact-nonnegative-integer? (car pair))))
            #false]
           [else
            (define sorted-lop (sort lop < #:key car))
            (struct my-exn ())
            (with-handlers ([my-exn? (λ (e) #false)])
              (for/list ([pair (in-list sorted-lop)]
                         [n (in-naturals)])
                (let ([key (car pair)]
                      [val (cdr pair)])
                  (cond [(equal? key n)
                         val]
                        [else
                         (raise (my-exn))]))))])]))



(define/contract make-args-hash-function
  (unconstrained-domain-> args-hash?)
  (keyword-lambda (kws kw-args . rest)
    (make-hash
     (append (for/list ([kw     (in-list kws)]
                        [kw-arg (in-list kw-args)])
               (cons kw kw-arg))
             (for/list ([n (in-naturals)]
                        [arg (in-list rest)])
               (cons n arg))))))

(module+ test
  (check-match (make-args-hash 1 2 3 #:kw-1 'kw-arg-1 #:kw-2 'kw-arg-2)
               (hash-table [0 1] [1 2] [2 3] ['#:kw-1 'kw-arg-1] ['#:kw-2 'kw-arg-2])))

(define-match-expander make-args-hash
  (lambda (stx) ; as a pat
    (syntax-parse stx
      [(make-args-hash)
       #'(hash-table)]
      [(make-args-hash x ...)
       #'(args-hash-cons* x ... (make-args-hash))]))
  (lambda (stx) ; as normal make-args-hash-function
    (syntax-parse stx
      [(make-args-hash stuff ...)
       #'(#%app make-args-hash stuff ...)]
      [make-args-hash
       #'make-args-hash-function])))

(module+ test
  (check-equal? (match (make-args-hash 1 2 3 #:kw-1 'kw-arg-1 #:kw-2 'kw-arg-2)
                  [(make-args-hash one two three #:kw-1 kw-arg-one #:kw-2 kw-arg-two)
                   (list one two three kw-arg-one kw-arg-two)])
                '(1 2 3 kw-arg-1 kw-arg-2)))



(define (hash-has-key?/c key)
  (flat-named-contract `(hash-has-key?/c (quote ,key))
                       (lambda (hash)
                         (and (hash? hash)
                              (hash-has-key? hash key)))))

(define/contract (args-hash-first args-hash) ((and/c args-hash? (hash-has-key?/c 0)) . -> . any/c)
  (hash-ref args-hash 0))

(define/contract (args-hash-rest args-hash) ((and/c args-hash? (hash-has-key?/c 0)) . -> . args-hash?)
  (when (not (immutable? args-hash))
    (set! args-hash (make-immutable-hash (hash->list args-hash))))
  (for/hash ([(key val) (in-hash (hash-remove args-hash 0))])
    (cond [(number? key)
           (values (sub1 key) val)]
          [else
           (values key val)])))

(module+ test
  (check-match (args-hash-rest (hash 0 "0" 1 "1" 2 "2" '#:kw "kw-arg"))
               (hash-table [0 "1"] [1 "2"] ['#:kw "kw-arg"]))
  )

(define args-hash-cons-function
  (procedure-rename
   (hash-lambda/match
     [(hash-table [(and (or 0 (? keyword?)) key) val]
                  [(or 0 1) args-hash])
      (hash-set (for/hash ([(key_0 val_0) (in-hash args-hash)])
                  (cond [(and (number? key_0) (equal? 0 key))
                         (values (add1 key_0) val_0)]
                        [else
                         (values key_0 val_0)]))
                key val)])
   'args-hash-cons))

(module+ test
  (check-match (args-hash-cons "thing" (hash 0 "0" 1 "1" 2 "2" '#:kw "kw-arg"))
               (hash-table [0 "thing"] [1 "0"] [2 "1"] [3 "2"] ['#:kw "kw-arg"]))
  )

(define-match-expander args-hash-cons
  (lambda (stx) ; as a pat
    (syntax-parse stx
      [(args-hash-cons val:expr args-hash:expr)
       #'(app (lambda (x)
                (cond [(not (hash-has-key? x 0)) #f]
                      [else
                       (cons (args-hash-first x)
                             (args-hash-rest x))]))
              (cons val args-hash))]
      [(args-hash-cons kw:kw val:expr args-hash:expr)
       #'(app (lambda (x)
                (cond [(not (hash-has-key? x 'kw)) #f]
                      [else
                       (cons (hash-ref x 'kw)
                             (hash-remove x 'kw))]))
              (cons val args-hash))]
      ))
  (lambda (stx) ; as normal args-hash-cons-function
    (syntax-parse stx
      [(args-hash-cons stuff ...)
       #'(#%app args-hash-cons stuff ...)]
      [args-hash-cons
       #'args-hash-cons-function])))

(module+ test
  (check-match (match (args-hash-cons "thing" (hash 0 "0" 1 "1" 2 "2" '#:kw "kw-arg"))
                 [(args-hash-cons val hash)
                  (cons val hash)])
               (cons "thing" (hash-table [0 "0"] [1 "1"] [2 "2"] ['#:kw "kw-arg"])))
  )

(define args-hash-cons*-function
  (procedure-rename
   (procedure-reduce-arity
    (hash-lambda/match
      [(hash-table [0 args-hash])
       args-hash]
      [(hash-table [0 val] [1 args-hash])
       (args-hash-cons val args-hash)]
      [(hash-table [0 args-hash] [(? keyword? kws) kw-args] ...)
       (keyword-apply/sort make-args-hash kws kw-args (list args-hash))]
      [(args-hash-cons val (args-hash-cons val-or-args-hash rest-hash))
       (args-hash-cons val (apply/hash args-hash-cons* (args-hash-cons val-or-args-hash rest-hash)))]
      )
    (arity-at-least 1))
   'args-hash-cons*))

(module+ test
  (check-match (args-hash-cons* "thing" "other-thing" (hash 0 "0" 1 "1" 2 "2" '#:kw "kw-arg"))
               (hash-table [0 "thing"] [1 "other-thing"] [2 "0"] [3 "1"] [4 "2"] ['#:kw "kw-arg"]))
  )

(define-match-expander args-hash-cons*
  (lambda (stx) ; as a pat
    (syntax-parse stx
      [(args-hash-cons* args-hash:expr)
       #'args-hash]
      [(args-hash-cons* val:expr args-hash:expr)
       #'(app (lambda (x)
                (cond [(not (hash-has-key? x 0)) #f]
                      [else
                       (cons (args-hash-first x)
                             (args-hash-rest x))]))
              (cons val args-hash))]
      [(args-hash-cons* kw:kw val:expr args-hash:expr)
       #'(app (lambda (x)
                (cond [(not (hash-has-key? x 'kw)) #f]
                      [else
                       (cons (hash-ref x 'kw)
                             (hash-remove x 'kw))]))
              (cons val args-hash))]
      [(args-hash-cons* val:expr stuff ... args-hash:expr)
       #'(args-hash-cons val (args-hash-cons* stuff ... args-hash))]
      [(args-hash-cons* kw:kw val:expr stuff ... args-hash:expr)
       #'(args-hash-cons kw val (args-hash-cons* stuff ... args-hash))]
      ))
  (lambda (stx) ; as normal args-hash-cons*-function
    (syntax-parse stx
      [(args-hash-cons* stuff ...)
       #'(#%app args-hash-cons* stuff ...)]
      [args-hash-cons*
       #'args-hash-cons*-function])))

(module+ test
  (check-match (match (args-hash-cons* "thing" "other-thing" (hash 0 "0" 1 "1" 2 "2" '#:kw "kw-arg"))
                 [(args-hash-cons* val other-val hash)
                  (list val other-val hash)])
               (list "thing" "other-thing" (hash-table [0 "0"] [1 "1"] [2 "2"] ['#:kw "kw-arg"])))
  )
    

(define/contract args-hash-append (case-> (#:rest (listof args-hash?) . -> . args-hash?))
  (case-lambda
    [() (make-hash)]
    [(h) h]
    [(h1 h2) (let* ([h1-lop (hash->list h1)]
                    [h2-lop (hash->list h2)]
                    [h1-length (length (filter (cons/c number? any/c) h1-lop))])
               (define h2-lop-with-h1-length-added
                 (for/list ([pair (in-list h2-lop)])
                   (let ([key (car pair)]
                         [val (cdr pair)])
                     (cond [(number? key) (cons (+ key h1-length) val)]
                           [else pair]))))
               (make-hash (append h1-lop h2-lop-with-h1-length-added)))]
    [(h1 . rest)
     (args-hash-append h1 (apply args-hash-append rest))]))

(module+ test
  (check-match (args-hash-append (hash 0 "0" 1 "1" 2 "2" '#:kw "kw-arg") (hash 0 "other-0" 1 "other-1" 2 "other-2" '#:other-kw "other-kw-arg"))
               (hash-table [0 "0"] [1 "1"] [2 "2"] [3 "other-0"] [4 "other-1"] [5 "other-2"] ['#:kw "kw-arg"] ['#:other-kw "other-kw-arg"]))
  )

(module+ test
  (check-equal? (args-hash-rest (hash 0 "0" 1 "1" 2 "2" '#:kw "kw-arg"))
                '#hash((0 . "1") (1 . "2") (#:kw . "kw-arg")))
  (check-equal? (args-hash-cons "thing" (hash 0 "0" 1 "1" 2 "2" '#:kw "kw-arg"))
                '#hash((0 . "thing") (1 . "0") (2 . "1") (3 . "2") (#:kw . "kw-arg")))
  (check-equal? (args-hash-cons #:other-kw "other-kw-arg" (hash 0 "0" 1 "1" 2 "2" '#:kw "kw-arg"))
                '#hash((0 . "0") (1 . "1") (2 . "2") (#:kw . "kw-arg") (#:other-kw . "other-kw-arg")))
  )



(define-syntax match?/c
  (syntax-rules ()
    [(match?/c pat)
     (flat-named-contract '(match?/c pat)
                          (lambda (x)
                            (match x
                              [pat #t]
                              [_ #f])))]
    [(match?/c pat #:when pred)
     (flat-named-contract '(match?/c pat #:when pred)
                          (lambda (x)
                            (match x
                              [pat pred]
                              [_ #f])))]
    ))





(define ((make-hash-lambda-contract-proj args-hash-contract-proj range-contract-proj) blame)
  (lambda (f)
    (unless (procedure? f)
      (raise-blame-error blame f
                         '(expected: "procedure?" given: "~e") f))
    (local [(define args-hash-blame (blame-add-context blame "the args-hash of" #:swap? #t))
            (define range-blame     (blame-add-context blame "the range of"))
            (define args-hash-wrapper (args-hash-contract-proj args-hash-blame))
            (define range-wrapper     (range-contract-proj range-blame))]
      (procedure-rename
       (procedure-reduce-keyword-arity
        (hash-lambda args-hash
          (call-with-values (λ () (apply/hash f (args-hash-wrapper args-hash)))
                            range-wrapper))
        (procedure-arity f)
        (procedure-required-keywords f)
        (procedure-allowed-keywords f))
       (object-name f)))))

(define/contract (make-hash-lambda-contract args-hash-contract [range-contract 'any])
  (contract? contract? . -> . contract?)
  (define args (list args-hash-contract range-contract))
  (local [(define (contract-name_0 c)
            (match c
              ['any 'any]
              [`(values ,loc ...) `(values ,@(map contract-name loc))]
              [c (contract-name c)]))
          (define (contract-projection_0 c)
            (-> void?)
            (match c
              ['any (λ (blame) values)]
              [c (contract-projection c)]))]
    (make-contract #:name `(make-hash-lambda-contract ,@(map contract-name_0 args))
                   #:first-order procedure?
                   #:projection (apply make-hash-lambda-contract-proj
                                       (map contract-projection_0 args)))))



(define (procedure-required-keywords f)
  (#%app (compose first list procedure-keywords) f))

(define (procedure-allowed-keywords f)
  (#%app (compose second list procedure-keywords) f))

(define/contract (arity-map f arity)
  ((exact-nonnegative-integer? . -> . any/c) procedure-arity? . -> . any/c)
  (match arity
    [(? exact-nonnegative-integer? _)
     (f arity)]
    [(arity-at-least v)
     (arity-at-least (arity-map f v))]
    [(? list? _)
     (for/list ([sub-arity (in-list arity)])
       (arity-map f sub-arity))]
    [_
     (error "what? arity is:" arity)]))









(begin-for-syntax
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












(define my-hash=?
  (lambda args
    (for/and ([h (in-list args)])
      (for/and ([other-h (in-list args)] #:when (not (equal? h other-h)))
        (for/and ([h-key (in-hash-keys h)])
          (and (hash-has-key? other-h h-key)
               (equal? (hash-ref h h-key)
                       (hash-ref other-h h-key))))))))



(module+ test
  
  (define f
    (hash-lambda h
      h))
  
  (check-equal? (f 1 2 3 #:aoeu 'aoeu #:nti 'nti)
                (make-hash (list (cons '#:aoeu 'aoeu)
                                 (cons '#:nti 'nti)
                                 (cons 0 1)
                                 (cons 1 2)
                                 (cons 2 3)
                                 )))
  
  (check-equal? (apply/hash f (make-hash (list (cons '#:aoeu 'aoeu)
                                               (cons '#:nti 'nti)
                                               (cons 0 1)
                                               (cons 1 2)
                                               (cons 2 3)
                                               )))
                (make-hash (list (cons '#:aoeu 'aoeu)
                                 (cons '#:nti 'nti)
                                 (cons 0 1)
                                 (cons 1 2)
                                 (cons 2 3)
                                 )))
  
  (define (kinetic-energy #:m m #:v v)
    (* 1/2 m (sqr v)))
  
  (let ([random-m (+ (random 100) (random))]
        [random-v (+ (random 100) (random))])
    (check-equal? (apply/hash kinetic-energy
                              (make-hash (list (cons '#:m random-m)
                                               (cons '#:v random-v))))
                  (kinetic-energy #:m random-m #:v random-v)))
  
  (check-equal? (args-hash->args-list
                 (make-hash (list (cons 3 "3")
                                  (cons 1 "1")
                                  (cons 0 "0")
                                  (cons 2 "2"))))
                (list "0" "1" "2" "3"))
  
  )