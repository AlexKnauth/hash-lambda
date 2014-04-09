#lang racket/base

(provide hash-lambda
         hash-lambda/match
         apply/hash
         args-hash?
         args-hash->args-list
         make-args-hash
         args-hash-first
         args-hash-rest
         args-hash-cons
         args-hash-cons*
         args-hash-append
         hash-has-key?/c
         match?/c
         make-hash-lambda-contract
         (all-from-out "misc/keyword-lambda.rkt")
         args-hash->string
         )

(require racket/match
         racket/format
         racket/local
         racket/contract
         racket/list
         racket/math)

(require (for-syntax racket/base syntax/parse racket/list))

(module+ test
  (require rackunit))



(require "misc/keyword-lambda.rkt")



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











(define my-hash=?
  (lambda args
    (for/and ([h (in-list args)])
      (for/and ([other-h (in-list args)] #:when (not (equal? h other-h)))
        (for/and ([h-key (in-hash-keys h)])
          (and (hash-has-key? other-h h-key)
               (equal? (hash-ref h h-key)
                       (hash-ref other-h h-key))))))))












(define (args-hash->string args-hash)
  (define (key<? a b)
    (cond [(and (number?  a) (number?  b)) (< a b)]
          [(and (keyword? a) (keyword? b)) (keyword<? a b)]
          [(and (number?  a) (keyword? b)) #t]
          [(and (keyword? a) (number?  b)) #f]
          [else (error "args-hash->string: key should be either a number or a keyword, but keys are ~v and ~v" a b)]))
  (let* ([lop (hash->list args-hash)]
         [sorted-lop (sort lop key<? #:key car)])
    (string-append
     "(make-args-hash"
     (apply string-append
            (for/list ([p (in-list sorted-lop)])
              (let ([key (car p)] [val (cdr p)])
                (string-append
                 (if (keyword? key) (string-append " "(~s key)" ") " ")
                 (~v val)))))
     ")")))















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