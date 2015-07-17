#lang racket/base

(provide hash-lambda
         hash-lambda/match
         apply/kw-hash
         app/kw-hash
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
         args-hash->string
         )

(require racket/match
         racket/format
         racket/local
         racket/contract
         racket/list
         racket/math
         racket/hash
         kw-utils/keyword-lambda
         kw-utils/keyword-apply-sort
         kw-utils/keyword-app
         kw-utils/kw-hash
         (except-in kw-utils/arity+keywords arity-map)
         "prop-object-name.rkt"
         (for-syntax racket/base syntax/parse racket/list syntax/name unstable/syntax
                     (for-syntax racket/base
                                 )))
(module+ test
  (require rackunit))



(begin-for-syntax
  (define-syntax  kw (make-rename-transformer #'keyword))
  
  ;; a helper function for hash-lambda
  (define (id->string id)
    (symbol->string (syntax->datum id)))
  )

;; (hash-lambda args-hash-id body ...+)
;; (hash-lambda [args-hash-id args-hash-contract] body ...+)
;; 
;; like (lambda args-list-id body ...+), except that it takes all keywords, and it puts its arguments
;;   into a hash table instead of a list.
;; In the hash table, the by-position arguments have their position as the key, and the keyword
;;   arguments have the (quoted) keyword as the key.
;; The second form is like the first form except that it applies args-hash-contract to args-hash-id.
(define-syntax hash-lambda
  (lambda (stx)
    (syntax-parse stx
      [(hash-lambda args-hash:id body:expr ...+)
       ;#:with name (or (syntax-local-infer-name stx) '||)
       ;#'(hash-lambda-procedure
       ;   (let ([name (#%plain-lambda (args-hash) body ...)])
       ;     name))
       #'(keyword-lambda (kws kw-args . rest)
           (let ([args-hash (keyword-apply-make-args-hash kws kw-args rest)])
             body ...))]
      [(hash-lambda [args-hash:id args-hash-contract] body:expr ...+)
       #:declare args-hash-contract
       (expr/c #'contract? #:name (string-append "contract for "(id->string #'args-hash)""))
       #:with name (or (syntax-local-infer-name stx) '||)
       #'(local [(define/contract name
                   (make-hash-lambda-contract args-hash-contract 'any)
                   (hash-lambda args-hash
                     body ...))]
           name)]
      )))

(struct hash-lambda-procedure (proc)
  #:transparent
  #:property prop:object-name
  (λ (this) (object-name (hash-lambda-procedure-proc this)))
  #:property prop:procedure
  (keyword-lambda (kws kw-args this . rest-args)
    (let ([this.proc (hash-lambda-procedure-proc this)])
      (this.proc (keyword-apply-make-args-hash kws kw-args rest-args))))
  )

(define-syntax-rule (hash-lambda/match match-clause ...)
  (hash-lambda args-hash
    (match args-hash match-clause ...)))

(module+ test
  (local []
    (define return-args-hash
      (hash-lambda args-hash
        args-hash))
    (check-equal? (return-args-hash "0" "1" #:keyword "keyword-argument" "2")
                  (hash 0 "0" 1 "1" 2 "2" '#:keyword "keyword-argument"))
    (check-equal? (object-name return-args-hash) 'return-args-hash)
    (define my+
      (hash-lambda args-hash
        (+ (hash-ref args-hash 0)
           (hash-ref args-hash 1))))
    (check-equal? (my+ 1 2)
                  3)
    (check-equal? (object-name my+) 'my+)
    (define/contract kinetic-energy (#:mass number? #:velocity number? . -> . number?)
      (hash-lambda args-hash
        (let ([mass     (hash-ref args-hash '#:mass)]
              [velocity (hash-ref args-hash '#:velocity)])
          (* 1/2 mass (sqr velocity)))))
    (check-equal? (kinetic-energy #:mass 2 #:velocity 1)
                  1)
    (check-equal? (object-name kinetic-energy) 'kinetic-energy)
    )
  (local []
    (define my+
      (hash-lambda/match
       [(hash-table [0 a] [1 b])
        (+ a b)]))
    (check-equal? (my+ 1 2)
                  3)
    (check-equal? (object-name my+) 'my+)
    (define/contract kinetic-energy (#:mass number? #:velocity number? . -> . number?)
      (hash-lambda/match
       [(hash-table ['#:mass mass] ['#:velocity velocity])
        (* 1/2 mass (sqr velocity))]))
    (check-equal? (kinetic-energy #:mass 2 #:velocity 1)
                  1)
    (check-equal? (object-name kinetic-energy) 'kinetic-energy)
    )
  )

;; kw-hash+list->args-hash : (Hashof Kw Any) (Listof Any) -> (Hashof (U Kw Nat) Any)
(define (kw-hash+list->args-hash kw-hash lst)
  (for/fold ([args-hash kw-hash]) ([arg (in-list lst)] [i (in-naturals)])
    (hash-set args-hash i arg)))

;; (args-hash->kw-hash+list args-hash)
;; the inverse of kw-hash+list->args-hash
;; returns 2 values:
;;   the first value is a hash containing the keyword arguments
;;   the second value is a list containing the positional arguments
(define (args-hash->kw-hash+list args-hash)
  (define-values [kw-hash n]
    (for/fold ([kw-hash (hash)] [n 0])
              ([(k v) (in-hash args-hash)])
      (cond [(keyword? k)
             (values (hash-set kw-hash k v) n)]
            [(exact-nonnegative-integer? k)
             (values kw-hash (max n (add1 k)))]
            [else
             (error 'args-hash
                    (string-append "expected a keyword or a natural number as a key, given: ~v\n"
                                   "  args-hash: ~v")
                    k args-hash)])))
  (define lst
    (for/list ([i (in-range n)])
      (hash-ref args-hash i)))
  (values kw-hash lst))

;; (apply/hash proc args-hash #:<kw> kw-arg ...)
;; like apply, but accepts an args-hash instead of a list
(define apply/hash
  (keyword-lambda (kws kw-args f args-hash)
    (define-values [kw-hash lst]
      (args-hash->kw-hash+list args-hash))
    (keyword-app apply/kw-hash kws kw-args f kw-hash lst)))

(module+ test
  (test-case "apply/hash"
    (check-equal? (apply/hash list (hash 0 "0" 1 "1"))
                  (list "0" "1"))
    (check-equal? (apply/hash list (hash 1 "1" 0 "0"))
                  (list "0" "1"))
    (define (kinetic-energy #:m m #:v v)
      (* 1/2 m (sqr v)))
    (check-equal? (apply/hash kinetic-energy (hash '#:m 2 '#:v 1))
                  1)
    ))



(define (args-hash? x)
  (and (hash? x)
       (immutable? x)
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
  (cond [(not (args-hash? x)) #false]
        [else
         (define-values [kw-hash lst]
           (args-hash->kw-hash+list x))
         (cond [(hash-empty? kw-hash) lst]
               [else #false])]))


;; equivalent to (keyword-apply make-args-hash kws kw-args rest)
(define (keyword-apply-make-args-hash kws kw-args rest)
  (define kw-hash
    (keyword-app-make-kw-hash kws kw-args))
  (kw-hash+list->args-hash kw-hash rest))

(define (keyword-app-make-args-hash kws kw-args . rest)
  (keyword-apply-make-args-hash kws kw-args rest))

(define/contract make-args-hash-function
  (unconstrained-domain-> args-hash?)
  (make-keyword-procedure
   keyword-app-make-args-hash
   (let ([make-args-hash
          (lambda rest
            (kw-hash+list->args-hash (hash) rest))])
     make-args-hash)))

(module+ test
  (check-equal? (make-args-hash 1 2 3 #:kw-1 'kw-arg-1 #:kw-2 'kw-arg-2)
                (hash 0 1 1 2 2 3 '#:kw-1 'kw-arg-1 '#:kw-2 'kw-arg-2)))

(define-match-expander make-args-hash
  (lambda (stx) ; as a pat
    (syntax-parse stx
      [(make-args-hash)
       #'(hash-table)]
      [(make-args-hash x ...)
       #'(args-hash-cons* x ... (make-args-hash))]))
  (make-variable-like-transformer #'make-args-hash-function) ; as normal make-args-hash-function
  )

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
  (for/hash ([(key val) (in-hash (hash-remove args-hash 0))])
    (cond [(number? key)
           (values (sub1 key) val)]
          [else
           (values key val)])))

(module+ test
  (check-equal? (args-hash-rest (hash 0 "0" 1 "1" 2 "2" '#:kw "kw-arg"))
                (hash 0 "1" 1 "2" '#:kw "kw-arg"))
  )

(define args-hash-cons*/no-kw
  (let ([args-hash-cons*
         (case-lambda
           [(hsh) hsh]
           [(val . rst)
            (define n (length rst))
            (match-define-values [rst-vals (list hsh)]
              (split-at-right rst 1))
            (define hsh-shft
              (for/hash ([(k v) (in-hash hsh)])
                (cond [(number? k) (values (+ k n) v)]
                      [else (values k v)])))
            (for/fold ([hsh hsh-shft]) ([val (in-list (cons val rst-vals))] [i (in-naturals)])
              (hash-set hsh i val))]
           )])
    args-hash-cons*))

(define keyword-app-args-hash-cons*
  (case-lambda
    [(kws kw-args hsh)
     (for/fold ([hsh hsh]) ([kw (in-list kws)] [kw-arg (in-list kw-args)])
       (hash-set hsh kw kw-arg))]
    [(kws kw-args val . rst)
     (keyword-app-args-hash-cons* kws kw-args (apply args-hash-cons*/no-kw val rst))]))

(define args-hash-cons*-function
  (make-keyword-procedure keyword-app-args-hash-cons*
                          args-hash-cons*/no-kw))

(define args-hash-cons-function args-hash-cons*-function)

(module+ test
  (check-equal? (args-hash-cons "thing" (hash 0 "0" 1 "1" 2 "2" '#:kw "kw-arg"))
                (hash 0 "thing" 1 "0" 2 "1" 3 "2" '#:kw "kw-arg"))
  
  (check-equal? (match (args-hash-cons "thing" (hash 0 "0" 1 "1" 2 "2" '#:kw "kw-arg"))
                  [(args-hash-cons val hash)
                   (cons val hash)])
                (cons "thing" (hash 0 "0" 1 "1" 2 "2" '#:kw "kw-arg")))

  (check-equal? (args-hash-cons* "thing" "other-thing" (hash 0 "0" 1 "1" 2 "2" '#:kw "kw-arg"))
                (hash 0 "thing" 1 "other-thing" 2 "0" 3 "1" 4 "2" '#:kw "kw-arg"))

  (check-equal? (args-hash-cons* #:kw-2 "thing" #:kw-3 "other-thing" (hash 0 "0" 1 "1" 2 "2" '#:kw "kw-arg"))
                (hash 0 "0" 1 "1" 2 "2" '#:kw "kw-arg" '#:kw-2 "thing" '#:kw-3 "other-thing"))
  
  (check-equal? (match (args-hash-cons* "thing" "other-thing" (hash 0 "0" 1 "1" 2 "2" '#:kw "kw-arg"))
                 [(args-hash-cons* val other-val hash)
                  (list val other-val hash)])
                (list "thing" "other-thing" (hash 0 "0" 1 "1" 2 "2" '#:kw "kw-arg")))
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
  (make-variable-like-transformer #'args-hash-cons-function) ; as normal args-hash-cons-function
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
  (make-variable-like-transformer #'args-hash-cons*-function) ; as normal args-hash-cons*-function
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    

(define/contract args-hash-append (case-> (#:rest (listof args-hash?) . -> . args-hash?))
  (case-lambda
    [() (hash)]
    [(h) h]
    [hs
     (define-values [kwhs lsts]
       (for/lists [kwhs lsts] ([h (in-list hs)])
         (args-hash->kw-hash+list h)))
     (kw-hash+list->args-hash (apply hash-union kwhs) (apply append lsts))]))

(module+ test
  (check-equal? (args-hash-append
                 (hash 0 "0" 1 "1" 2 "2" '#:kw "kw-arg")
                 (hash 0 "other-0" 1 "other-1" 2 "other-2" '#:other-kw "other-kw-arg"))
                (hash 0 "0" 1 "1" 2 "2" 3 "other-0" 4 "other-1" 5 "other-2"
                      '#:kw "kw-arg" '#:other-kw "other-kw-arg"))
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
  (local [(define args-hash-blame (blame-add-context blame "the args-hash of" #:swap? #t))
          (define range-blame     (blame-add-context blame "the range of"))
          (define args-hash-wrapper (args-hash-contract-proj args-hash-blame))
          (define range-wrapper     (range-contract-proj range-blame))]
    (lambda (f)
      (unless (procedure? f)
        (raise-blame-error blame f
                           '(expected: "procedure?" given: "~e") f))
      (procedure-rename
       (procedure-reduce-arity+keywords
        (hash-lambda args-hash
          (call-with-values (λ () (apply/hash f (args-hash-wrapper args-hash)))
                            range-wrapper))
        (procedure-arity+keywords f))
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




(define (args-hash->string args-hash)
  (define-values [kw-hsh lst] (args-hash->kw-hash+list args-hash))
  (define (string-append* . args)
    (apply string-append (flatten args)))
  (string-append*
   "(make-args-hash"
   (for/list ([arg (in-list lst)])
     (format " ~v" arg))
   (for/list ([p (in-list (sort (hash->list kw-hsh) keyword<? #:key car))])
     (match-define (cons kw arg) p)
     (format " ~s ~v" kw arg))
   ")"))















(module+ test
  
  (define f
    (hash-lambda h
      h))
  
  (check-equal? (f 1 2 3 #:aoeu 'aoeu #:nti 'nti)
                (hash '#:aoeu 'aoeu
                      '#:nti 'nti
                      0 1
                      1 2
                      2 3
                      ))
  
  (check-equal? (apply/hash f (hash '#:aoeu 'aoeu
                                    '#:nti 'nti
                                    0 1
                                    1 2
                                    2 3
                                    ))
                (hash '#:aoeu 'aoeu
                      '#:nti 'nti
                      0 1
                      1 2
                      2 3
                      ))
  
  (define (kinetic-energy #:m m #:v v)
    (* 1/2 m (sqr v)))
  
  (let ([random-m (+ (random 100) (random))]
        [random-v (+ (random 100) (random))])
    (check-equal? (apply/hash kinetic-energy
                              (hash '#:m random-m
                                    '#:v random-v))
                  (kinetic-energy #:m random-m #:v random-v)))
  
  (check-equal? (args-hash->args-list
                 (hash 3 "3"
                       1 "1"
                       0 "0"
                       2 "2"))
                (list "0" "1" "2" "3"))
  
  )
