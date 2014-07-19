#lang racket/base

(provide syntax->string
         quoted->string)

(require racket/match
         racket/string
         racket/list
         racket/format)

(define (syntax->string stx #:stx.e [stx.e (syntax-e stx)])
  (cond [(symbol? stx.e)  (symbol->string stx.e)]
        [(number? stx.e)  (number->string stx.e)]
        [(string? stx.e)  (~v stx.e)]
        [(bytes? stx.e)   (~v stx.e)]
        [(regexp? stx.e)  (~v stx.e)]
        [(boolean? stx.e) (~v stx.e)]
        [(keyword? stx.e) (string-append "#:"(keyword->string stx.e))]
        [else
         (let ([stx.lst (syntax->list stx)])
           (cond
             [(list? stx.lst) (syntax-list->string stx #:stx.lst stx.lst)]
             [(pair? stx.e)   (syntax-pair->string stx #:stx.e stx.e)]
             [(vector? stx.e) (syntax-vector->string stx #:stx.e stx.e)]
             [(box? stx.e)    (syntax-box->string stx #:stx.e stx.e)]
             [(hash? stx.e)   (syntax-hash->string stx #:stx.e stx.e)]
             [(prefab-struct? stx.e) (syntax-prefab-struct->string stx #:stx.e stx.e)]
             [else (error 'syntax->string "!!! not done yet. given: ~v, stx.e is: ~v" stx stx.e)]))]))

(define (quoted->string dat)
  (syntax->string (datum->syntax #f dat)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; helper functions

(define (syntax-list->string stx #:stx.lst [stx.lst (syntax->list stx)])
  (cond 
    [(and (= 2 (length stx.lst))
          (member (syntax-e (car stx.lst))
                  (list 'quote  'quasiquote  'unquote  'unquote-splicing
                        'syntax 'quasisyntax 'unsyntax 'unsyntax-splicing)))
     (syntax->string:handle-quote-etc stx #:stx.lst stx.lst)]
    [else
     (let ([stx.paren-shape (cond [(syntax-property stx 'paren-shape)] [else #\(])])
       (match stx.paren-shape
         [#\( (string-append "("(string-join (map syntax->string stx.lst))")")]
         [#\[ (string-append "["(string-join (map syntax->string stx.lst))"]")]
         [#\{ (string-append "{"(string-join (map syntax->string stx.lst))"}")]))]))

(define (syntax-pair->string stx #:stx.e [stx.e (syntax-e stx)])
  (cond [(list? stx.e) (syntax-list->string stx #:stx.e stx.e)]
        [else
         (let ([stx.paren-shape (cond [(syntax-property stx 'paren-shape)] [else #\(])])
           (define-values (without-last last)
             (split-at-right stx.e 0))
           (match stx.paren-shape
             [#\( (string-append "("(string-join (map syntax->string without-last))" . "(syntax->string last)")")]
             [#\[ (string-append "["(string-join (map syntax->string without-last))" . "(syntax->string last)"]")]
             [#\{ (string-append "{"(string-join (map syntax->string without-last))" . "(syntax->string last)"}")]))
         ]))

(define (syntax-vector->string stx #:stx.e [stx.e (syntax-e stx)])
  (let ([stx.e->list (vector->list stx.e)]
        [stx.paren-shape (cond [(syntax-property stx 'paren-shape)] [else #\(])])
    (match stx.paren-shape
      [#\( (string-append "#("(string-join (map syntax->string stx.e->list))")")]
      [#\[ (string-append "#["(string-join (map syntax->string stx.e->list))"]")]
      [#\{ (string-append "#{"(string-join (map syntax->string stx.e->list))"}")])))

(define (syntax-box->string stx #:stx.e [stx.e (syntax-e stx)])
  (let ([val-stx (unbox stx.e)])
    (string-append "#&"(syntax->string val-stx))))

(define (syntax-hash->string stx #:stx.e [stx.e (syntax-e stx)])
  (let ([lop (hash->list stx.e)]
        [stx.paren-shape (cond [(syntax-property stx 'paren-shape)] [else #\(])])
    (define (key-val-pair->string p)
      (let ([key-str (quoted->string (car p))] [val-str (syntax->string (cdr p))])
        (string-append "("key-str" . "val-str")")))
    (match stx.paren-shape
      [#\( (string-append "#hash("(string-join (map key-val-pair->string lop))")")]
      [#\[ (string-append "#hash["(string-join (map key-val-pair->string lop))"]")]
      [#\{ (string-append "#hash{"(string-join (map key-val-pair->string lop))"}")])))

(define (syntax-prefab-struct->string stx #:stx.e [stx.e (syntax-e stx)])
  (let ([prefab-key (prefab-struct-key stx.e)]
        [field-vals (rest (vector->list (struct->vector stx.e)))]
        [stx.paren-shape (cond [(syntax-property stx 'paren-shape)] [else #\(])])
    (match stx.paren-shape
      [#\( (string-append "#s("(quoted->string prefab-key)" "(string-join (map syntax->string field-vals))")")]
      [#\[ (string-append "#s["(quoted->string prefab-key)" "(string-join (map syntax->string field-vals))"]")]
      [#\{ (string-append "#s{"(quoted->string prefab-key)" "(string-join (map syntax->string field-vals))"}")])))

(define (syntax->string:handle-quote-etc stx #:stx.lst [stx.lst (syntax->list stx)])
  (match (match stx.lst [`(,quote-etc ,thing) `(,(syntax-e quote-etc) ,thing)])
    [`(quote ,stx_0)            (string-append "'"(syntax->string stx_0))]
    [`(quasiquote ,stx_0)       (string-append "`"(syntax->string stx_0))]
    [(list 'unquote stx_0)      (string-append ","(syntax->string stx_0))]
    [`(unquote-splicing ,stx_0) (string-append ",@"(syntax->string stx_0))]
    [`(syntax ,stx_0)            (string-append "#'"(syntax->string stx_0))]
    [`(quasisyntax ,stx_0)       (string-append "#`"(syntax->string stx_0))]
    [`(unsyntax ,stx_0)          (string-append "#,"(syntax->string stx_0))]
    [`(unsyntax-splicing ,stx_0) (string-append "#,@"(syntax->string stx_0))]))

(define (prefab-struct? x)
  (prefab-struct-key x))

(module+ test
  (require rackunit)
  
  (define (paren-shape stx shape)
    (syntax-property stx 'paren-shape shape))
  (define ps paren-shape)
  (define-syntax-rule (|(| expr) #`expr)
  (define-syntax-rule (|[| expr) (ps #`expr #\[))
  (define-syntax-rule (|{| expr) (ps #`expr #\{))
  
  (check-equal? (syntax->string #`(cond #,(ps #'[this that] #\[) #,(ps #'[else this] #\[)))
                "(cond [this that] [else this])")
  
  (check-equal? (syntax->string #'this) "this")
  (check-equal? (syntax->string #'5) "5")
  (check-equal? (syntax->string #'"this") "\"this\"")
  (check-equal? (syntax->string (syntax #"this")) "#\"this\"")
  (check-equal? (syntax->string (syntax #rx"this")) "#rx\"this\"")
  (check-equal? (syntax->string (syntax #px"this")) "#px\"this\"")
  (check-equal? (syntax->string (syntax #t)) "#t")
  (check-equal? (syntax->string (syntax #f)) "#f")
  
  (check-equal? (syntax->string #'(this that)) "(this that)")
  (check-equal? (syntax->string (ps #'[this that] #\[)) "[this that]")
  (check-equal? (syntax->string (ps #'{this that} #\{)) "{this that}")
  
  (check-equal? (syntax->string #'(this . that)) "(this . that)")
  (check-equal? (syntax->string (ps #'[this . that] #\[)) "[this . that]")
  (check-equal? (syntax->string (ps #'{this . that} #\{)) "{this . that}")
  
  (check-equal? (syntax->string #'(this this . that)) "(this this . that)")
  (check-equal? (syntax->string (ps #'[this this . that] #\[)) "[this this . that]")
  (check-equal? (syntax->string (ps #'{this this . that} #\{)) "{this this . that}")
  
  (check-equal? (syntax->string (syntax #(this that))) "#(this that)")
  (check-equal? (syntax->string (ps (syntax #[this that]) #\[)) "#[this that]")
  (check-equal? (syntax->string (ps (syntax #{this that}) #\{)) "#{this that}")
  
  (check-equal? (syntax->string (syntax #&this)) "#&this")
  
  (check-equal? (syntax->string (syntax #hash((this . that)))) "#hash((this . that))")
  
  (check-equal? (syntax->string (syntax #s(this that))) "#s(this that)")
  
  (check-equal? (syntax->string (syntax 'this)) "'this")
  (check-equal? (syntax->string (syntax `this)) "`this")
  (check-equal? (syntax->string (syntax ,this)) ",this")
  (check-equal? (syntax->string (syntax ,@this)) ",@this")
  (check-equal? (syntax->string (syntax #'this)) "#'this")
  (check-equal? (syntax->string (syntax #`this)) "#`this")
  (check-equal? (syntax->string (syntax #,this)) "#,this")
  (check-equal? (syntax->string (syntax #,@this)) "#,@this")
  
  (check-equal? (syntax->string #'(lambda (x) (+ x 1))) "(lambda (x) (+ x 1))")
  (check-equal? (syntax->string #'(lambda (x) . ((+ x 1)))) "(lambda (x) (+ x 1))")
  
  )