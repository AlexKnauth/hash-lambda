#lang racket/base

(provide mutable-match-lambda-clause-append
         mutable-match-lambda-next
         within-mutable-match-lambda-clause-append?
         )

(require racket/list
         racket/format
         racket/match
         kw-utils/keyword-lambda
         kw-utils/arity+keywords
         )



(define (mutable-match-lambda-clause-append #:name [name 'mutable-match-lambda] . orig-fs)
  (define new-name
    (cond [(symbol? name) name]
          [name (string->symbol (~a name))]
          [else 'mutable-match-lambda]))
  (define (rename proc)
    (cond [name (procedure-rename proc new-name)]
          [else proc]))
  (procedure-reduce-arity+keywords
   (rename
    (keyword-lambda (kws kw-args . args)
      (define next (make-next #:name new-name orig-fs kws kw-args args))
      (parameterize ([current-mutable-match-lambda-next next])
        (try orig-fs kws kw-args args))))
   (apply arity+keywords-combine/or (map procedure-arity+keywords orig-fs))))



(define (try orig-fs kws kw-args args)
  (let/cc k
    (define orig-next (current-mutable-match-lambda-next))
    (define (loop fs)
      (match fs
        ['() (orig-next)]
        [(cons fst rst) (define (next)
                          (call-with-values (λ () (loop rst)) k))
                        (parameterize ([current-mutable-match-lambda-next next])
                          (cond [(procedure-arity+keywords-matches? fst (length args) kws)
                                 (keyword-apply fst kws kw-args args)]
                                [else (loop rst)]))]))
    (loop orig-fs)))



(define current-mutable-match-lambda-next
  (make-parameter #f))

(define (mutable-match-lambda-next)
  (let ([next (current-mutable-match-lambda-next)])
    (cond
      [next (next)]
      [else
       (error 'mutable-match-lambda-next "not within a mutable-match-lambda clause")])))

(define (within-mutable-match-lambda-clause-append?)
  (if (current-mutable-match-lambda-next) #t #f))



(define (make-next #:name name orig-fs kws kw-args args)
  (define orig-next (current-mutable-match-lambda-next))
  (cond [orig-next orig-next]
        [else (define (next)
                (error name
                       (string-append
                        "no clause matches" "\n"
                        "  args: ~a" "\n"
                        "  clauses: ~v")
                       (string-append*
                        (for/list ([arg (in-list args)])
                          (format "~v " arg))
                        (for/list ([kw (in-list kws)]
                                   [kw-arg (in-list kw-args)])
                          (format "~a ~v " kw kw-arg)))
                       orig-fs))
              next]))



(define (string-append* . args)
  (apply string-append (flatten args)))



