#lang racket/base

(provide mutable-match-lambda-clause-append
         mutable-match-lambda-next
         within-mutable-match-lambda-clause-append?
         )

(require racket/list
         racket/match
         hash-lambda
         )



(define (mutable-match-lambda-clause-append . orig-fs)
  (keyword-lambda (kws kw-args . args)
    (define next (make-next orig-fs kws kw-args args))
    (parameterize ([current-mutable-match-lambda-next next])
      (try orig-fs kws kw-args args))))



(define (try orig-fs kws kw-args args)
  (let/cc k
    (define orig-next (current-mutable-match-lambda-next))
    (define (loop fs)
      (match fs
        ['() (orig-next)]
        [(cons fst rst) (define (next)
                          (call-with-values (λ () (loop rst)) k))
                        (parameterize ([current-mutable-match-lambda-next next])
                          (keyword-apply fst kws kw-args args))]))
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



(define (make-next orig-fs kws kw-args args)
  (define orig-next (current-mutable-match-lambda-next))
  (cond [orig-next orig-next]
        [else (define (next)
                (error 'mutable-match-lambda
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



