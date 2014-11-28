#lang racket/base

(provide let/inferred-name)

(require syntax/parse/define
         (for-syntax racket/base
                     syntax/parse
                     syntax/name
                     ))
(module+ test
  (require rackunit))

(define-syntax let/inferred-name
  (lambda (stx)
    (syntax-parse stx
      [(let/inferred-name name-id:id body:expr ...+)
       #:with name (syntax-local-infer-name stx)
       #'(let ([name-id 'name]) body ...)])))

(module+ test
  (check-equal? (let ([x (let/inferred-name name name)]) x) 'x)
  (check-equal? (let ([y (let/inferred-name name name)]) y) 'y)
  )
