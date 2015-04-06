#lang racket/base

(provide prop:object-name
         prop:object-name-supported?
         )

(define-values (prop:object-name-fallback _1 _2)
  (make-struct-type-property 'prop:object-name-fallback))

(define prop:object-name
  (dynamic-require 'racket/base 'prop:object-name
                   (λ () prop:object-name-fallback)))

(define prop:object-name-supported?
  (not (eq? prop:object-name prop:object-name-fallback)))

