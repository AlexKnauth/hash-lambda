#lang setup/infotab

(define name "hash-lambda")

(define collection 'multi)

(define version "-1.0")

(define deps '("base" "rackunit-lib" "at-exp-lib" "scribble-lib" "sandbox-lib"))

(define primary-file "hash-lambda/main.rkt")

(define scribblings '(["hash-lambda/docs/table-of-contents.scrbl" (multi-page)]
                      ["hash-lambda/docs/hash-lambda.scrbl" ()]
                      ["hash-lambda/docs/misc.rkt" (multi-page)]
                      ["hash-lambda/docs/misc/keyword-lambda.scrbl" ()]
                      ["hash-lambda/docs/misc/arity+keywords.scrbl" ()]
                      ["hash-lambda/docs/misc/keyword-case-lambda.scrbl" ()]
                      ["hash-lambda/docs/misc/mutable-match-lambda.scrbl" ()]
                      ["hash-lambda/docs/misc/syntax-to-string.scrbl" ()]
                      ))