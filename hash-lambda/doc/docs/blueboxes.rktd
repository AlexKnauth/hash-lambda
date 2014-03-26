995
((3) 0 () 1 ((q lib "hash-lambda/main.rkt")) () (h ! (equal) ((c form c (c (? . 0) q hash-lambda/match)) q (109 . 6)) ((c def c (c (? . 0) q args-hash-rest)) q (817 . 3)) ((c def c (c (? . 0) q args-hash->args-list)) q (1239 . 3)) ((c def c (c (? . 0) q make-hash-lambda-contract)) q (2909 . 5)) ((c form c (c (? . 0) q keyword-lambda)) q (1330 . 2)) ((c def c (c (? . 0) q apply/hash)) q (294 . 5)) ((c def c (c (? . 0) q hash-has-key?/c)) q (2785 . 3)) ((c def c (c (? . 0) q keyword-apply/sort)) q (2344 . 13)) ((c def c (c (? . 0) q args-hash?)) q (438 . 3)) ((c form c (c (? . 0) q keyword-case-lambda)) q (1394 . 24)) ((c form c (c (? . 0) q match?/c)) q (2856 . 3)) ((c form c (c (? . 0) q hash-lambda)) q (0 . 3)) ((c def c (c (? . 0) q args-hash-first)) q (707 . 3)) ((c def c (c (? . 0) q args-hash-cons*)) q (1037 . 4)) ((c def c (c (? . 0) q make-args-hash)) q (494 . 6)) ((c def c (c (? . 0) q args-hash-cons)) q (931 . 4)) ((c def c (c (? . 0) q args-hash-append)) q (1149 . 3))))
syntax
(hash-lambda args-hash-id body ...+)
(hash-lambda [args-hash-id args-hash-contract] body ...+)
syntax
(hash-lambda/match match-clause ...)
 
match-clause = [pat body ...+]
             | [pat (=> id) body ...+]
             | [pat #:when cond-expr body ...+]
procedure
(apply/hash proc args-hash #:<kw> kw-arg ...) -> any
  proc : procedure?
  args-hash : args-hash?
  kw-arg : any/c
procedure
(args-hash? x) -> boolean?
  x : any/c
procedure
(make-args-hash stuff                      
                ...                        
                #:<kw> more-stuff ...) -> args-hash?
  stuff : any/c
  more-stuff : any/c
procedure
(args-hash-first args-hash) -> any/c
  args-hash : (and/c args-hash? (hash-has-key?/c 0))
procedure
(args-hash-rest args-hash) -> args-hash?
  args-hash : (and/c args-hash? (hash-has-key?/c 0))
procedure
(args-hash-cons val args-hash) -> args-hash?
  val : any/c
  args-hash : args-hash?
procedure
(args-hash-cons* val ... args-hash) -> args-hash?
  val : any/c
  args-hash : args-hash?
procedure
(args-hash-append args-hash ...) -> args-hash?
  args-hash : args-hash?
procedure
(args-hash->args-list args-hash) -> (or/c list? #f)
  args-hash : any/c
syntax
(keyword-lambda (kws kw-args . rest-args) body ...)
syntax
(keyword-case-lambda clause ...)
 
clause     = [kw-formals maybe-when body-expr ...+]
           | [#:args-hash match-pattern maybe-when body-expr ...+]
           | [#:rest match-pattern maybe-when body-expr ...+]
           | [#:kws match-pattern #:kw-args match-pattern #:rest match-pattern maybe-when body-expr ...+]
           | [_ maybe-when body-expr ...+]
           | [else maybe-when body-expr ...+]
           | [#:when condition-expr body-expr ...+]
           | [#:unless condition-expr body-expr ...+]
              
maybe-when = 
           | #:when condition-expr
           | #:unless condition-expr
              
kw-formals = (arg ...)
           | (arg ... . rest-id)
           | rest-id
              
arg        = id
           | [id default-expr]
           | keyword id
           | keyword [id default-expr]
procedure
(keyword-apply/sort f                      
                    kws                    
                    kw-args                
                    v ...                  
                    lst                    
                    #:<kw> kw-arg ...) -> any
  f : procedure?
  kws : (listof keyword?)
  kw-args : list?
  v : any/c
  lst : list?
  kw-arg : any/c
procedure
(hash-has-key?/c key) -> flat-contract?
  key : any/c
syntax
(match?/c pat)
(match?/c pat #:when pred)
procedure
(make-hash-lambda-contract  args-hash-contract     
                           [range-contract])   -> contract?
  args-hash-contract : (or/c contract? 'any)
  range-contract : (or/c contract? 'any) = 'any
