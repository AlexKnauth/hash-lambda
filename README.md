hash-lambda
===========

hash-lambdas allow you to create a "rest argument" see Declaring a Rest Argument that includes
keyword arguments. Instead of storing the arguments in a list, hash-lambda stores them in a
hash table, where you can use (hash-ref args-hash 0), etc. to access by-position arguments,
and (hash-ref args-hash '#:kw), etc. to access keyword arguments, or you can use
hash-lambda/match and use match patterns for the arguments. 
You can also use apply/hash to apply an args-hash to a function.

Examples:
```racket
> (require hash-lambda)
> (define my+
    (hash-lambda/match
     [(hash-table [0 a] [1 b])
      (+ a b)]))
> (my+ 1 2)
3
> (define/contract kinetic-energy (#:mass number? #:velocity number? . -> . number?)
    (hash-lambda/match
     [(hash-table ['#:mass mass] ['#:velocity velocity])
      (* 1/2 mass (sqr velocity))]))
> (kinetic-energy #:mass 2 #:velocity 1)
1
```
