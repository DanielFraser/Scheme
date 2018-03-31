;Daniel Fraser
;Created 9/24/2016

;Problem 1 doubles every element in the list
(define (echo lst)
  (if (null? lst)
      '()        ;if list is null, return empty list(acts as a safeguard and an endpoint)
      (append (list (car lst) (car lst))  (echo (cdr lst)))) ;add 2 of the first element, move onto next element
  )

;Problem 2 (assumming 0 <= x < length lst) gets the nth element in the list
(define (nth x lst)
  (cond                             ;decides to either return current first element or move onto next one
    ((zero? x) (car lst))           ;if x is zero (the point we need to be at) then return the first element
    (else (nth (- x 1) (cdr lst)))) ;subtract 1 from x and move onto next
  )

;Problem 3 reverses the list on all levels
(define (deep-reverse lst) 
  (if(list? lst)                      ;is the current value of lst, a list?
     (reverse (map deep-reverse lst)) ;if its a list, move into the list and repeat until no moreinside lists
     lst)                             ;return list
  )

;Problem 4 converts keys to values
(define (assoc-all keys a-list)
  (map (lambda (x) (cadr(assoc x a-list))) keys) ;associates each value of keys with a-list then
                                                 ;takes the 2nd value of each list and adds it into a new one
 )

;Problem 5 my own version of a map function
(define (filter fn lst)
  (if(null? lst) ;;is list null
     '()         ;;return empty list
  (if (fn (car lst))  ;;checks first element against function
      (cons (car lst) (filter fn (cdr lst))) ;;if its #t, adds it to a new list
      (filter fn (cdr lst)))) ;;otherwise, it moves onto the next (eventually will get empty list)
  )
 
