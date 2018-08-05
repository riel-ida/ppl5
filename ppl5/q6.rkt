#lang racket
(require racket/sandbox)
(require racket/exn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 1: The lazy lists interface ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cons-lzl cons)

(define empty-lzl empty)

(define empty-lzl? empty?)

(define head car)

(define tail
  (lambda (lz-lst)
    ((cdr lz-lst))))

(define div
  (lambda (num n)
    (floor (/ num n))))

;; Signature: take(lz-lst,n)
;; Type: [LzL*Number -> List]
;; If n > length(lz-lst) then the result is lz-lst as a List
(define take
  (lambda (lz-lst n)
    (if (or (= n 0) (empty-lzl? lz-lst))
      empty-lzl
      (cons (head lz-lst)
            (take (tail lz-lst) (- n 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 2: Auxiliary functions for testing ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Signature: check-inf-loop(mission)
; Purpose: check if the result is infinite loop,
;          if so, return 'infinite
;          otherwise the actual result
; Type: [[Empty -> T1] -> Union(T1, Symbol)]
(define check-inf-loop
  (lambda (mission)
    (with-handlers ([exn:fail:resource?
                     (lambda (e)
                       (if (equal? (exn->string e)
                                   "with-limit: out of time\n")
                           'infinite
                           'error))])
      (call-with-limits 1 #f mission))))

; A function that creates an infinite loop
(define (inf x) (inf x))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 3: The assignment ;
;;;;;;;;;;;;;;;;;;;;;;;;;;

; Signature: all-subs(long)
; Type: [List(T) -> LZL(List(T))]
; Purpose: compute all lists that can be obtained 
; from long by removing items from it.
; Pre-conditions: -
; Tests:
; (take (all-subs '(1 2 3)) 8) ->
; '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

(define all-subs
  (lambda (long)
    (allSubsGen long 0 (expt 2 (length long)))))
;original - original list
;counter - number of sublists that we have created
;limit - number of sublists
(define allSubsGen
  (lambda (original counter limit)
    (if (empty? original)
         empty-lzl
         (if (eq? counter limit)
             empty-lzl
             (cons-lzl (createNextSub original counter '()) (lambda () (allSubsGen original (+ 1 counter) limit)))))))

(define createNextSub
  (lambda (original counter sub)
    (if (eq? counter 0)
         sub
         (if (empty? original)
             sub
             (if(eq? (modulo counter 2) 0)
                (createNextSub (cdr original) (div counter 2) sub)
                (createNextSub (cdr original) (div counter 2) (append sub (cons (car original) '()))))))))
                        
  


;;;;;;;;;;;;;;;;;;;;;
; Part 4: The tests ;
;;;;;;;;;;;;;;;;;;;;;

;; Make sure to add take or another utility to test here
;; If the results are obained in a different order, change the test accordingly.
(check-inf-loop (lambda () (take (all-subs '(1 2 3)) 8)))
(check-inf-loop (lambda () (take (all-subs '(a b)) 3)))
(check-inf-loop (lambda () (take (all-subs '()) 1)))
(check-inf-loop (lambda () (take (all-subs '(7)) 4)))
(check-inf-loop (lambda () (take (all-subs '(a b c d)) 14)))
(check-inf-loop (lambda () (take (all-subs '(1 2)) 8)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 5: The tests expected results;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
> (check-inf-loop (lambda () (take (all-subs '(1 2 3)) 8))
'(() (1) (2) (1 2) (3) (1 3) (2 3) (1 2 3))
> (check-inf-loop (lambda () (take (all-subs '(a b)) 3)))
'(() (a) (b))
> (check-inf-loop (lambda () (take (all-subs '()) 1)))
'()
> (check-inf-loop (lambda () (take (all-subs '(7)) 4)))
'(() (7))
> (check-inf-loop (lambda () (take (all-subs '(a b c d)) 10)))
'(() (a) (b) (a b) (c) (a c) (b c) (a b c) (d) (a d) (b d) (a b d) (c d) (a c d))
> (check-inf-loop (lambda () (take (all-subs '(1 2)) 8)))
'(() (1) (2) (1 2))
|#