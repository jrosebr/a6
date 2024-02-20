#lang racket

;Problem 1
(define binary-to-decimal-cps
  (λ (n k)
    (cond
      [(null? n) (k 0)]
      [else (+ (k (car n)) (binary-to-decimal-cps (cdr n) (λ (v)
                                                            (k (* 2 v)))))])))

;(binary-to-decimal-cps '() (λ (v) v))
;(binary-to-decimal-cps '(1) (λ (v) v))
;(binary-to-decimal-cps '(0 1) (λ (v) v))
;(binary-to-decimal-cps '(1 1 0 1) (λ (v) v))


;Problem 2 (???)
(define star-cps
  (λ (m k)
    (λ (n)
      (k (* m n)))))

;((star-cps 2 (λ (v) v)) 3)
;((star-cps ((star-cps 2 (λ (v) v)) 3) (λ (v) v)) 5)


;Problem 3
(define times-cps
  (λ (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) (k 0)]
      [else (* (k (car ls)) (times-cps (cdr ls) (λ (v)
                                                  (k v))))])))

;(times-cps '(1 2 3 4 5) (λ (v) v))
;(times-cps '(1 2 3 0 3) (λ (v) v))


;Problem 4 (???)
(define times-cps-shortcut
  (λ (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) 0]
      [else (* (k (car ls)) (times-cps (cdr ls) (λ (v)
                                                  (k v))))])))

;(times-cps-shortcut '(1 2 3 4 5) (λ (v) v))
;(times-cps-shortcut '(1 2 3 0 3) (λ (v) v))


;Problem 5
(define remv-first-9*-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k '())]
      
      [(pair? (car ls))
       (cond
         [(equal? (car ls) (remv-first-9*-cps (car ls) (λ (v) v)))
          (cons (k (car ls)) (remv-first-9*-cps (cdr ls) (λ (v)
                                                           (k v))))]
         [else (cons (k (cdr ls)) (remv-first-9*-cps (car ls) (λ (v)
                                                   (k v))))])]
      
      [(eqv? (car ls) '9) (k (cdr ls))]
      [else (cons (k (car ls)) (remv-first-9*-cps (cdr ls) (λ (v)
                                                             (k v))))])))

(remv-first-9*-cps '((1 2 (3) 9)) (λ (v) v))
(remv-first-9*-cps '(9 (9 (9 (9)))) (λ (v) v))
(remv-first-9*-cps '(((((9) 9) 9) 9) 9) (λ (v) v))

















