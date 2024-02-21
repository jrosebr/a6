#lang racket

(define empty-k
  (lambda ()
    (lambda (v) v)))


;Problem 1
(define binary-to-decimal-cps
  (λ (n k)
    (cond
      [(null? n) (k 0)]
      [else (+ (k (car n)) (binary-to-decimal-cps (cdr n) (λ (v)
                                                            (k (* 2 v)))))]
      )))

;(binary-to-decimal-cps '() (empty-k))
;(binary-to-decimal-cps '(1) (empty-k))
;(binary-to-decimal-cps '(0 1) (empty-k))
;(binary-to-decimal-cps '(1 1 0 1) (empty-k))


;Problem 2 (???)
(define star-cps
  (λ (m k)
    (λ (n k^)
      (k (k^ (* m n))))))

;((star-cps 2 (empty-k)) 3 (empty-k))
;((star-cps ((star-cps 2 (empty-k)) 3 (empty-k)) (empty-k)) 5 (empty-k))


;Problem 3
(define times-cps
  (λ (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) (k 0)]
      [else (times-cps (cdr ls) (λ (v)
                                  (k (* v (car ls)))))]
      )))

;(times-cps '(1 2 3 4 5) (empty-k))
;(times-cps '(1 2 3 0 3) (empty-k))


;Problem 4 (???)
(define times-cps-shortcut
  (λ (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) 0]
      [else (times-cps (cdr ls) (λ (v)
                                  (k (* v (car ls)))))]
      )))

;(times-cps-shortcut '(1 2 3 4 5) (empty-k))
;(times-cps-shortcut '(1 2 3 0 3) (empty-k))


;Problem 5 (???) X
(define remv-first-9*-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k '())]
      
      
      
      
      [else (cons (k (car ls)) (remv-first-9*-cps (cdr ls) (λ (v)
                                                             (k v))))]
      )))

(remv-first-9*-cps '((1 2 (3) 9)) (empty-k))
(remv-first-9*-cps '(9 (9 (9 (9)))) (empty-k))
(remv-first-9*-cps '(((((9) 9) 9) 9) 9) (empty-k))

;Problem 6
(define cons-cell-count-cps
  (lambda (ls k)
    (cond
      [(pair? ls)
       (add1 (+ (cons-cell-count-cps (car ls) (λ (v)
                                               (k v))) (cons-cell-count-cps (cdr ls) (λ (v)
                                                                                      (k v)))))]
      [else (k 0)]
      )))


;(cons-cell-count-cps '(1 2 3 4) (empty-k))
;(cons-cell-count-cps '(1 2 (3 (4) 5) 4 ()) (empty-k))


;Problem 7
(define find-cps
  (lambda (u s k)
    (let ((pr (assv u s)))
      (if pr (find-cps (cdr pr) s (λ (v)
                                (k v))) (k u)))))

;(find-cps 5 '((5 . a) (6 . b) (7 . c)) (empty-k))
;(find-cps 7 '((5 . a) (6 . 5) (7 . 6)) (empty-k))
;(find-cps 5 '((5 . 6) (9 . 6) (2 . 9)) (empty-k))


;Problem 8 (???)
(define ack-cps
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack-cps (sub1 m) 1 (λ (v)
                                   (k v)))]
      [else (ack-cps (sub1 m)
                 (ack-cps m (sub1 n) (λ (v)
                                   (k v))) (λ (w)
                                             (k w)))]
      )))


;(ack-cps 0 1 (empty-k)) ;2
;(ack-cps 1 0 (empty-k)) ;2
;(ack-cps 1 1 (empty-k)) ;3


;Problem 9 (???)
(define fib-cps
  (lambda (n k)
    ((lambda (fib)
       (fib fib n))
     (lambda (fib n)
       (cond
     [(zero? n) (k 0)]
     [(zero? (sub1 n)) (k 1)]
     [else (+ (fib fib (sub1 n)) (fib fib (sub1 (sub1 n))))]
     )))))

(define fib
  (λ (n)
    (cond
      ((<= n 1) 1)
      (else (+ (fib (sub1 n)) (fib (sub1 (sub1 n))))))))

;(fib-cps 2 (empty-k))
;(fib 2)

;(fib-cps 5 (empty-k))
;(fib 5)


;Problem 10
(define unfold-cps
  (lambda (p f g seed k)
    ((lambda (h)
       ((h h) seed '()))
     (lambda (h)
       (lambda (seed ans)
     (if (p seed)
         ;then
         (k ans)
         ;else
         ((h h) (g seed) (cons (f seed) ans))))))))


(define null?-cps
    (lambda (ls k)
      (k (null? ls))))

(define car-cps
    (lambda (pr k)
      (k (car pr))))

(define cdr-cps
    (lambda (pr k)
      (k (cdr pr))))


;(unfold-cps null? car cdr '(a b c d e) (empty-k))
;(unfold-cps null?-cps car-cps cdr-cps '(a b c d e) (empty-k))

;Problem 11
(define empty-s
  (lambda ()
    '()))
 
(define unify-cps
  (lambda (u v s k)
    (cond
      ((eqv? u v) (k s))
      ((number? u) (k (cons (cons u v) s)))
      ((number? v) (unify-cps v u s (λ (v)
                                      (k v))))
      ((pair? u)
       (if (pair? v)
           ;then
           (let ((s (unify-cps (find-cps (car u) s (λ (v)
                                                 (k v))) (find-cps (car v) s) s)))
             (if s (unify-cps (find-cps (cdr u) s) (find-cps (cdr v) s) s) #f))
           ;else
           #f))
      (else (k #f)))))


;Problem 12


;Problem 13





