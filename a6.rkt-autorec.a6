#lang racket

(define empty-k
  (lambda ()
    (lambda (v) v)))


;Problem 1
(define binary-to-decimal-cps
  (λ (n k)
    (cond
      [(null? n) (k 0)]
      [else (binary-to-decimal-cps (cdr n) (λ (v)
                                             (k (+ (* 2 v) (car n)))))]
      )))

;(binary-to-decimal-cps '() (empty-k))
;(binary-to-decimal-cps '(1) (empty-k))
;(binary-to-decimal-cps '(0 1) (empty-k))
;(binary-to-decimal-cps '(1 1 0 1) (empty-k))


;Problem 2
(define star-cps
  (lambda (m k)
    (k (lambda (n k)
         (k (* m n))))))

;(star-cps 2 (lambda (s) (s 3 (empty-k)))) ;(???)
;(star-cps 5 (lambda (s-5) (star-cps 2 (lambda (s-2) (s-2 3 (lambda (v) (s-5 v (empty-k))))))))
;((star-cps 2 (empty-k)) 3 (empty-k)) ;WORKS
;((star-cps ((star-cps 2 (empty-k)) 3 (empty-k)) (empty-k)) 5 (empty-k)) ;WORKS


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


;Problem 4
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


;Problem 5
(define remv-first-9*-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k '())]
      [(pair? (car ls))
       (remv-first-9*-cps (car ls)
                          (λ (v)
                            (cond
                              [(equal? (car ls) v)
                               (remv-first-9*-cps (cdr ls)
                                                  (λ (w)
                                                    (k (cons (car ls) w))))]
  
                              [else (k (cons v (cdr ls)))])))]
      
      [(eqv? (car ls) '9) (k (cdr ls))]
      [else (remv-first-9*-cps (cdr ls) (λ (w)
                                          (k (cons (car ls) w))))]
      )))

;(remv-first-9*-cps '((1 2 (3) 9)) (empty-k))
;(remv-first-9*-cps '(9 (9 (9 (9)))) (empty-k))
;(remv-first-9*-cps '(((((9) 9) 9) 9) 9) (empty-k))


;Problem 6
(define cons-cell-count-cps
  (lambda (ls k)
    (cond
      [(pair? ls)
       (cons-cell-count-cps (car ls) (λ (v)
                                       (cons-cell-count-cps (cdr ls) (λ (w)
                                                                       (k (add1 (+ v w)))))))]
      [else (k 0)]
      )))


;(cons-cell-count-cps '(1 2 3 4) (empty-k))
;(cons-cell-count-cps '(1 2 (3 (4) 5) 4 ()) (empty-k))


;Problem 7
(define find-cps
  (lambda (u s k)
    (let ((pr (assv u s)))
      (if pr
          ;then
          (find-cps (cdr pr) s (λ (v)
                                 (k v)))
          ;else
          (k u)))))

;(find-cps 5 '((5 . a) (6 . b) (7 . c)) (empty-k))
;(find-cps 7 '((5 . a) (6 . 5) (7 . 6)) (empty-k))
;(find-cps 5 '((5 . 6) (9 . 6) (2 . 9)) (empty-k))


;Problem 8 
(define ack-cps
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack-cps (sub1 m) 1 (λ (v)
                                       (k v)))]
      [else (ack-cps m (sub1 n) (λ (v)
                                  (ack-cps (sub1 m)
                                           v (λ (w)
                                               (k w)))))]
      )))


;(ack-cps 0 1 (empty-k)) ;2
;(ack-cps 1 0 (empty-k)) ;2
;(ack-cps 1 1 (empty-k)) ;3


;Problem 9
(define fib-cps
  (lambda (n k)
    ((lambda (fib^ k)
       (fib^ fib^ n k))
     (lambda (fib^^ n k)
       (cond
         [(zero? n) (k 0)]
         [(zero? (sub1 n)) (k 1)]
         [else (fib^^ fib^^ (sub1 n) (λ (v)
                                       (fib^^ fib^^ (sub1 (sub1 n)) (λ (w)
                                                                      (k (+ v w))))))]))
     k)))


(define fib
  (lambda (n)
    ((lambda (fib^)
       (fib^ fib^ n))
     (lambda (fib^^ n)
       (cond
         [(zero? n) 0]
         [(zero? (sub1 n)) 1]
         [else (+ (fib^^ fib^^ (sub1 n)) (fib^^ fib^^ (sub1 (sub1 n))))])))))


;(fib-cps 2 (empty-k))
;(fib 2)

;(fib-cps 5 (empty-k))
;(fib 5)


;Problem 10

(define unfold-cps
  (lambda (p f g seed k)
    ((lambda (h^ k)
       (h^ h^ (lambda (v)
                (v seed '() k))))
     (lambda (h^^ k)
       (k (lambda (seed ans k)
            (p seed (lambda (m)
                      (if m
                          (k ans)
                          (h^^ h^^
                               (lambda (w)
                                 (g seed
                                    (lambda (n)
                                      (f seed
                                         (lambda (l)
                                           (w n (cons l ans) k)))))))))))))
     k)))
    

(define null?-cps
  (lambda (ls k)
    (k (null? ls))))

(define car-cps
  (lambda (pr k)
    (k (car pr))))

(define cdr-cps
  (lambda (pr k)
    (k (cdr pr))))


;(unfold-cps null? car cdr '(a b c d e) (empty-k)) ;(e d c b a)
;(unfold-cps null?-cps car-cps cdr-cps '(a b c d e) (empty-k)) ;(e d c b a)

;Problem 11
(define empty-s
  (lambda ()
    '()))
 
(define unify-cps
  (lambda (u v s k)
    (cond
      [(eqv? u v) (k s)]
      [(number? u) (k (cons (cons u v) s))]
      [(number? v) (unify-cps v u s k)]
      [(pair? u)
       (if (pair? v)
           (find-cps (car u) s (λ (f)
                                 (find-cps (car v) s (λ (g)
                                                       (unify-cps f g s (λ (y)
                                                                          (if y
                                                                              (find-cps (cdr u) y (λ (q)
                                                                                                    (find-cps (cdr v) y (λ (p)
                                                                                                                          (unify-cps q p y k)))))
                                                                              (k #f))))))))
           (k #f))]
      (else (k #f)))))


;(unify-cps 'x 5 (empty-s) (empty-k)) ;WORKS
;(unify-cps 'x 5 (unify-cps 'y 6 (empty-s) (empty-k)) (empty-k)) ;WORKS
;(unify-cps '(x y) '(5 6) (empty-s) (empty-k)) ;DOES NOT WORK
;(unify-cps 'x 5 (unify-cps 'x 6 (empty-s) (empty-k)) (empty-k)) ;WORKS
;(unify-cps '(x x) '(5 6) (empty-s) (empty-k)) ;DOES NOT WORK
;(unify-cps '(1 2 3) '(x 1 2) (empty-s) (empty-k)) ;DOES NOT WORK
;(unify-cps 'x 'y (empty-s) (empty-k)) ;WORKS
                                                     

;Problem 12
;This problem took me 3 hours. I'm so tired.
(define M-cps
  (λ (f k)
    (k (λ (ls k^)
         (cond
           [(null? ls) (k^ '())]
           [else  (M-cps f (λ (w)
                             (f (car ls) (λ (c)
                             (cons c (w (cdr ls) k^))))))] 
           )))))

(M-cps (lambda (n k) (k (add1 n))) (lambda (p) (p (quote (1 2 3 4 5)) (empty-k))))


;Problem 13 (???)
#;(define use-of-M-cps
    ((M-cps (lambda (n) (add1 n))) '(1 2 3 4 5) (empty-k)))

