#lang lazy

(require racket/trace)

(define Y (λ (f)
            ((λ (x) (f (x x)))
             (λ (x) (f (x x))))))

(define sum-list
  (λ (rec)
    (λ (acc ls)
      ((mt? ls)
       acc
       (rec (+ acc (hd ls)) (tl ls))))))

(define t (λ (a b) a))
(define f (λ (a b) b))

(define : (λ (l ls) (λ (f) (f l ls))))

(define hd (λ (ls) (ls t)))
(define tl (λ (ls) (ls f)))

(define mt (λ (f) t))

(define mt? (λ (ls) (ls (λ (a b) f))))

(define lfold
  (λ (rec)
    (λ (f acc ls)
      ((mt? ls)
       acc
       (rec f (f acc (hd ls)) (tl ls))))))

(define (flist . elems)
  (if (empty? elems)
      mt
      (: (car elems) (apply flist (cdr elems)))))