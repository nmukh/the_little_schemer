#lang racket
(define atom?
   (lambda (a)
      (not (list? a))))

;;Chapter 2
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

;;Chapter 7
(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat)(cdr lat)) #f)
      (else (set? (cdr lat))))))