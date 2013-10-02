#lang racket

(define atom?
   (lambda (a)
      (not (list? a))))

;;Chapter 2
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (equal? (car lat) a)
                (member? a (cdr lat)))))))
;;Chapter 3
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat)'())
      (else
       (cond
         ((eq? (car lat) a)
          (multirember a (cdr lat)))
         (else (cons (car lat)
                     (multirember a (cdr lat)))))))))

(define firsts
  (lambda(l)
    (cond
      ((null? l)'())
      (else (cons (car (car l))
                  (firsts (cdr l)))))))
      
;;Chapter 4
(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1)(number? a2))
       (= a1 a2))
      ((or (number? a1)(number? a2))
       #f)
      (else (eq? a1 a2)))))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n))(car lat))
      (else (pick (sub1 n)(cdr lat))))))

(define one?
  (lambda(n)
    (cond
      (else (= n 1)))))

;;Chapter 5
(define eqlist?
  (lambda (l1 l2)
  (cond
    ((and (null? l1)(null? l2)) #t)
    ((or (null? l1)(null? l2)) #f)
    ((and (atom? (car l1))
          (atom? (car l2)))
     (and (eqan? (car l1)(car l2))
          (eqlist? (cdr l1)(cdr l2))))
    ((or (atom? (car l1))
         (atom? (car l2)))
     #f)
    (else
     (and (eqlist? (car l1)(car l2))
          (eqlist? (cdr l1)(cdr l2)))))))

(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2))
       (eqan? s1 s2))
      ((or (atom? s1)(atom? s2))
       #f)
      (else (eqlist? s1 s2)))))


;;Chapter 7
(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat)(cdr lat)) #f)
      (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond 
      ((null? lat)'())
      ((member? (car lat)(cdr lat))
       (makeset (cdr lat)))
      (else (cons (car lat)
                  (makeset (cdr lat)))))))

(define subset?
  (lambda(set1 set2)
    (cond
      ((null? set1) #t)
      ((member? (car set1) set2)
       (subset? (cdr set1) set2))
      (else #f))))

(define eqset?
  (lambda(set1 set2)
    (and (subset? set1 set2)(subset? set2 set1))))

(define intersect?
  (lambda(set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2)
                (intersect?
                 (cdr set1) set2))))))

(define intersect
  (lambda(set1 set2)
    (cond
      ((null? set1)'())
      ((member? (car set1) set2)
       (cons (car set1)
             (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)
       (union (cdr set1) set2))
      (else (cons (car set1)(union (cdr set1) set2))))))
    
(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set))(car l-set))
      (else 
       (intersect (car l-set)
       (intersectall (cdr l-set)))))))

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x)#t)
      ((null? x)#f)
      ((null?(cdr x))#f)
      ((null?(cdr(cdr x))) #t)
      (else #f))))

(define first
  (lambda(p)
    (cond
      (else (car p)))))

(define second
  (lambda(p)
    (cond
      (else (car(cdr p))))))

(define build
  (lambda(s1 s2)
    (cond
      (else (cons s1
                  (cons s2 '()))))))

(define fun?
  (lambda(rel)
    (set? (firsts rel))))

(define revrel
  (lambda(rel)
    (cond
      ((null? rel)'())
      (else
       (cons (build 
              (second (car rel))
              (first (car rel)))
             (revrel (cdr rel)))))))

(define fullfun?
  (lambda(fun)
    (set? (second fun))))

;;Chapter 8 - Lambda the Ultimate
 
(define eq?-c
  (lambda(a)
    (lambda(x)
      (eq? x a))))

(define rember-f
  (lambda(test?)
    (lambda(a l)
      (cond
        ((null? l)'())
        ((test? (car l) a)(cdr l))
        (else (cons (car l)
                    ((rember-f test?) a 
                                      (cdr l))))))))

(define insert-g
  (lambda(seq)
    (lambda(new old l)
      (cond
        ((null? l)('()))
        ((eq? (car l) old)
         (seq new old (cdr l)))
        (else (cons (car l)
                    ((insert-g seq) new old
                                    (cdr l))))))))
                                                                      
(define multirember-f
    (lambda(test?)
      (lambda(a lat)
        (cond
          ((null? lat) '())
          ((test? a (car lat))
           ((multirember-f test?) a 
                                  (cdr lat)))
           (else (cons (car lat)((multirember-f test?) a (cdr lat))))))))

(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat)
       (col '() '()))
      ((eq? (car lat) a)
       (multirember&co a
       (cdr lat)
       (lambda (newlat seen)
         (col newlat
              (cons (car lat) seen)))))
      (else
       (multirember&co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col (cons (car lat) newlat)
                              seen)))))))
                       
  
  
;;9. and again and again

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn)
       (keep-looking a (pick sorn lat) lat))
      (else (eq? sorn a)))))

(define eternity
  (lambda(x)
    (eternity x)))

;;Collatz
(define C
  (lambda(n)
    (cond
      ((one? n) 1)
      (else
       (cond
         ((even? n) (C (/ n 2)))
         (else (C (add1 (* 3 n)))))))))

;;Ackermann
(define A
  (lambda(n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n)
               (A n (sub1 m)))))))