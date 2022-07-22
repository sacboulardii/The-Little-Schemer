(define atom?
    (lambda (x)
        (and (not (null? x)) (not (pair? x)))))

(define lat?
    (lambda (l)
        (cond
            ((null? l) #t)
            ((atom? (car l)) (lat? (cdr l)))
            (else #f))))

(define member?
    (lambda (a lat)
        (cond
            ((null? lat) #f)
            (else a))))
                        
(define rember
    (lambda (a lat)
        (cond
            ((null? lat) (quote ()))
            (else (cond
                ((eq? a (car lat)) (cdr lat))
                (else (cons (car lat)
                    (rember a
                        (cdr lat)))))))))

(define remberS
    (lambda (a lat)
        (cond
            ((null? lat) (quote ()))
            ((eq? a (car lat)) (cdr lat))
            (else (cons (car lat) (rember a (cdr lat)))))))

(define firsts
    (lambda (lat)
        (cond
            ((null? lat) (quote ()))
            (else (cons (car (car lat))
                (firsts (cdr lat)))))))

(define insertR
    (lambda (old new lat)
        (cond
            ((null? lat) (quote ()))
            ((eq? old (car lat)) (cons old (cons new (cdr lat))))
            (else (cons (car lat) (insertR old new (cdr lat)))))))

(define insertL
    (lambda (old new lat)
        (cond
            ((null? lat) (quote ()))
            ((eq? old (car lat)) (cons new lat))
            (else (cons (car lat) (insertL old new (cdr lat)))))))

(define subst
    (lambda (old new lat)
        (cond
            ((null? lat) (quote ()))
            ((eq? old (car lat)) (cons new (cdr lat)))
            (else (cons (car lat) (subst old new (cdr lat)))))))

(define subst2
    (lambda (new o1 o2 lat)
        (cond
            ((null? lat) (quote ()))
            ((or (eq? o1 (car lat)) (eq? o2 (car lat)))
                (cons new (cdr lat)))
            (else (cons (car lat) 
                (subst2 new o1 o2 (cdr lat)))))))

(define multirember
    (lambda (a lat)
        (cond
            ((null? lat) (quote ()))
            ((eq? a (car lat)) (multirember a (cdr lat)))
            (else (cons (car lat) (multirember a (cdr lat)))))))

(define multiinsertR
    (lambda (old new lat)
        (cond
            ((null? lat) (quote ()))
            ((eq? old (car lat)) 
                (cons old (cons new 
                    (multiinsertR old new (cdr lat)))))
            (else (cons (car lat) (multiinsertR old new (cdr lat)))))))

(define multiinsertL
    (lambda (old new lat)
        (cond
            ((null? lat) (quote ()))
            ((eq? old (car lat)) 
                (cons new (cons old 
                    (multiinsertL old new (cdr lat)))))
            (else (cons (car lat) (multiinsertL old new (cdr lat)))))))

(define multisubst
    (lambda (a lat)
        (cond
            ()
            ()
            ())))

(display (lat? (quote ())))
(display (member? "v" (quote (23 321))))
(display (rember 23 (quote (23 43 54 4 12 34 54))))
(display (remberS 4 (quote (23 43 54 4 12 34 54))))
(display (firsts (quote ((23 24) (44 45) (1 2) (8 9)))))
(display (insertR 23 99 (quote (9 43 54 4 12 23 34 54))))
(display (insertL 23 99 (quote (9 43 54 4 12 23 34 54))))
(display (subst 23 99 (quote (9 43 54 4 12 23 34 54))))
(display (subst2 99 43 23 (quote (9 43 54 4 12 23 34 54))))
(display (multirember 12 (quote (9 43 12 54 4 12 23 12 34 12 54))))
(display (multiinsertR 12 99 (quote (9 43 12 54 4 12 23 12 34 12 54))))
(display (multiinsertL 12 99 (quote (9 43 12 54 4 12 23 12 34 12 54))))
