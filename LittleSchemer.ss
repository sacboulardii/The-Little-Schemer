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

; (define insertR
    ; (lambda (old new lat)
        ; (cond
            ; )))

(display (lat? (quote ())))
(display (member? "v" (quote (23 321))))
(display (rember 23 (quote (23 43 54 4 12 34 54))))
(display (remberS 4 (quote (23 43 54 4 12 34 54))))
(display (firsts (quote ((23 24) (44 45) (1 2) (8 9)))))