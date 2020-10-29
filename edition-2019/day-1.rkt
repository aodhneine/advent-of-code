#lang racket

(define (fuel-requirement module-mass)
  (- (floor (/ module-mass 3)) 2))

(define (sum lst)
  (foldl (lambda (x a) (+ a x)) 0 lst))

(define (total-fuel-requirement modules)
  (sum (map fuel-requirement modules)))

(define (load-input file)
  (let ([input (open-input-file file #:mode 'text)])
    (map string->number (port->lines input))))
