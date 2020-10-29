#lang racket

(define (fuel-requirement mass)
  (- (floor (/ mass 3)) 2))

(define (module-fuel-requirement mass acc)
  (let ([required-mass (fuel-requirement mass)])
    (cond
      [(and (<= required-mass 2) (> required-mass 0)) (+ acc required-mass)]
      [(< required-mass 0) acc]
      [else (module-fuel-requirement required-mass (+ acc required-mass))])))

(define (sum lst)
  (foldl (lambda (x a) (+ a x)) 0 lst))

(define (total-fuel-requirement modules)
  (sum (map (lambda (x) (module-fuel-requirement x 0)) modules)))

(define (load-input file)
  (let ([input (open-input-file file #:mode 'text)])
    (map string->number (port->lines input))))
