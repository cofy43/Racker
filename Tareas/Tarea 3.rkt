#lang racket
;Ejercicio 1
(define (monos mono1 mono2)
  (cond
    [(and mono1 mono2) "Hay problema"]
    [(and (eq? mono1 #f)(eq?  mono2 #f) "Hay problema")]
    [else "No hay problema"]))
(define (perico numero)
  (cond
    [(or (< numero 0)(> numero 23)) "Hora incorrecta"]
    [(and (> numero 7)(< numero 20)) "EL perico puede hablar"]
    [else "EL perico no puede hablar"]))
(define (divisorPropio numero)
  ((filter (modulo [lambda(elementos) (interger? (/ numero elementos))](build-list numero values)))))
(define (superSuma numero)
  (cond
    [(= numero 0) 0]
    [else (+ (/ numero 10) (superSuma (/ numero 10)))]))