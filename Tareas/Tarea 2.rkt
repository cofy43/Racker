#lang racket
;Ejercicio 1
(define (esPar numero)
  (if (eq? (modulo numero 2) 0)
      "par"
      "impar"))
;Ejercicio 2
(define (tricotomia numero1 numero2)
  (cond
    [(< numero1 numero2) ("Primer numero mayor") ]
    [(> numero1 numero2) ("Segundo numero mayor")]
    [(eq? numero1 numero2)("Son iguales")]))
;Ejercicio 3
;a
(cons 0 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 empty))))))
;b
(cons 'adam (cons 0 (cons 'juan (cons 1 (cons 'luis (cons 2 empty))))))
;c
(cons 2 (cons 1 (cons 1 (cons 2 (cons 3 (cons 1 empty))))))
;Ejercicio 4
(list 'a 'b 'c 'd 'e)
(list 1 2)
;Ejercicio 5
(define (mes numero)
  (cond
    [(eq? numero 1)'Enero]
    [(eq? numero 2)'Febrero]
    [(eq? numero 3)'Marzo]
    [(eq? numero 4)'Abril]
    [(eq? numero 5)'Mayo]
    [(eq? numero 6)'Junio]
    [(eq? numero 7)'Julio]
    [(eq? numero 8)'Agosto]
    [(eq? numero 9)'Septiembre]
    [(eq? numero 10)'Octubre]
    [(eq? numero 11)'Noviembre]
    [(eq? numero 12)'Diciembre]
    [(> numero 12 )'Numero_erroneo]
    [(< numero 12 )'Numero_erroneo]))