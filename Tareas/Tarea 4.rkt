#lang racket
;Funcion auxiliar que filtra la lista con el primer elemento de una lista que sean
;multiplos de este
;auxiliar : list -> list

(define (auxiliar lista)

  (filter (lambda (x) (or (= x (car lista)) (> (remainder x (car lista)) 0))) lista))

;Funcion que recibe un numero y genera una lista de los numeros primos desde el 2 hasta
;el numero que le indicaron utilizando el algoritmo de la criba de aristotenes
;criba-eratostenes : number -> list

(define (criba-eratostenes numero)
  (cond
    [(< numero 1) '()]
    [(auxiliar (cdr (cdr (build-list (+ numero 1) values))))]))
;Funcion auxiliar que asigna el valor del cero al diez con su representacion en
;japones
;numero-japones : number -> list

(define (numeros-japones numero)

  (cond
    [(= numero 0) "rei 一"]
    [(= numero 1) "ichi 一"]
    [(= numero 2) "ni 二"]
    [(= numero 3) "san 三"]
    [(= numero 4) "yon 四"]
    [(= numero 5) "go 五"]
    [(= numero 6) "roku 六"]
    [(= numero 7) "nana 七"]
    [(= numero 8) "haci 八"]
    [(= numero 9) "kyu 九"]
    [(= numero 10) "ju 十"]
    ))

;Funcion que recibe un numero y construye su representacion en japones
;a-japones : number -> string
(define (a-japones numero)

  (cond
    [(< numero 0) (cons "マイナス " (cons (a-japones (* numero -1)) empty))]
    [(<= numero 10) (numeros-japones numero)]
    [(= (modulo numero 10) 0) (cons (numeros-japones (/ numero 10))(cons (numeros-japones (10) empty)))]
    [else (cons (numeros-japones (quotient numero 10))(cons " ju "(cons (numeros-japones(modulo numero 10)) empty)))])
)
