#lang racket
;funcion para concatenar cadenas:
;(string-append argumentos)


;Funcion que remueve la primera aparicion de un elemento en una
;lista y devuelve la lista sin la primera aparicion del elemento
;removeFist : symbol list -> list
(define (removeFirst simbolo lista)
  (cond
    [(null? lista) "No se encontro coincidencias"]
    [(= simbolo (car lista)) (car lista)]
    [else (cons (car lista)(removeFirst simbolo (cdr lista)))]))
;Funcion que remueve la aparicion de un elemento en una
;lista y devuelve la lista sin la aparicion del elemento
;removeFist : symbol list -> list
(define (remove simbolo lista)
  (cond
    [(null? lista) "No se encontro"]
    [(= simbolo (car lista)) (remove simbolo (cdr lista))]
    [else (cons (car lista)(remove simbolo (cdr lista)))]))
;Funcion que verifica si se encuentra un elemento en una
;lista y devuelve un booleano con la respuesta
;contains? : symbol list -> boolean
(define (contains? simbolo lista)
  (cond
    [(null? lista) #f]
    [(= simbolo (car lista))]
    [else (contains? simbolo (cdr lista))]))
;Funcion que cuenta cuantas apariciones tiene un elemento
;en una lista y devuelve el numero de apariciones
;how-many : symnbol list -> number
(define (how-many simbolo lista)
  (cond
   [(null? lista) 0]
   [(= simbolo (car lista)) (+ 1 (how-many simbolo (cdr lista)))]
   [else (how-many simbolo (cdr lista))]))
;Funcion que cuenta el numero de elementos de una lista
;how-mucho : list -> number
(define (how-much lista)
  (cond
    [(null? lista) 0]
    [else (+ 1 (how-much (cdr lista)))]))
(define (fibonacci numero)
  (cond
    [(= numero 0) 1]
    [(= numero 1) 1]
    [(= numero 2) 1]
    [else (+ (fibonacci (- numero 1)) (fibonacci (- numero 2)))]))
