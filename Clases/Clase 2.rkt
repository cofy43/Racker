#lang racket
(cons 'amarillo (cons 'rojo (cons 'azul empty )))
(cons 'venus (cons 'jupiter (cons 'saturno (cons 'urano (cons 'tierra (cons 'neptuno (cons 'pluton empty)))))))
(cons '0 (cons '1 (cons '2 (cons '3 (cons '4 (cons '5  (cons '6 (cons '7 (cons '8 (cons '9 empty))))))))))
;cons recibe 2 elementos elemento y una lista
(define list '())
(define list1 '(a b c))
(define list2 '(letras))
(define list3 '(estas ))
(define list4 '(son ))
(define list5 '(las ))
(cons list (cons list3 (cons list4 (cons list5 (cons list2 (cons list1 empty))))))
;Función appen para unir dos listas
;cat = first primer elemento de una lista
;cdr = rest resto de los elementos de una lista
(cons 'adan (cons '0 (cons 'juan (cons '1 (cons 'luis (cons '2 empty))))))
;pedir ejercicios
;definicion de una función:
;(define (nombre_funcion) parametros)
; (definicion de la funcion
;Por convencion las funcione sse comentan con una breve descripcion de lo que hace
;nombre_funcion : parametros (tipo) -> ouput
;Funcion que calcula el area de un circulo
;areaCirculo : number -> number
(define PI 3.141592365)
(define (areaCirculo radio)
  (* PI (* radio radio)))
;Funcion que calcula el area de un anillo entre dos circulos
;areaAnillo : number number -> number
(define (areaAnillo circulo1 circulo2)
 (-(areaCirculo circulo1) (areaCirculo circulo2)))
(define (listOfNumber lista)
  (if (null lista)
      #t
      (if (number? (car lista))
          (listOfNumber (cdr lista))
          #f)))
(define (listLengh lista)
  (if (null lista)
      0
      (+ 1 (listLengh (cdr lista)))))