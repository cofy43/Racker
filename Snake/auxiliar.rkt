#lang plai

(require 2htdp/image 2htdp/universe)

(require "definitions.rkt")

;;posn=? :: posn -> posn -> Bool
;funcion que compara dos posiciones, cada posicion es una coordenada (x,y)
;(x1,y1) = (x2,y2) <-> x1 = x2 y y1 = y2
;hint: usa las funciones posn-x y posn-y que nos regresan la coordenada x y y de la posición respectivamente 
(define (posn=? p1 p2)
  (and (= (posn-x p1)(posn-x p2)) (= ( posn-y p1) (posn-y p2)))
  )

;;snake-head :: snake -> goo
;función que recibe una snake y nos regresa su cabeza, recuerda que snake es una estructura compuesta de una dirección
;y una lista de goos que representa la serpiente.
;hint: utiliza la función snake-segs que recibe una snake y regresa la lista de goos de esta.
(define (snake-head sn)
  (car (snake-segs sn))
  )

;;snake-body :: snake -> List-goo
;Función que recibe una snake y nos regresa su cuerpo sin la cabeza.
(define (snake-body sn)
  (cdr (snake-segs sn))
  )

;;snake-change-dir :: snake -> dir -> snake
;Función que recibe una snake y una nueva dirección y cambia la dirección de la snake.
;Hint: construye una nueva snake.
(define (snake-change-dir sn d)
  (snake d (snake-segs sn))
  )