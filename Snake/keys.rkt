#lang plai

(require 2htdp/image 2htdp/universe)

(require "definitions.rkt")
(require "auxiliar.rkt")

;;dir? :: String -> Booleano
;Función que toma una cadena y verifica si esta cadena es una dirección valida up, down, left, right.
(define (dir? x)
  (or (string=? x "up")
      (string=? x "down")
      (string=? x "left")
      (string=? x "right")
  )
)

;; opposite-dir? :: String -> String -> Bool
; Función que recibe dos cadenas que representan una dirección y dicen si son opuestas o no
(define (opposite-dir? d1 d2)
  (or (and (string=? d1 "up") (string=? d2 "down"))
      (and (string=? d1 "down") (string=? d2 "up"))
      (and (string=? d1 "left") (string=? d2 "right"))
      (and (string=? d1 "right") (string=? d2 "left"))
  )
)

(define (world-change-dir w d)
  (define the-snake (pit-snake w))
  (cond [(and (opposite-dir? (snake-dir the-snake) d) 
              (cons? (rest (snake-segs the-snake))))
         (stop-with w)]
        [else 
         (pit (snake-change-dir the-snake d) 
              (pit-goos w))]))

