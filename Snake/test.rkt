#lang plai

(require 2htdp/image 2htdp/universe)

(require "definitions.rkt")
(require "clock-ticks.rkt")
(require "main.rkt")
(require "keys.rkt")
(require "render.rkt")
(require "finish.rkt")
(require "auxiliar.rkt")

(define goo-list (build-list MAX-GOO (lambda (x) (fresh-goo))))
(define snake0 (snake "right" (list (posn 1 1))))
(define world0 (pit snake0 goo-list))

(define left-snake0 (snake "left" (list (posn 1 1))))
(define left-world0 (pit left-snake0 goo-list))


(define snake1 (snake "left" (list (posn 5 5))))
(define snake2 (snake "up" (list (posn 2 2) (posn 2 3) (posn 2 4))))
(define wall-snake (snake "right" (list (posn 0 1) (posn 1 1))))
(define self-eating-snake (snake "up" (list
                                       (posn 19 3)
                                       (posn 19 4)
                                       (posn 20 4)
                                       (posn 21 4)
                                       (posn 22 4)
                                       (posn 22 3)
                                       (posn 21 3)
                                       (posn 20 3)
                                       (posn 19 3)
                                       (posn 18 3))))
(define goo1 (goo (posn 5 5) 45))
(define goo2 (goo (posn 4 8) 1))
(define goo3 (goo (posn 6 9) 40))
(define goo4 (goo (posn 1 1) 120))
(define goo5 (goo (posn 1 9) 58))
(define goo-list1 (list goo1 goo2 goo3 goo4 goo5))
(define world1 (pit snake1 goo-list1))
(define world2 (pit snake2 goo-list1))

(define right-snake1 (snake "right" (list (posn 5 5))))
(define right-world1 (pit right-snake1 goo-list1))

(module+ test 
  
  (require rackunit rackunit/text-ui)
  
  (check-equal? (pit-snake (next-pit world2))
                (snake "up" (list (posn 2 1) (posn 2 2) (posn 2 3))))
  (check-equal? (pit-snake (next-pit world1))
                (snake "left" (list (posn 4 5) (posn 5 5))))
  (check-true (let ([f (pit-goos (next-pit world1))])
                (= (length f) MAX-GOO)))
  (check-equal? (pit-snake (next-pit (pit snake0 (list (goo (posn SIZE SIZE) 100)))))
                (snake "right" (list (posn 2 1))))
  (check-equal? (pit-snake (next-pit (pit snake0 (list (goo (posn 1 1) 130)))))
                (snake "right" (list (posn 2 1) (posn 1 1))))
  
  (check-equal? (direct-snake world0 "down") 
                (world-change-dir world0 "down"))
  (check-equal? (direct-snake world0 "right")
                world0)
  
  (check-equal? (render-pit world0)
                (snake+scene snake0
                             (goo-list+scene goo-list MT-SCENE)))
  (check-equal? (render-pit world1)
                (snake+scene snake1 (goo-list+scene goo-list1 MT-SCENE)))
  (check-equal? (render-pit world2)
                (snake+scene snake2 (goo-list+scene goo-list1 MT-SCENE)))
  
  (check-true (dead? (pit wall-snake '())))
  (check-true (dead? (pit self-eating-snake  '())))
  (check-false (dead? (pit snake1  '())))
  (check-false (dead? (pit snake2  '())))
  (check-false (dead? world0))
  
  (check-equal? (render-end world1)
                (overlay (text "Game over" 15 "black")
                         (render-pit world1)))
  (check-equal? (render-end world2)
                (overlay (text "Game over" 15 "black")
                         (render-pit world2)))

  (define (prop:goo-rot-- i)
    (test-begin 
     (for ([i (in-range i)])
       (define goos (list-of-n-goo MAX-GOO))
       (define goo-initial-expire (map goo-expire goos))
       (check-equal? (map sub1 goo-initial-expire)
                     (map goo-expire (age-goo goos))))))

  (define (prop:new-goo-range i)
    (test-begin 
     (for ([i (in-range i)])
       (define f (fresh-goo))
       (check-true (and (< 0 (posn-x (goo-loc f)) SIZE)
                        (< 0 (posn-y (goo-loc f)) SIZE))))))
  
  (define (list-of-n-goo n)
    (cond [(zero? n) empty]
          [else (define rand (random 5))
                (cons (list-ref goo-list1 rand) (list-of-n-goo (sub1 n)))]))
  

  (check-equal? (pit-snake (world-change-dir (pit snake1 "foobar") "down"))
                (snake "down" (list (posn 5 5))))
  (check-equal? (pit-snake (world-change-dir (pit snake2 "right") "left"))
                (snake "left" (list (posn 2 2) (posn 2 3) (posn 2 4))))
  
  (prop:goo-rot-- 1000)
  
  (check-equal? (grow snake0)
                (snake "right" (list (posn 2 1) (posn 1 1))))
  (check-equal? (grow snake1)
                (snake "left" (list (posn 4 5) (posn 5 5))))
  (check-equal? (grow snake0)
                (snake "right" (list (posn 2 1) 
                                     (posn 1 1))))
  
  (prop:new-goo-range 1000)
  
  (check-equal? (can-eat (snake "right" `(,(posn 3 3))) `(,(goo (posn 3 3) 130)))
                (goo (posn 3 3) 130))
  (check-false (can-eat (snake "right" `(,(posn 3 3))) `(,(goo (posn 3 4) 130)
                                                         ,(goo (posn 2 2) 0))))
  (check-equal? (can-eat snake0 (list (goo (posn 1 1) 1)))
                (goo (posn 1 1) 1))
  (check-false (can-eat snake0 (list (goo (posn 2 1) 1))))
  
  (check-equal? (slither snake0) (snake "right" (list (posn 2 1))))
  (check-equal? (slither (snake "right" (list (posn 4 4) 
                                              (posn 4 5)
                                              (posn 4 6))))
                (snake "right" (list (posn 5 4) (posn 4 4) (posn 4 5))))
  (check-equal? (slither snake0)
                (snake "right" (list (posn 2 1))))
  
  (check-equal? (length (eat (list (goo (posn 1 1) 130)) (goo (posn 1 1) 130)))
                1)
  (check-equal? (grow (snake "left" (list (posn 1 1))))
                (snake "left" (list (posn 0 1) (posn 1 1))))
  
  (check-equal? (next-head snake0) (posn 2 1))
  (check-equal? (next-head (snake "left" (list (posn 1 1))))
                (posn 0 1))
  (check-equal? (next-head (snake "up" (list (posn 1 1))))
                (posn 1 0))
  (check-equal? (next-head (snake "down" (list (posn 1 1)))) 
                (posn 1 2))
  (check-equal? (next-head snake0) (posn 2 1))
  
  (check-equal? (posn-move (posn 1 1) 2 3) (posn 3 4))
  (check-equal? (posn-move (posn 3 4) 6 0) (posn 9 4))
  (check-equal? (posn-move (posn 2 8) 0 5) (posn 2 13))
  (check-equal? (posn-move (posn 2 3) 0 0) (posn 2 3))
  
  (check-equal? (all-but-last '(1 2 3 4 5 6))
                '(1 2 3 4 5))
  (check-equal? (all-but-last (snake-segs snake2))
                `(,(posn 2 2) ,(posn 2 3)))
  (check-equal? (all-but-last (list 0)) empty)
  (check-equal? (all-but-last (list 0 1 2)) (list 0 1))
  
  
  (check-true (dir? "up"))
  (check-true (dir? "down"))
  (check-true (dir? "left"))
  (check-true (dir? "right"))
  (check-false (dir? "f"))
  (check-true (dir? "right"))
  
  (check-equal? (world-change-dir world1 "left") world1)
  (check-equal? (world-change-dir world1 "right") right-world1)
  (check-equal? (world-change-dir world0 "left") left-world0)
  (check-equal? (world-change-dir world0 "right") 
                (pit (snake "right" (snake-segs (pit-snake world0)))
                     (pit-goos world0)))
  (check-equal? (world-change-dir world0 "down")
                (pit (snake "down" (snake-segs (pit-snake world0)))
                     (pit-goos world0)))
  
  (check-true (opposite-dir? "up" "down"))
  (check-true (opposite-dir? "left" "right"))
  (check-true (opposite-dir? "right" "left"))
  (check-true (opposite-dir? "down" "up"))
  (check-false (opposite-dir? "left" "down"))
  (check-false (opposite-dir? "right" "down"))
  (check-false (opposite-dir? "down" "left"))
  (check-false (opposite-dir? "up" "right"))
  (check-true (opposite-dir? "up" "down"))
  (check-true (opposite-dir? "down" "up"))
  (check-false (opposite-dir? "up" "up") #f)
  (check-equal? (opposite-dir? "right" "left") #t)
  (check-equal? (opposite-dir? "left" "right") #t)
  
  
  (check-equal? (snake+scene snake1 MT-SCENE)
                (place-image HEAD-LEFT-IMG (* 5 SEG-SIZE)
                             (* 5 SEG-SIZE) MT-SCENE))
  (check-equal? (snake+scene snake2 MT-SCENE)
                (img+scene (posn 2 2) HEAD-UP-IMG 
                           (img+scene (posn 2 3) SEG-IMG 
                                      (img+scene (posn 2 4) SEG-IMG MT-SCENE))))
  (check-equal? (snake+scene (snake "up" (list (posn 1 1))) MT-SCENE)
                (img+scene (posn 1 1) HEAD-UP-IMG MT-SCENE))
  
  (check-equal? (goo-list+scene (list goo1) MT-SCENE)
                (place-image GOO-IMG (* 5 SEG-SIZE)
                             (* 5 SEG-SIZE) MT-SCENE))
  (check-equal? (goo-list+scene goo-list1 MT-SCENE)
                (img-list+scene (list (posn 5 5) (posn 4 8) (posn 6 9) (posn 1 1) (posn 1 9))
                                GOO-IMG MT-SCENE))
  
  (check-equal? (img-list+scene (list (posn 3 3) (posn 4 4)) SEG-IMG MT-SCENE)
                (place-image SEG-IMG (* 3 SEG-SIZE) (* 3 SEG-SIZE) 
                             (place-image SEG-IMG (* 4 SEG-SIZE) (* 4 SEG-SIZE) MT-SCENE)))
  (check-equal? (img-list+scene (list (posn 1 1) (posn 10 10)) SEG-IMG MT-SCENE)
                (place-image SEG-IMG (* 1 SEG-SIZE) (* 1 SEG-SIZE)  
                             (place-image SEG-IMG (* 10 SEG-SIZE) (* 10 SEG-SIZE) MT-SCENE)))
  (check-equal? (img-list+scene (list (posn 1 1)) GOO-IMG MT-SCENE)
                (place-image GOO-IMG SEG-SIZE SEG-SIZE
                             (img-list+scene empty GOO-IMG MT-SCENE)))
  
  (check-equal? (img+scene (posn 4 3) SEG-IMG MT-SCENE)
                (place-image SEG-IMG (* 4 SEG-SIZE) (* 3 SEG-SIZE)  MT-SCENE))
  (check-equal? (img+scene (posn 5 2) GOO-IMG MT-SCENE)
                (place-image GOO-IMG (* 5 SEG-SIZE) (* 2 SEG-SIZE)  MT-SCENE))
  (check-equal? (img+scene (posn 1 1) SEG-IMG MT-SCENE)
                (place-image SEG-IMG SEG-SIZE SEG-SIZE MT-SCENE))
  
  (check-false (self-colliding? snake1))
  (check-false (self-colliding? snake2))
  (check-false (self-colliding? wall-snake))
  (check-true (self-colliding? self-eating-snake))
  (check-false (self-colliding? snake0))
  (check-true (self-colliding? (snake (snake-dir snake0)
                                      (cons (posn 1 1) 
                                            (snake-segs snake0)))))
  
  (check-false (wall-colliding? snake1))
  (check-false (wall-colliding? snake2))
  (check-false (wall-colliding? self-eating-snake))
  (check-true (wall-colliding? wall-snake))
  (check-true 
   (wall-colliding? (snake "right" (list (posn (/ WIDTH-PX SEG-SIZE) 0)))))
  (check-true 
   (wall-colliding? (snake "down" (list (posn 0 (/ HEIGHT-PX SEG-SIZE))))))
  (check-true 
   (wall-colliding? (snake "up" (list (posn 1 0)))))
  (check-equal? (wall-colliding? (snake "right" 
                                        (list (posn 0 1))))
                true)
  (check-equal? (wall-colliding? (snake "right" 
                                        (list (posn 1 0))))
                true)
  (check-equal? (wall-colliding? (snake "right" 
                                        (list (posn 1 1))))
                false)
  (check-true (wall-colliding? (snake "right" (list (posn 1 SIZE)))))
  
 
  
  (check-false (posn=? (posn 1 1) (posn 2 2)))
  (check-false (posn=? (posn 1 2) (posn 2 1)))
  (check-true (posn=? (posn 3 4) (posn 3 4)))
  (check-true (posn=? (posn 2 2) (posn 2 2)))
  (check-equal? (posn=? (posn 1 2) (posn 1 1)) false)
  (check-equal? (posn=? (posn 1 2) (posn 1 2)) true)
  (check-equal? (posn-move (posn 0 0) 2 3) (posn 2 3))    
  
  (check-equal? (snake-head snake1) (posn 5 5))
  (check-equal? (snake-head snake2) (posn 2 2))
  (check-equal? (snake-head snake0) (posn 1 1))
  
  (check-equal? (snake-body snake1) empty)
  (check-equal? (snake-body snake0) empty)
  (check-equal? (snake-body snake2) (list (posn 2 3) (posn 2 4)))
  
  (check-equal? (snake-change-dir snake0 "up") 
                (snake "up" (list (posn 1 1))))
  (check-equal? (snake-change-dir snake1 "down") 
                (snake "down" (list (posn 5 5))))
  (check-equal? (snake-change-dir snake2 "left") 
                (snake "left" (list (posn 2 2) (posn 2 3) (posn 2 4))))
  
  (check-true (rotten? (goo (posn 1 2) 0)))
  (check-true (rotten? (goo (posn 6 9) 0)))
  (check-true (rotten? (goo (posn 23 2) 0)))
  
  (check-false (rotten? (goo (posn 1 2) 2)))
  (check-false (rotten? (goo (posn 3 45) 45334534)))
  (check-false (rotten? (goo (posn 2 4) 9)))
  
  (check-equal? (decay (goo (posn 1 2) 2))
                (goo (posn 1 2) 1))
  (check-equal? (decay (goo (posn 132 0) 2))
                (goo (posn 132 0) 1))
  (check-equal? (decay (goo (posn 1 2) 10))
                (goo (posn 1 2) 9))
  (check-equal? (decay (goo (posn 3 5) 8))
                (goo (posn 3 5) 7))
  
  "all tests run")
