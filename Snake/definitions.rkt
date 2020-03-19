#lang plai

(require 2htdp/image 2htdp/universe)

(struct pit (snake goos) #:transparent)

(struct snake (dir segs) #:transparent)

(struct goo (loc expire) #:transparent)

(struct posn (x y) #:transparent)

(define TICK-RATE 1/10)

(define SIZE 30)

(define SEG-SIZE 15)

(define MAX-GOO 5)
(define EXPIRATION-TIME 150)

(define WIDTH-PX  (* SEG-SIZE 30))
(define HEIGHT-PX (* SEG-SIZE 30))

(define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))
(define GOO-IMG (bitmap "graphics/goo.gif"))
(define SEG-IMG  (bitmap "graphics/body.gif"))
(define HEAD-IMG (bitmap "graphics/head.gif"))

(define HEAD-LEFT-IMG HEAD-IMG)
(define HEAD-DOWN-IMG (rotate 90 HEAD-LEFT-IMG))
(define HEAD-RIGHT-IMG (flip-horizontal HEAD-LEFT-IMG))
(define HEAD-UP-IMG (flip-vertical HEAD-DOWN-IMG))

(define ENDGAME-TEXT-SIZE 15)