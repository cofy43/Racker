#lang plai

(require 2htdp/image 2htdp/universe)

(require "definitions.rkt")
(require "auxiliar.rkt")

(define (self-colliding? sn)
  (cons? (member (snake-head sn) (snake-body sn))))

(define (wall-colliding? sn)
  (define x (posn-x (snake-head sn)))
  (define y (posn-y (snake-head sn)))
  (or (= 0 x) (= x SIZE)
      (= 0 y) (= y SIZE)))