;; Code for CS61A project 2 -- picture language

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
	(beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1))))
	(let ((top-left (beside up up))
	      (bottom-right (below right right))
	      (corner (corner-split painter (- n 1))))
	  (beside (below painter top-left)
		  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
	  (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (identity x) x)

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
				  identity flip-vert)))
    (combine4 painter)))

;; or

; (define flipped-pairs
;   (square-of-four identity flip-vert identity flip-vert))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
				  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
			   (edge1-frame frame))
	       (scale-vect (ycor-vect v)
			   (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
	((frame-coord-map frame) (start-segment segment))
	((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define (draw-line v1 v2)
  (penup)
  (setxy (- (* (xcor-vect v1) 200) 100)
	 (- (* (ycor-vect v1) 200) 100))
  (pendown)
  (setxy (- (* (xcor-vect v2) 200) 100)
	 (- (* (ycor-vect v2) 200) 100)))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
	(painter
	 (make-frame new-origin
		     (sub-vect (m corner1) new-origin)
		     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
		    (make-vect 0.5 0.5)
		    (make-vect 1.0 0.5)
		    (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
		     (make-vect 0.0 0.0)
		     (make-vect 0.65 0.35)
		     (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      split-point
			      (make-vect 0.0 1.0)))
	  (paint-right
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.0)
			      (make-vect 0.5 1.0))))
      (lambda (frame)
	(paint-left frame)
	(paint-right frame)))))

;;
;; Your code goes here
;;

(define full-frame (make-frame (make-vect -0.5 -0.5)
			       (make-vect 2 0)
			       (make-vect 0 2)))

(define (up-split painter n)
	(if (= n 0)
	    painter
		(let ((smaller (up-split painter (- n 1))))
			(below painter (beside smaller smaller)))))

(define (split outer-proc inner-proc)
	(define (helper painter n)
		(if (= n 0)
		    painter
			(let ((smaller (helper painter (- n 1))))
				(outer-proc (inner-proc smaller smaller)))))
	helper)

; ; 2.46
(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect v1 v2)
	(make-vect (+ (xcor-vect v1) (xcor-vect v2))
	           (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
	(make-vect (- (xcor-vect v1) (xcor-vect v2))
	           (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
	(make-vect (* s (xcor-vect v))
	           (* s (ycor-vect v))))

; ; 2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
; selectors ↓
(define frame-origin car)
(define frame-egde1 cadr)
(define frame-edge2 caddr)

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
; selectors ↓
(define frame-origin car)
(define frame-edge1 cadr)
(define frame-edge2 cddr)

; 2.48
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

; 2.49
; outline painter
(define base1-segment 
	(make-segment (make-vect 0 0) (make-vect 1 0)))
(define base2-segment
	(make-segment (make-vect 0 1) (make-vect 1 1)))
(define height1-segment
	(make-segment (make-vect 0 0) (make-vect 0 1)))
(define height2-segment
	(make-segment (make-vect 1 0) (make-vect 1 1)))
(define outline-painter
	(segments->painter (list base1-segment base2-segment height1-segment height2-segment)))

;x painter
(define diagonal1
	(make-segment (make-vect 0 1) (make-vect 1 0)))
(define diagonal2
	(make-segment (make-vect 1 1) (make-vect 0 0)))
(define x-painter
	(segments->painter (list diagonal1 diagonal2)))

; diamond painter
(define seg1
	(make-segment (make-vect 0.5 1.0) (make-vect 1.0 0.5)))
(define seg2
	(make-segment (make-vect 1 0.5) (make-vect 0.5 0)))
(define seg3
	(make-segment (make-vect 0.5 0) (make-vect 0 0.5)))
(define seg4
	(make-segment (make-vect 0 0.5) (make-vect 0.5 1)))
(define diamond-painter
	(segments->painter (list seg1 seg2 seg3 seg4)))

; ; 2.50
; flip horiz
(define (hflip-painter painter)
	(transform-painter painter
					   (make-vect 0 1)
					   (make-vect 1 1)
					   (make-vect 0 0)))
(define (rotate180 painter)
	(transform-painter painter 
					   (make-vect 1 1)
					   (make-vect 0 1)
					   (make-vect 1 0)))
(define (rotate270 painter)
	(transform-painter painter
					   (make-vect 1 0)
					   (make-vect 1 1)
					   (make-vect 0 0)))

; 2.51
(define (below painter1 painter2)
	(let ((paint-up
		  	(transform-painter painter2
							   (make-vect 0 0.5)
							   (make-vect 1 0.5)
							   (make-vect 0 1)))
		  (paint-down
		  	(transform-painter painter1
			                   (make-vect 0 0)
							   (make-vect 1 0)
							   (make-vect 0 0.5))))
		(lambda (frame)
			(paint-up frame)
			(paint-down frame))))

(define (below painter1 painter2)
	(rotate90 (beside (rotate270 painter1) (rotate270 painter2))))

