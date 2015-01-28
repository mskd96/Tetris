#lang racket
(require racket/gui)
(require racket/vector)

;(play-sound "1.wav" `wav)
(define size 30);;;;;;change it to vary size of squares
(define rows 20);;;;;;change it to vary the number of rows 
(define columns 8);;;;;;change it to vary the number of columns
(define frame-size (* columns size))
(define frame-height (* rows size))
(define bitmap-size 750)
(define (concat l) (foldr append `() l))
(define left #f)
(define right #f)
(define rotate #f)
(define fast #f)
(define paused #f)
(define level 0)
(define over #f)
(define reset #f)
(define tetri-count (build-vector 7 (lambda (x) (cons (+ x 1) 0))))
(define-syntax lc
  (syntax-rules (: <- @)
    [(lc exp : var <- lexp) (map (lambda (var) exp) lexp)]
    [(lc exp : @ guard) (if guard (list exp) `())]
    [(lc exp : @ guard qualifier ...) 
     (concat (lc (lc exp : qualifier ...) : guard))]
    [(lc exp : var <- lexp qualifier ...) 
     (concat (lc (lc exp :  qualifier ... ) : var <- lexp))]))

;;;;;;;;;;;some 2d-vector functions
(define (make-2d-vector r c)
  (build-vector r (lambda (x) (make-vector c  #f))))

(define (2d-vector-count 2dvec proc)
  (define (helper vec)
    (vector-count proc vec))
  (foldr (lambda (x y) (+ (helper x) y)) 0 (vector->list 2dvec)))

(define (2d-vector-ref vec r c)
  (vector-ref (vector-ref vec r) c))

(define (2d-vector-set! vec r c val)
  (vector-set! (vector-ref vec r) c val))

(define (2d-vector-length vec)
  (cons (vector-length vec)
        (vector-length (vector-ref vec 0 ))))

(define (2d-vector-copy vec)
  (let ([new-vec (make-2d-vector (car (2d-vector-length vec)) (cdr (2d-vector-length vec)))])
    (begin 
      (lc (2d-vector-set! new-vec x y (2d-vector-ref vec x y)) : x <- (build-list (car (2d-vector-length vec)) (lambda (x) x))
          y <- (build-list (cdr (2d-vector-length vec)) (lambda (x) x)))
      new-vec)))

(define vec (make-2d-vector rows columns))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define frame (new frame% [label "TETRIS"];;;;;;;main frame
                   [min-width (+ frame-size 200)
                              ]
                   [min-height (+ (* 4 24) frame-height)
                    ]))

(define frame1 (new frame% [label "OUR-PROJECT"];;;;;frame asking about levels
                    [min-width (+ frame-size 200)]
                    [min-height frame-height]))
;;;;;;;;;;;;;frame and message of instructions
(define frame2 (new frame% [label "INSTRUCTIONS"]
                    [width 200]
                    [height 200]))
(new message% [parent frame2]
     [label "ROTATE : W ;
DOWN : S ; 
LEFT : A ;
RIGHT : D

PRESS THE START BUTTON IN MAIN FRAME
TO CLOSE THIS WINDOW AND CONTINUE THE GAME"])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define panel (new vertical-panel%
                   [parent frame1]
                   [alignment '(center center)]))
(define pause
  (new button% [parent frame]
       [label "PAUSE"]
       [callback (lambda (button event) 
                   (if paused (begin ;(set! over #f)
                                (set! paused #f)
                                (send pause set-label "PAUSE")
                                (send frame2 show #f))
                       (begin ;(set! over #t)
                         (set! paused #t)
                         (send pause set-label "START")
                         (send frame2 show #t))))]))
(define game-score 
  (new message%
       [parent frame]
       [label "SCORE : 0         "]))

(new button% [parent frame]
     [label "STOP"]
     [callback (lambda (button event)
                 (send frame2 show #f)
                 (set! over #t))])

(new button% [parent frame]
     [label "RESET"]
     [callback (lambda (button event)
                 (begin (set! reset #t)
                        (set! paused #f)
                        (send frame2 show #f)
                        (send pause set-label "PAUSE")
                        (send game-score set-label "SCORE : 0")
                        (set! abcd 0)
                        (tet)))])

(new button% [parent panel]
     [label "EASY"]
     [callback (lambda (button event) 
                 (begin (set! level 1)
                        (send frame1 show #f)
                        (send frame show #t)
                        (tet)
                        (vel)))])

(new button% [parent panel]
     [label "MEDIUM"]
     [callback (lambda (button event) (begin (set! level 2)
                                             (send frame1 show #f)
                                             (send frame show #t)
                                             (tet) (vel)))])
(new button% [parent panel]
     [label "HARD"]
     [callback (lambda (button event) (begin (set! level 3)
                                             (send frame1 show #f)
                                             (send frame show #t)
                                             (tet) (vel)))])

(send frame1 show #t)

(define mycanvas%
  (class canvas%
    (define/override (on-char event)
      (define pressed (send event get-key-code))
      (cond [(equal? pressed #\a) (set! left #t)]
            [(equal? pressed #\d) (set! right #t)]
            [(equal? pressed #\w) (set! rotate #t)]
            [(equal? pressed #\s) 
             (set! fast #t)]))
    (super-new)))

(define canvas
  (new mycanvas% [parent frame]
       [paint-callback
        (lambda (canvas dc) (paint dc))]))

(define (paint dc) (send dc draw-bitmap my-bitmap 0 0))

(define my-bitmap (make-object bitmap% bitmap-size bitmap-size ))
(define bm-dc (make-object bitmap-dc% my-bitmap))
(send bm-dc clear)
;;;;;;;;some pens and brushes
(define black-pen (make-object pen% "BLACK" 1 'solid))
(define yellow-brush (make-object brush% "YELLOW" 'solid))
(define blue-brush (make-object brush% "BLUE" 'solid))
(define red-brush (make-object brush% "RED" 'solid))
(define green-brush (make-object brush% "GREEN" 'solid))
(define orange-brush (make-object brush% "orange" 'solid))
(define violet-brush (make-object brush% "violet" 'solid))
(define lightblue-brush (make-object brush% "dodgerblue" 'solid))
(define no-brush (make-object brush% "LIME" 'transparent))

(define (draw-rectangle1 x y dc col)
  (send dc set-brush   (cond [(equal? col 1) yellow-brush]
                             [(equal? col 2) lightblue-brush]
                             [(equal? col 3) orange-brush]
                             [(equal? col 4) blue-brush]
                             [(equal? col 5) violet-brush]
                             [(equal? col 6) red-brush]
                             [(equal? col 7) green-brush]))
  (send dc set-pen black-pen) 
  (send dc draw-rectangle x y size size))

(define (draw-tetris 2d-vec dc next);;;;;;;this is the function which draws a given vector and the next tetri
  (begin 
    (send dc clear)
    (send dc draw-line (* size columns) 0 (* size columns) (* size rows))
    (lc (draw-rectangle1 (+ (* size y) 50 frame-size) (+ (- (/ frame-height 2) size) (* size x)) dc (2d-vector-ref next x y)) 
        : x <- (build-list (car (2d-vector-length next)) (lambda (x) x))
        y <- (build-list (cdr (2d-vector-length next)) (lambda (x) x)) @ (2d-vector-ref next x y)) 
    (lc (draw-rectangle1 y x dc (2d-vector-ref 2d-vec (/ x size) (/ y size)))
        : x <-(build-list rows (lambda (x) (* size x))) 
        y <- (build-list columns (lambda (x) (* size x))) @ 
        (2d-vector-ref 2d-vec (/ x size) (/ y size)))
    (send canvas refresh)
    (sleep/yield 0.01)
    ))
;;;;;;;;;;;;here are the 7 tetri's
(define tetri1 (build-vector 2 (lambda (x) (make-vector 2 1))))
(define tetri2 (build-vector 4 (lambda (x) (make-vector 4 (if (= x 0) 2 #f)))))
(define tetri3 (build-vector 3 (lambda (x) (build-vector 3 (lambda (y) (cond ((= x 0) 3)
                                                                             ((and (= x 1) (= y 0)) 3)
                                                                             (else #f)))))))
(define tetri4 (build-vector 3 (lambda (x) (build-vector 3 (lambda (y) (cond ((= x 0) 4)
                                                                             ((and (= x 1) (= y 2)) 4)
                                                                             (else #f)))))))
(define tetri5 (build-vector 3 (lambda (x) (build-vector 3 (lambda (y) (cond ((= x 0) 5)
                                                                             ((and (= x 1) (= y 1)) 5)
                                                                             (else #f)))))))
(define tetri6 (build-vector 3 (lambda (x) (build-vector 3 (lambda (y) (cond ((and (= x 0) (or (= y 0) (= y 1))) 6)
                                                                             ((and (= x 1) (or (= y 1) (= y 2))) 6)
                                                                             (else #f)))))))
(define tetri7 (build-vector 3 (lambda (x) (build-vector 3 (lambda (y) (cond ((and (= x 0) (or (= y 2) (= y 1))) 7)
                                                                             ((and (= x 1) (or (= y 1) (= y 0))) 7)
                                                                             (else #f)))))))

(define (tetris n)
  (cond [(= n 1) (build-vector 2 (lambda (x) (make-vector 2 1)))]
        [(= n 2) (build-vector 4 (lambda (x) (make-vector 4 (if (= x 0) 2 #f))))]
        [(= n 3) (build-vector 3 (lambda (x) (build-vector 3 (lambda (y) (cond ((= x 0) 3)
                                                                               ((and (= x 1) (= y 0)) 3)
                                                                               (else #f))))))]
        [(= n 4) (build-vector 3 (lambda (x) (build-vector 3 (lambda (y) (cond ((= x 0) 4)
                                                                               ((and (= x 1) (= y 2)) 4)
                                                                               (else #f))))))]
        [(= n 5) (build-vector 3 (lambda (x) (build-vector 3 (lambda (y) (cond ((= x 0) 5)
                                                                               ((and (= x 1) (= y 1)) 5)
                                                                               (else #f))))))]
        [(= n 6) (build-vector 3 (lambda (x) (build-vector 3 (lambda (y) (cond ((and (= x 0) (or (= y 0) (= y 1))) 6)
                                                                               ((and (= x 1) (or (= y 1) (= y 2))) 6)
                                                                               (else #f))))))]
        [(= n 7) (build-vector 3 (lambda (x) (build-vector 3 (lambda (y) (cond ((and (= x 0) (or (= y 2) (= y 1))) 7)
                                                                               ((and (= x 1) (or (= y 1) (= y 0))) 7)
                                                                               (else #f))))))]))
(define (rotate-tetri vec1)
  (let ([vec2 (rotate-vec vec1)]
        [len (2d-vector-length vec1)])
    (begin (lc (2d-vector-set! vec1 x y (2d-vector-ref vec2 x y)) :
               x <- (build-list (car len) (lambda (x) x))
               y <- (build-list (cdr len) (lambda (x) x)))
           vec1)))

(define (put 2dvec smallvec x1 y1)
  (let* ([l (2dvec->coord 2dvec)]
         [l1 (if (null? l) l (map car l))]
         [l2 (move x1 y1 (2dvec->coord smallvec))])
    (coord->vec (append l l2) (begin (clear 2dvec) 2dvec))))

(define (2dvec->coord 2dvec)
  (let* ([var (2d-vector-length 2dvec)])
    (lc (cons (cons y x) (2d-vector-ref 2dvec (/ x size) (/ y size)))
        : x <-(build-list (car var) (lambda (x) (* size x))) 
        y <- (build-list (cdr var) (lambda (x) (* size x))) @ 
        (2d-vector-ref 2dvec (/ x size) (/ y size)))))
(define (move dx dy list);;;;;;;list is the list of (cons coordinates tetri-no)
  (map (lambda (x) (cons (cons (+ (caar x) (* size dx)) (+ (cdar x) (* size dy))) (cdr x))) list))
(define (check-coord list1 list2);;;;;;;given 2 lists it checks if any of the elements in a list are equal to any element in the other
  (not (ormap (lambda (x)
                (ormap (lambda (y) (equal? y x)) list2)) list1)))

(define (remove-tet coord list-tet)
  (filter (lambda (x) (check-coord (list x) list-tet)) coord))
(define (check 2dvec smallvec x1 y1 comm);;;;;;this is the checking functin which decides whether a tetri should perform a move 
  (let* ([l1 (2dvec->coord 2dvec)]
         [tet-list (move x1 y1 (2dvec->coord smallvec))]
         [l2 (remove-tet l1 tet-list)]
         [l3 (cond [(equal? 'down comm) (move 0 1 tet-list)]
                   [(equal? 'left comm) (move -1 0 tet-list)]
                   [(equal? 'rotate comm) (move x1 y1 (2dvec->coord (rotate-vec smallvec)))]
                   [(equal? 'right comm) (move 1 0 tet-list)])])
    (and (check-coord (map car l2) (map car l3))
         (boundaries l3))))

(define (boundaries coord);;;;;;;given a  list of (cons coordinates tetri-no) checks if all coordinates are in the boundary
  (let* ([l1 (map caar coord)]
         [l2 (map cdar coord)]
         [l3 (andmap (lambda (x) (and (<= x (* size (- columns 1))) (>= x 0))) l1)]
         [l4 (andmap (lambda (x) (and (<= x (* size (- rows 1))) (>= x 0))) l2)])
    (and l3 l4)))

(define (coord->vec 2dlist 2dvec)
  (map (lambda (x) (2d-vector-set! 2dvec (/ (cdar x) size) (/ (caar x) size) (cdr x))) 2dlist))

(define (fall 2dvec smallvec x1 y1 comm)
  (let* ([l1 (2dvec->coord 2dvec)]
         [tet-list (move x1 y1 (2dvec->coord smallvec))]
         [l2 (remove-tet l1 tet-list)]
         [l3 (cond [(equal? 'down comm) (move 0 1 tet-list)]
                   [(equal? 'left comm) (move -1 0 tet-list)]
                   [(equal? 'right comm) (move 1 0 tet-list)]
                   [(equal? 'rotate comm) (move x1 y1 (2dvec->coord (rotate-tetri smallvec)))])])
    (coord->vec (append l2 l3) (begin (clear 2dvec) 2dvec))))

(define (clear 2dvec);;;;;clear makes the vector full of #f's
  (lc (2d-vector-set! 2dvec x y #f) : x <-(build-list (car (2d-vector-length 2dvec)) (lambda (x) x)) 
      y <- (build-list (cdr (2d-vector-length 2dvec)) (lambda (x) x)) ))

(define (rotate-vec 2dvec)
  (list->2dvector (transpose (reverse (2dvector->list 2dvec)))))
(define (transpose m);m is a 2d-list
  (define (abc m)
    (if (null? m) '()
        (cons (caar m) (abc (cdr m)))))
  (define (bcd m)
    (if (null? m) '()
        (cons (cdar m) (bcd (cdr m)))))
  (if (null? (car m)) '()
      (cons (abc m) (transpose (bcd m)))))
(define (2dvector->list 2dvec)
  (let* ((2dlist (vector->list 2dvec)))
    (map (lambda (x) (vector->list x)) 2dlist)))
(define (list->2dvector list)
  (let* ((2dvec (list->vector list)))
    (vector-map (lambda (x) (list->vector x)) 2dvec)))

;;;;;;;;;;;;;;;;; these are for clearing the fully completed rows
(define (checkequal vec)
  (equal? columns (vector-count (lambda (x) x) vec)))

(define (fallbyone 2dvec num)
  (vector-set! (begin (map (lambda (x) (vector-set! 2dvec x (vector-ref 2dvec (- x 1)))) (reverse (cdr (build-list num (lambda (x) x))))) 2dvec)
               0 (make-vector columns #f)))

(define (entirecheck 2dvec)
  (define (helper num)
    (cond ((not (= num 0)) (cond ((checkequal (vector-ref 2dvec num)) (begin (fallbyone 2dvec (+ num 1)) (helper num)))
                                 (else (helper (- num 1)))))))
  (helper (- rows 1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;finding number of holes in a given vector
(define (fill8 vec);;;;;fills 8's in some of the positions of the vector (those which are #f and are not holes
  (define (helper 2dvec x y)
    (cond ((and (>= x 0) (< x rows) (>= y 0) (< y columns) (equal? #f (2d-vector-ref 2dvec x y))) 
           (begin (2d-vector-set! 2dvec x y 8) (helper 2dvec (+ x 1) y) (helper 2dvec (- x 1) y)
                  (helper 2dvec x (+ y 1)) (helper 2dvec x (- y 1))))))
  (define (loop x)
    (if (>= x columns) vec (if (2d-vector-ref vec 0 x) (loop (+ x 1)) (helper vec 0 x))))
  (loop 0))
(define (remove-8 2dvec)
  (begin 
    (vector-map! (lambda (x) (vector-map (lambda (y) (if (equal? y 8) #f y)) x)) 2dvec)
    (void))) 

(define (holes 2dvec)
  (2d-vector-count (begin (fill8 2dvec) 2dvec) (lambda (x) (equal? x #f))))

(define (hole? x y 2dvec);;;;;;checks if there is a hole at (x,y)
  (let* ((new (begin (fill8 2dvec) 2dvec))
         (element (2d-vector-ref new y x))
         (bool (equal? #f element)))
    (begin (remove-8 2dvec) bool)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (blockades 2dvec);;;;;finding number of blockades in a give 2d-vector
  (define a 0)
  (lc  (set! a (+ a 1))  :  y <-(build-list rows (lambda (x) x)) 
       x <- (build-list columns (lambda (x) x)) @ (and (2d-vector-ref 2dvec y x) (check-blockade x y 2dvec)))
  a)

(define (check-blockade x y 2dvec);;;;;;;;checks if a blockade is there at xth row yth column
  (define (helper x1)
    (cond [(>= x1 rows) #f]
          [(equal? #f (2d-vector-ref 2dvec x1 x)) #t]
          [else (helper (+ x1 1))]))
  (helper y))

(define (height 2dvec);;;;;;;;;finds the total height of a 2d-vector
  (foldr + 0 (lc (- rows y) : x <- (build-list columns (lambda (x) x))
                 y <- (build-list rows (lambda (x) x)) @ (2d-vector-ref 2dvec y x))))

(define (clears vec);;;;;;;finds the number of clears in agiven 2d-vector
  (vector-count (lambda (x) (checkequal x)) vec))

(define combo 0);;;;;;combo is the count of consecutive clears
(define abcd 0);;;;abcd stores the score at any given instant

(define (score-rules clears)
  (if (= clears 0) 0 (- (* 100 clears) 50)))

(define (score-display vec y drop)
  (let ([line-clear (clears vec)])
    (begin 
      (set! combo (if (= line-clear 0) 0 (+ combo line-clear)))
      (set! abcd (+ abcd (*  (if drop 1 0.5) (* level (+ y (score-rules line-clear) (if (= combo line-clear) 0 (* 50 combo))) ))))
      (send game-score set-label (string-append "SCORE : " (number->string 
                                                            abcd))))))
                  
(define (choose);;;;;;randomly choosing tetri's (level 2)
  (let* ([i (random 7)])
    (cond [(= i 0) tetri1]
          [(= i 1) tetri2]
          [(= i 2) tetri3]
          [(= i 3) tetri4]
          [(= i 4) tetri5]
          [(= i 5) tetri6]
          [(= i 6) tetri7])))

(define (choose2 vec tet)
  (cond [(= level 2) (choose)]
        [else 
         (let ([tet-no (help1 vec tet level)])
           (if (equal? tet-no (tetri-no tet))
               (begin (set! consec-tetri (+ 1 consec-tetri))
                      (if (> consec-tetri 1) (choose) (tetris tet-no)))
               (begin (set! consec-tetri 0)
                      (tetris tet-no))))]))

(define consec-tetri 0)

(define (tetri-no vec)
  (define (help num) (ormap (lambda (x) x) (vector->list (vector-ref vec num))))
  (define l (vector-length vec))
  (cond ((equal? 2 l) (or (help 0) (help 1)))
        ((equal? 3 l) (or (help 0) (help 1) (help 2)))
        ((equal? 4 l) (or (help 0) (help 1) (help 2) (help 3)))))

(define (restore vec)
  (if (equal? (tetris (tetri-no vec)) vec) vec (restore (rotate-tetri vec))))

(define next-tetri
  (let ([tet (choose)])
    (begin (vector-set! tetri-count (- (tetri-no tet) 1) 
                        (cons (tetri-no tet) (+ (cdr (vector-ref tetri-count (- (tetri-no tet) 1))) 1)))
           tet)))
(define (gameover 2dvec bool smallvec)
  (let* ([tet-coord (move (- (quotient columns 2) 2) 0 (2dvec->coord (tetris (tetri-no smallvec))))]) 
    (if bool (not (check-coord (map car tet-coord) (map car (2dvec->coord 2dvec))))
        #f)))
(define (vel) (cond [(= level 1) 40]
                    [(= level 2) 25]
                    [(= level 3) 15]))
(define k #f)
(define (tet);;;;;;;;;;;every time program enters the loop i increases by 1.the tetri moves down once in some fixed 
  ;;;;;;;;;;;;;;;;;;;;;;number of loops which depends on the level.
  ;;;;;;;;;;;;;;;;;;;;;;2dvec is the 2d-vector,smallvec is the current tetri,x1,y1 are the positions of it,
  ;;;;;;;;;;;;;;;;;;;;;;bool is true if a new tetri is to be called
  (define (autofall 2dvec smallvec x1 y1 bool i)
    (if (or over (gameover 2dvec bool next-tetri)) (if k (void) 
                                                     (begin (send frame show #f) 
                                                            (display "GAME OVER") (set! k #t)
                                                            (newline)))
        (begin
          (cond [bool (begin (set! smallvec next-tetri)                
                             (put 2dvec smallvec (- (quotient columns 2) 2) 0)                       
                             (set! next-tetri (choose2 vec smallvec))
                             
                             (cond [(= (car (vector-argmax cdr tetri-count)) (tetri-no next-tetri)) 
                                    (set! next-tetri (tetris (car (vector-argmin cdr tetri-count))))])
                             (vector-set! tetri-count (- (tetri-no next-tetri) 1) 
                                          (cons (tetri-no next-tetri) (+ (cdr (vector-ref tetri-count (- (tetri-no next-tetri) 1))) 1)))
                             (set! x1 (- (quotient columns 2) 2))
                             (set! y1 0)
                             (set! bool #f)
                             (set! fast #f))]
                [reset (begin (set! bool #t)
                              (clear vec)
                              (set! reset #f))]
                [(and left (not paused) (not (= 0 (modulo i (cond [fast 1] (else (vel))))))) 
                 (begin (set! left #f)
                        (cond [(check 2dvec smallvec x1 y1 'left) 
                               (begin (fall 2dvec smallvec x1 y1 'left) (set! x1 (- x1 1)))]))]
                [(and right (not paused) (not (= 0 (modulo i (cond [fast 1] (else (vel))))))) 
                 (begin (set! right #f)
                        (cond [(check 2dvec smallvec x1 y1 'right)
                               (begin (fall 2dvec smallvec x1 y1 'right) (set! x1 (+ x1 1)))]))]
                [(and rotate (not paused) (not (= 0 (modulo i (cond [fast 1] (else (vel)))))))                                                                                                                                        
                 (begin (set! rotate #f)
                        (cond [(check 2dvec smallvec x1 y1 'rotate)
                               (fall 2dvec smallvec x1 y1 'rotate)]
                              [(check 2dvec smallvec x1 y1 'down)
                               (begin (fall 2dvec smallvec x1 y1 'down)
                                      (set! y1 (+ y1 1)))]))]
                [(check 2dvec smallvec x1 y1 'down) (cond [(= 0 (modulo i (cond [paused (+ i 1)]
                                                                                [fast 1]
                                                                                (else (vel))))) 
                                                           (begin (fall 2dvec smallvec x1 y1 'down)
                                                                  (set! rotate #f)
                                                                  (set! left #f)
                                                                  (set! right #f)
                                                                  (set! fast #f)
                                                                  (set! y1 (+ y1 1)))])]
                (else (cond [(= 0 (modulo i (quotient (vel) 2)))
                            (begin (score-display vec y1 fast)
                                   (entirecheck 2dvec)
                                   (restore smallvec)
                                   (set! bool #t))])))
          
          (draw-tetris 2dvec bm-dc (tetris (tetri-no next-tetri)));;;;this calls the draw-tetris function which draws the 2d-vector
          (autofall 2dvec smallvec x1 y1 bool (+ i 1))
          )))
  (autofall vec tetri1 0 0 #t 0))

;;;;;;;;;;;;;;;;;;;algo part
(define (score vec);;;;;;;;;;score of a 2d-vector is calculated here 
  (let ([a (height vec)]
        [b (clears vec)]
        [d (blockades vec)]
        [c (holes vec)])
    (begin (remove-8 vec)
           (entirecheck vec)
           (+ (* a -5) (* b 3) (* c -0.5) (* d -1)))))

(define tetrilist (build-list 7 (lambda (x) (tetris (+ x 1)))))

(define (all-rotations tetri);;;;gives all possible rotations of a given tetri
  (let ([x (tetri-no tetri)])
    (cond [(equal? x 1) (list tetri)]
          [(or (= x 2) (= x 6) (= x 7)) (list tetri (rotate-vec tetri))]
          (else (list tetri (rotate-vec tetri) (rotate-vec (rotate-vec tetri)) (rotate-vec (rotate-vec (rotate-vec tetri))))))))

(define rot-tetrilist (map (lambda (x) (all-rotations x)) tetrilist));;;;;this contains all tetri's(includes rotated tetri's also)

;(define (countvec1 xyz);;;;;;;;;;;taking most frequent of most hard 30
;  (define vec (build-vector 8 (lambda (x) (cons 0 x))))
;  (define (helper rem)
;    (if (null? rem) (cdr (vector-argmax car vec))
;        (begin (vector-set! vec (caar rem) (cons (+ 1 (car (vector-ref vec (caar rem)))) (caar rem)))  
;               (helper (cdr rem)))))
;  (helper (take (sort xyz #:key cdr <) (if (< (length xyz) 30) (length xyz) 30))))

(define (countvec2 xyz level);;;;;;;;;;;taking least or most frequent of most easy 30
  (define vec (build-vector 8 (lambda (x) (cons 0 x))))
  (define (helper rem)
    (if (null? rem) (cdr ((if level vector-argmax vector-argmin) car (list->vector (reverse (vector->list (vector-drop vec 1))))))
        (begin (vector-set! vec (caar rem) (cons (+ 1 (car (vector-ref vec (caar rem)))) (caar rem)))  
               (helper (cdr rem)))))
  (helper (take-suff (sort xyz #:key cdr >) (if level 5 30))))

(define (take-suff list x)  
  (take list (if (< (length list) x) (length list) x)))

(define (help1 vec tetri level)
  (countvec2 (concat (matchall (map car (take-suff (sort (concat (map (lambda (x) (match- vec x)) (all-rotations tetri))) #:key cdr <) 5)))) 
             (= level 1)))

(define (matchall 2dveclist) ; to give all possible combinations of 2d-vectors including rotation
  (lc (map (lambda (z) (cons (tetri-no y) (cdr z))) (match- x y))
      : x <- 2dveclist y <- (concat rot-tetrilist)))


(define (hole-overlap vec smallvec x1 y1)
  (andmap (lambda (x) x) (lc (hole? (+ x1 x) (+ y1 y) vec) : x <- (build-list (cdr (2d-vector-length smallvec)) (lambda (x) x))
                             y <- (build-list (car (2d-vector-length smallvec)) (lambda (x) x))
                             @ (2d-vector-ref smallvec y x))))
(define (check-help coord-vec coord-tet)
  (and (check-coord (map car coord-vec) (map car coord-tet)) 
       (not (and (boundaries (move 0 1 coord-tet))
                 (check-coord (map car coord-vec) (map car (move 0 1 coord-tet)))))))
(define (match- vec smallvec);;;;;;;;;;given a 2d-vector and a tetri it gives all possible combinations along with score of each of them 
  (let ([x (2d-vector-length smallvec)]
        [coord-vec (2dvec->coord vec)]
        [coord-tet (2dvec->coord smallvec)])
    (lc (let ([vec1 (2d-vector-copy vec)])
          (begin (put vec1 smallvec x y)
                 (cons vec1 (score vec1))))
        : x <- (build-list (+ columns 4) (lambda (x) (- x 4)))
        y <- (build-list (+ 4 rows) (lambda (x) (- x 4)))
        @ (and (boundaries (append coord-vec (move x y coord-tet)))
               (check-help coord-vec (move x y coord-tet))
               (not (hole-overlap vec smallvec x y))))))