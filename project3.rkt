#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")

;; We'll use the same version of Some and Optional that we used on Homework 5.
(define-struct (Some X)
  ([value : X]))
    
(define-type (Optional X)
  (U 'none (Some X)))

;; The game has two players, who we'll call 'black and 'white. You can choose
;; any color or pattern you would like for the pieces, but we'll still call
;; the players by these names.
(define-type Player (U 'black 'white))

;; (Pos row col) represents a position on the game board. The lower-left corner
;; is at row 0, column 0. The lower-right corner is at row 0, column 6. The
;; upper-left corner is at row 5, column 0. The upper-right corner is at row 5,
;; column 6.
(define-struct Pos
  ([row : Integer]   ;; integer between 0 and 5 inclusive
   [col : Integer])) ;; integer between 0 and 6 inclusive

;; (Stack height pieces) represents one full column of the game board. The
;; integer height is the number of pieces currently in that column. The
;; list pieces is a list of the pieces in the column (or more precisely, a list
;; of the players who placed the pieces). The last element of the list is the
;; bottom piece in the stack, while the first element of the list is the top
;; piece in the stack (so far). The value of height should alway match the
;; length of the list pieces.
(define-struct Stack
  ([height : Integer]
   [pieces : (Listof Player)]))

;; (Board stacks) represents a game board. The list stacks will always have
;; seven elements, representing the seven columns of the game board. The first
;; element of stacks represents the leftmost column of the board.
(define-struct Board
  ([stacks : (Listof Stack)]))

;; (Game board next) represents the state of the game at a given moment of time.
;; The current state of the game board is saved in board. The player whose turn
;; it is currently is stored in next.
(define-struct Game
  ([board : Board]
   [next : Player]))

;; If a player has won the game by creating a line of four, then
;; (Winning-Line player start end) can be used to keep track of which player
;; created the line of four, and where that line is (it goes from start to end).
;; Generally, if a player's winning move creates more than one line of four, we
;; won't care which one gets represented.
(define-struct Winning-Line
  ([player : Player]
   [start : Pos]
   [end : Pos]))

(define-struct CircleDetails
  ;; Details of board image
  ;; Easier to customize
  ([radius : Integer]
   [p1-color : Image-Color]
   [p2-color : Image-Color]
   [bg-color : Image-Color]))

(define-type Dir (U 'vert 'hori 'diagUp 'diagDown))
;; Used to organize later functions that search the board at a given direction

(define-type Strategy (Game -> Integer))
;; Chooses column

(define-struct Human
  ;; Player name that is either string or symbol
  ([name : (U String Symbol)]))

(define-struct Bot
  ;; Bot name and strategy it uses
  ([name : (U String Symbol)]
   [strategy : Strategy]))

(define-struct World
  ;; World struct that stores game state, distance between centers,
  ;; controller 1, controller 2, and whether or not the board has an error.
  ([game : Game]
   [distance : Integer]
   [controller1 : Controller]
   [controller2 : Controller]
   [error-board : Boolean]))

(define-type Controller (U Human Bot))

(define-type Heuristic (Game -> Integer))

(define-struct StackTester
  ([height : Integer]
   [pieces : (U (Listof Player) 'empty)]))

(define-struct BoardTester
  ([stacks : (Listof StackTester)]))

(define-struct Node
  ([value : Game]
   [children : (Listof Node)]))

(define-struct SomeReal
  ([r : Real]))

;; VARIABLES FOR TESTING

(: b1 : Board)
(define b1 (Board (list
                   (Stack 4 (list 'black 'white 'white 'black 'white))
                   (Stack 3 (list 'black 'white 'black 'black))
                   (Stack 4 (list 'black 'white 'white 'black 'black))
                   (Stack 2 (list 'white 'black 'black))
                   (Stack -1 (list))
                   (Stack 1 (list 'black 'white))
                   (Stack 0 (list 'white)))))

(: b2 : Board)
(define b2 (Board (list
                   (Stack 5 (list 'black 'white 'white 'black 'white 'black))
                   (Stack 3 (list 'black 'white 'black 'black))
                   (Stack 4 (list 'black 'white 'white 'black 'black))
                   (Stack 2 (list 'white 'black 'black))
                   (Stack -1 (list))
                   (Stack 1 (list 'black 'white))
                   (Stack 0 (list 'white)))))

(: b3 : Board)
(define b3 (Board (list
                   (Stack 4 (list 'black 'white 'white 'black 'white))
                   (Stack 4 (list 'black 'black 'white 'black 'black))
                   (Stack 4 (list 'black 'white 'white 'black 'black))
                   (Stack 2 (list 'white 'black 'black))
                   (Stack -1 (list))
                   (Stack 1 (list 'black 'white))
                   (Stack 0 (list 'white)))))
(: b4 : Board)
(define b4 (Board (list
                   (Stack 5 (list 'black 'white 'black 'black 'black 'black))
                   (Stack 4 (list 'black 'white 'black 'white 'black))
                   (Stack 5 (list 'white 'black 'black 'black 'black 'white))
                   (Stack 2 (list 'white 'black 'white))
                   (Stack 5 (list 'white 'white 'white 'white 'black 'white))
                   (Stack 1 (list 'black 'white))
                   (Stack 5 (list 'white 'white 'white 'white 'white 'white)))))
(: b5 : Board)
(define b5 (Board (list
                   (Stack 5 (list 'black 'white 'black 'black 'white 'black))
                   (Stack 4 (list 'black 'black 'black 'white 'black))
                   (Stack 5 (list 'white 'black 'black 'white 'black 'white))
                   (Stack 3 (list 'white 'black 'white 'black))
                   (Stack 5 (list 'white 'white 'white 'white 'black 'white))
                   (Stack 1 (list 'black 'white))
                   (Stack 0 (list 'black)))))
(: b6 : Board)
(define b6 (Board (list
                   (Stack 4 (list 'black 'white 'black 'white 'black))
                   (Stack 3 (list 'white 'black 'white 'black))
                   (Stack 2 (list 'black 'white 'white))
                   (Stack 2 (list 'white 'black'black))
                   (Stack 5 (list 'white 'black 'white 'black 'white 'white))
                   (Stack 4 (list 'white 'white 'black 'white 'black))
                   (Stack 4 (list 'black 'white 'black 'white 'black)))))
(: b7 : Board)
(define b7 (Board (list
                   (Stack 5 (list 'white 'white 'white 'white 'white 'white))
                   (Stack 5 (list 'white 'white 'white 'white 'white 'white))
                   (Stack 5 (list 'white 'white 'white 'white 'white 'white))
                   (Stack 5 (list 'white 'white 'white 'white 'white 'white))
                   (Stack 5 (list 'white 'white 'white 'white 'white 'white))
                   (Stack 5 (list 'white 'white 'white 'white 'white 'white))
                   (Stack 5 (list 'white 'white 'white 'white 'white 'white)))))
(: b8 : Board)
(define b8 (Board (list
                   (Stack 5 (list 'black 'white 'white 'white 'black 'white))
                   (Stack 5 (list 'white 'black 'white 'black 'white 'black))
                   (Stack 5 (list 'black 'white 'white 'white 'black 'white))
                   (Stack 5 (list 'black 'white 'black 'black 'black 'white))
                   (Stack 5 (list 'white 'black 'white 'white 'white 'black))
                   (Stack 5 (list 'black 'black 'white 'black 'black 'black))
                   (Stack 5 (list 'white 'white 'white 'black 'white 'white)))))

(: b9 : Board)
(define b9 (Board (list
                   (Stack -1 (list))
                   (Stack 2 (list 'black 'black 'black))
                   (Stack 0 (list 'black))
                   (Stack 2 (list 'black 'white 'black))
                   (Stack 2 (list 'black 'white 'white))
                   (Stack 1 (list 'white 'black))
                   (Stack -1 (list)))))

(: b10 : Board)
(define b10 (Board (list
                    (Stack -1 (list))
                    (Stack 3 (list 'black 'black 'black 'black))
                    (Stack 0 (list 'black))
                    (Stack 2 (list 'black 'white 'black))
                    (Stack 2 (list 'black 'white 'white))
                    (Stack 1 (list 'white 'black))
                    (Stack -1 (list)))))

(: b11 : Board)
(define b11 (Board (list
                    (Stack -1 (list))
                    (Stack 2 (list 'black 'black 'black))
                    (Stack 0 (list 'black))
                    (Stack 2 (list 'black 'white 'black))
                    (Stack 2 (list 'black 'white 'white))
                    (Stack 1 (list 'white 'black))
                    (Stack 3 (list 'white 'white 'white 'white)))))

(: b12 : Board)
(define b12 (Board (list
                    (Stack 0 (list 'white))
                    (Stack 1 (list 'black 'black))
                    (Stack 0 (list 'black))
                    (Stack 0 (list 'black))
                    (Stack 0 (list 'white))
                    (Stack 0 (list 'white))
                    (Stack -1 (list)))))

(: b13 : Board)
(define b13 (Board (list
                    (Stack -1 (list))
                    (Stack 0 (list 'black))
                    (Stack -1 (list))
                    (Stack 0 (list 'black))
                    (Stack -1 (list))
                    (Stack -1 (list))
                    (Stack -1 (list)))))

(: b14 : Board)
(define b14 (Board (list
                    (Stack 5 (list 'black 'white 'black 'white 'black 'white))
                    (Stack 5 (list 'black 'white 'black 'white 'black 'white))
                    (Stack 0 (list 'black))
                    (Stack -1 (list))
                    (Stack -1 (list))
                    (Stack -1 (list))
                    (Stack -1 (list)))))

(: b15 : Board)
(define b15 (Board (list
                    (Stack -1 (list))
                    (Stack 2 (list 'white 'black 'white))
                    (Stack 1 (list 'white 'black))
                    (Stack 0 (list 'white))
                    (Stack -1 (list))
                    (Stack -1 (list))
                    (Stack -1 (list)))))

(: new-game-tree-children : (Listof Node))
(define new-game-tree-children
  (list
   (Node (Game (Board (list (Stack 0 '(black)) (Stack -1 '()) (Stack -1 '())
                            (Stack -1 '()) (Stack -1 '()) (Stack -1 '())
                            (Stack -1 '()))) 'white) '())
   (Node (Game (Board (list (Stack -1 '()) (Stack 0 '(black)) (Stack -1 '())
                            (Stack -1 '()) (Stack -1 '()) (Stack -1 '())
                            (Stack -1 '()))) 'white) '())
   (Node (Game (Board (list (Stack -1 '()) (Stack -1 '()) (Stack 0 '(black))
                            (Stack -1 '()) (Stack -1 '()) (Stack -1 '())
                            (Stack -1 '()))) 'white) '())
   (Node (Game (Board (list (Stack -1 '()) (Stack -1 '()) (Stack -1 '())
                            (Stack 0 '(black)) (Stack -1 '()) (Stack -1 '())
                            (Stack -1 '()))) 'white) '())
   (Node (Game (Board (list (Stack -1 '()) (Stack -1 '()) (Stack -1 '())
                            (Stack -1 '()) (Stack 0 '(black)) (Stack -1 '())
                            (Stack -1 '()))) 'white) '())
   (Node (Game (Board (list (Stack -1 '()) (Stack -1 '()) (Stack -1 '())
                            (Stack -1 '()) (Stack -1 '()) (Stack 0 '(black))
                            (Stack -1 '()))) 'white) '())
   (Node (Game (Board (list (Stack -1 '()) (Stack -1 '()) (Stack -1 '())
                            (Stack -1 '()) (Stack -1 '()) (Stack -1 '())
                            (Stack 0 '(black)))) 'white) '())))

(: t1 : Node)
(define t1
  (Node
   (Game b9 'black)
   (list
    (Node (Game (Board (list (Stack 0 '(black)) (Stack 2 '(black black black))
                             (Stack 0 '(black)) (Stack 2 '(black white black))
                             (Stack 2 '(black white white))
                             (Stack 1 '(white black)) (Stack -1 '()))) 'white)
          '())
    (Node (Game (Board (list (Stack -1 '()) (Stack 3 '(black black black black))
                             (Stack 0 '(black)) (Stack 2 '(black white black))
                             (Stack 2 '(black white white))
                             (Stack 1 '(white black)) (Stack -1 '()))) 'white)
          '())
    (Node (Game (Board (list (Stack -1 '()) (Stack 2 '(black black black))
                             (Stack 1 '(black black))
                             (Stack 2 '(black white black))
                             (Stack 2 '(black white white))
                             (Stack 1 '(white black)) (Stack -1 '()))) 'white)
          '())
    (Node (Game (Board (list (Stack -1 '()) (Stack 2 '(black black black))
                             (Stack 0 '(black))
                             (Stack 3 '(black black white black))
                             (Stack 2 '(black white white))
                             (Stack 1 '(white black)) (Stack -1 '())))
                'white) '())
    (Node (Game (Board (list (Stack -1 '()) (Stack 2 '(black black black))
                             (Stack 0 '(black)) (Stack 2 '(black white black))
                             (Stack 3 '(black black white white))
                             (Stack 1 '(white black)) (Stack -1 '()))) 'white)
          '())
    (Node (Game (Board (list (Stack -1 '()) (Stack 2 '(black black black))
                             (Stack 0 '(black)) (Stack 2 '(black white black))
                             (Stack 2 '(black white white))
                             (Stack 2 '(black white black)) (Stack -1 '())))
                'white) '())
    (Node (Game (Board (list (Stack -1 '()) (Stack 2 '(black black black))
                             (Stack 0 '(black)) (Stack 2 '(black white black))
                             (Stack 2 '(black white white))
                             (Stack 1 '(white black)) (Stack 0 '(black))))
                'white) '()))))

(: smallest-node : Node)
(define smallest-node (Node (Game b11 'white) '()))

(: largest-node : Node)
(define largest-node (Node (Game b10 'black) '()))

;; GAME LOGIC
;;
(: new-game : Game)
;; Define a game with no pieces on the board and black to move
(define new-game
  (Game (Board
         (make-list
          7 (Stack -1 (list))))
        'black))

(: column-search : Board Pos -> Stack)
;; Helper function that returns the stack wihtin a board that contains
;; a given coordinate value.
(define (column-search b p)
  (match* (b p) 
    [((Board l) (Pos rows cols)) (list-ref l cols) ]))

(check-expect (column-search b1 (Pos 3 4)) (Stack -1 (list)))
(check-expect (column-search b1 (Pos 3 5)) (Stack 1 (list 'black 'white)))
(check-expect (column-search b1 (Pos 3 6)) (Stack 0 (list 'white)))

(: board-ref : Board Pos -> (Optional Player))
;; If there is a piece at the given position, belonging to a Player x,
;; then board-ref returns (Some x). Otherwise, it returns 'none.
(define (board-ref b p)
  (if (<= 0 (Pos-col p) 6)
      (match* ((column-search b p) p)
        [((Stack h gs) (Pos rows cols))
         (cond [(or (< rows 0) (< cols 0) (< h rows)) 'none]
               [else (Some (list-ref gs (- h rows)))])])
      'none))

(check-expect (board-ref b1 (Pos 1 1)) (Some 'black))
(check-expect (board-ref b1 (Pos 0 0)) (Some 'white))
(check-expect (board-ref b1 (Pos 0 6)) (Some 'white))
(check-expect (board-ref b1 (Pos 3 6)) 'none)
(check-expect (board-ref b1 (Pos 0 4)) 'none)
(check-expect (board-ref b1 (Pos 8 4)) 'none)
(check-expect (board-ref b1 (Pos -8 4)) 'none)
(check-expect (board-ref b6 (Pos 0 0)) (Some 'black))
(check-expect (board-ref b7 (Pos 2 4)) (Some 'white))

(: valid-move? : Game Player Integer -> Boolean)
;; Takes in a game, a player, and a column number.
;; Returns true if the player can play in that column
(define (valid-move? g p1 c)
  (match g
    [(Game b p2) (match* ((board-ref b (Pos 5 c)) p1 p2)
                   [('none 'white 'white) #t]
                   [('none 'black 'black) #t]
                   [(_ _ _) #f])]))

(check-expect (valid-move? (Game b1 'black) 'black 6) #t)
(check-expect (valid-move? (Game b1 'black) 'white 6) #f)
(check-expect (valid-move? (Game b1 'black) 'black 1) #t)
(check-expect (valid-move? (Game b2 'black) 'black 0) #f)

(: change-stack : Game Player Integer -> Stack)
;; If valid, adds a given player to the board within a given
;; game at the given column index. If not valid, returns error.
(define (change-stack g p c)
  (if (valid-move? g p c)
      (match g
        [(Game b p) (match (column-search b (Pos 0 c))
                      [(Stack h ps) (Stack (+ h 1) (cons p ps))])])
      (error "Not a valid move!")))

(check-expect (change-stack (Game b1 'black) 'black 1)
              (Stack 4 (list 'black 'black 'white 'black 'black)))

(: replace-at : All (A) Integer A (Listof A) -> (Listof A))
;; Replace the item at the given position i in the list xs with the new item x
;; position counting starts at 0
(define (replace-at i x xs)
  (match* (xs i)
    [((cons xs xr) 0) (cons x (replace-at (- i 1) x xr))]
    [((cons xs xr) i) (cons xs (replace-at (- i 1) x xr))]
    [(_ _) '()]))

(: apply-move : Game Player Integer -> Game)
;; Given a game, a player, and a column number, apply-move takes care of the
;; player playing a piece in the given column, and returns the subsequent game
;; state. Otherwise, it returns an error
(define (apply-move g p1 c)
  (match g
    [(Game b p2)
     (match p2
       ['white (Game
                (Board ((inst replace-at Stack) c (change-stack g p2 c)
                                                (Board-stacks b))) 'black)]
       ['black (Game
                (Board ((inst replace-at Stack) c (change-stack g p2 c)
                                                (Board-stacks b))) 'white)])]))

(check-expect (apply-move (Game b1 'black) 'black 1) (Game b3 'white))
;;(check-error (apply-move (Game b1 'black) 'black 10) "Not a valid move!")

(: connect-four? : Board Pos Dir Player Natural -> Boolean)
;; Helper function that returns true if a given position within a given board
;; begins a line of four connected elements in a given direction.
(define (connect-four? b p d plyr i)
  (if (= i 4) #t
      (match* (p (board-ref b p))
        [((Pos row col) (Some x))
         (cond
           [(symbol=? d 'vert)
            (and (symbol=? x plyr)
                 (connect-four? b (Pos (+ row 1) col) d plyr (+ i 1)))]
           [(symbol=? d 'hori)
            (and (symbol=? x plyr)
                 (connect-four? b (Pos row (+ col 1)) d plyr (+ i 1)))]
           [(symbol=? d 'diagUp)
            (and (symbol=? x plyr)
                 (connect-four? b (Pos (+ row 1) (+ col 1)) d plyr (+ i 1)))]
           [(symbol=? d 'diagDown)
            (and (symbol=? x plyr)
                 (connect-four? b (Pos (- row 1) (+ col 1)) d plyr (+ i 1)))]
           [else #f])]
        [((Pos row col) 'none) #f])))

(check-expect (connect-four? b1 (Pos 0 0) 'vert 'black 0) #f)
(check-expect (connect-four? b4 (Pos 0 0) 'vert 'black 0) #t)
(check-expect (connect-four? b4 (Pos 0 1) 'vert 'white 0) #f)
(check-expect (connect-four? b4 (Pos 1 2) 'vert 'black 0) #t)
(check-expect (connect-four? b4 (Pos 1 3) 'vert 'black 0) #f)
(check-expect (connect-four? b4 (Pos 5 0) 'vert 'black 0) #f)
(check-expect (connect-four? b4 (Pos 1 3) 'hori 'black 0) #f)
(check-expect (connect-four? b4 (Pos 0 2) 'hori 'white 0) #t)
(check-expect (connect-four? b5 (Pos 5 0) 'diagDown 'black 0) #t)
(check-expect (connect-four? b5 (Pos 5 0) 'diagUp 'black 0) #f)
(check-expect (connect-four? b5 (Pos 3 0) 'hori 'black 0) #f)
(check-expect (connect-four? b7 (Pos 1 2) 'diagUp 'white 0) #t)
(check-expect (connect-four? b7 (Pos 1 2) 'vert 'white 0) #t)
(check-expect (connect-four? b7 (Pos 1 2) 'hori 'white 0) #t)
(check-expect (connect-four? b7 (Pos 1 2) 'diagDown 'white 0) #f)
(check-expect (connect-four? b7 (Pos 2 4) 'vert 'white 0) #t)
(check-expect (connect-four? b6 (Pos 1 2) 'diagUp 'white 0) #t)
(check-expect (connect-four? b4 (Pos 2 4) 'vert 'white 0) #t)
(check-expect (connect-four? b4 (Pos 1 2) 'vert 'black 0) #t)
(check-expect (connect-four? b4 (Pos 0 6) 'vert 'white 0) #t)
(check-expect (connect-four? b4 (Pos 1 6) 'vert 'white 0) #t)
(check-expect (connect-four? b4 (Pos 2 6) 'vert 'white 0) #t)

(: return-winning-line : Board Player Pos -> (U 'tie 'ongoing Winning-Line))
;; Helper function that returns the first Winning-Line that it finds in a given
;; board. If there are no winning lines, then it just returns 'ongoing. It may
;; actually be a 'tie, but these two cases will be accounted for in the next
;; function.
(define (return-winning-line b plyr p)
  (match p
    [(Pos rows cols)
     (cond
       [(and (> rows 5) (> cols 6)) 'ongoing]
       [(> rows 5) (return-winning-line b plyr (Pos 0 (+ 1 cols)))]
       [(connect-four? b p 'vert plyr 0)
        (Winning-Line plyr p (Pos (+ 3 rows) cols))]
       [(connect-four? b p 'hori plyr 0)
        (Winning-Line plyr p (Pos rows (+ 3 cols)))]
       [(connect-four? b p 'diagUp plyr 0)
        (Winning-Line plyr p (Pos (+ 3 rows) (+ 3 cols)))]
       [(connect-four? b p 'diagDown plyr 0)
        (Winning-Line plyr p (Pos (- rows 3) (+ 3 cols)))]
       [else (return-winning-line b plyr (Pos (+ 1 rows) cols))])]))
           
(check-expect (return-winning-line b4 'black (Pos 0 0))
              (Winning-Line 'black (Pos 0 0) (Pos 3 0)))
(check-expect (return-winning-line b5 'black (Pos 0 0))
              (Winning-Line 'black (Pos 3 0) (Pos 0 3)))
(check-expect (return-winning-line b7 'white (Pos 0 0))
              (Winning-Line 'white (Pos 0 0) (Pos 3 0)))
(check-expect (return-winning-line b6 'white (Pos 0 0))
              (Winning-Line 'white (Pos 1 2) (Pos 4 5)))
(check-expect (return-winning-line b1 'white (Pos 0 0))
              (Winning-Line 'white (Pos 2 0) (Pos 2 3)))
(check-expect (return-winning-line b7 'black (Pos 0 0)) 'ongoing)

(: check-full? : Board Pos -> Boolean)
;; Checks whether or not the board is full. If it is, that means that the game
;; is not ongoing. It is possible for there to be a tie before the game board
;; is filled, but the directions did not really talk very much about how to
;; define a tie, so for now, this is how a tie is defined in this program.
(define (check-full? b p)
  (match* (p (column-search b p))
    [((Pos rows 6) (Stack h ps)) (= h 5)]
    [((Pos rows cols) (Stack h ps))
     (and (= h 5) (check-full? b (Pos rows (+ 1 cols))))]))

(check-expect (check-full? b7 (Pos 5 0)) #t)
(check-expect (check-full? b4 (Pos 5 0)) #f)
(check-expect (check-full? b5 (Pos 5 0)) #f)

(: outcome : Game -> (U Winning-Line 'tie 'ongoing))
;; If one player has a line of four, then outcome returns a Winning-Line which
;; indicates which player won, and where their line of four is. If the player
;; has more than one line, this function returns any of them. If it is a tie,
;; this function returns 'tie. If the game is ongoing, this function returns
;; 'ongoing.
(define (outcome g)
  (match g
    [(Game b p)
     (local {                        
             (: plyr : Player)
             (define plyr (match p
                            ['white 'black]
                            ['black 'white]))
             (: wl : (U 'tie 'ongoing Winning-Line))
             (define wl (return-winning-line b plyr (Pos 0 0)))}
       (match wl
         ['ongoing (if (check-full? b (Pos 5 0)) 'tie 'ongoing)]
         [_ wl]))]))

(check-expect (outcome (Game b1 'black))
              (Winning-Line 'white (Pos 2 0) (Pos 2 3)))
(check-expect (outcome (Game b2 'white))
              (Winning-Line 'black (Pos 0 0) (Pos 0 3)))
(check-expect (outcome (Game b3 'black))
              (Winning-Line 'white (Pos 2 0) (Pos 2 3)))
(check-expect (outcome (Game b4 'black))
              (Winning-Line 'white (Pos 0 2) (Pos 0 5)))
(check-expect (outcome (Game b5 'white))
              (Winning-Line 'black (Pos 3 0) (Pos 0 3)))
(check-expect (outcome (Game b6 'white)) 'ongoing)
(check-expect (outcome (Game b7 'white)) 'tie)

(: game-over : World -> Boolean)
(define (game-over w)
  (match w
    [(World g d c1 c2 e)
     (match (outcome g)
       ['tie #t]
       ['ongoing #f]
       [_ #t])]))
;;
;; Note: this last check-expect would never happen in a game,
;; but it still represents a tie according to the game logic.

;; BOT LOGIC
;;
(: check-first-empty : Game Natural -> Integer)
;; Returns the first avaialable column in a game.
(define (check-first-empty g i)
  (match g
    [(Game (Board (cons (Stack h ps) ss)) p)
     (if (< h 5) i (check-first-empty (Game (Board ss) p) (+ 1 i)))]))

(check-expect (check-first-empty (Game b1 'white) 0) 0)
(check-expect (check-first-empty (Game b2 'white) 0) 1)

(: first-available : Strategy)
;; A strategy for a bot which always chooses the first available column,
;; going left to right.
(define (first-available game)
  (check-first-empty game 0))

(check-expect (first-available (Game b1 'white)) 0)
(check-expect (first-available (Game b2 'white)) 1)

(: apply-move-mod : Game Integer -> Game)
;; Similar to apply-move but does not take a "player" input,
;; as the information is innate to the Player.
(define (apply-move-mod g c)
  (match g
    [(Game b p)
     (match p
       ['white (Game
                (Board ((inst replace-at Stack) c (change-stack g p c)
                                                (Board-stacks b))) 'black)]
       ['black (Game
                (Board ((inst replace-at Stack) c (change-stack g p c)
                                                (Board-stacks b))) 'white)])]))

(check-expect (apply-move-mod (Game b12 'black) 0)
              (Game (Board (list (Stack 1 '(black white))
                                 (Stack 1 '(black black))
                                 (Stack 0 '(black))
                                 (Stack 0 '(black))
                                 (Stack 0 '(white))
                                 (Stack 0 '(white))
                                 (Stack -1 '()))) 'white))
(check-expect (apply-move-mod (Game b12 'black) 5)
              (Game (Board (list (Stack 0 '(white))
                                 (Stack 1 '(black black))
                                 (Stack 0 '(black))
                                 (Stack 0 '(black))
                                 (Stack 0 '(white))
                                 (Stack 1 '(black white))
                                 (Stack -1 '()))) 'white))

(: place-piece : Game Integer Integer Player -> Game)
;; Helper for hypothetical-board. Take game, row, col,
;; and player and returns an updated game state
;; where a column is filled with opposite colored pieces
;; so that the desired player can play in the desired position
(define (place-piece g rows cols p)
  (match g [(Game b p2)
            (match p
              ['black
               (cond
                 [(and (symbol? (board-ref b (Pos (- rows 1) cols))) (= 0 rows))
                  (apply-move-mod (Game (Game-board g) 'black) cols)]
                 [(symbol? (board-ref b (Pos (- rows 1) cols)))
                  (place-piece (apply-move-mod
                                (Game (Game-board g) 'white) cols) rows cols p)]
                 [else (apply-move-mod (Game (Game-board g) 'black) cols)])]
              ['white
               (cond
                 [(and (symbol? (board-ref b (Pos (- rows 1) cols))) (= 0 rows))
                  (apply-move-mod (Game (Game-board g) 'white) cols)]
                 [(symbol? (board-ref b (Pos (- rows 1) cols)))
                  (place-piece (apply-move-mod (Game (Game-board g) 'black)
                                               cols) rows cols p)]
                 [else (apply-move-mod (Game (Game-board g) 'white) cols)])])]))

(check-expect (place-piece (Game b1 'black) 4 3 'black)
              (Game
               (Board (list (Stack 4 '(black white white black white))
                            (Stack 3 '(black white black black))
                            (Stack 4 '(black white white black black))
                            (Stack 4 '(black white white black black))
                            (Stack -1 '()) (Stack 1 '(black white))
                            (Stack 0 '(white))))
               'white))

(: hypothetical-board : Game Integer Integer Player -> Game)
;; Constructs a game from a given game with the
;; addition of a move at the given position
;; for a given player.
(define (hypothetical-board g rows cols plyr) 
  (match g
    ((Game b p)
     (match* ((board-ref b (Pos rows cols)) plyr)
       [('none 'black) (Game (Game-board (place-piece g rows cols 'black))
                             'white)]
       [(_ 'black) (Game b 'white)]
       [('none 'white) (Game (Game-board (place-piece g rows cols 'white))
                             'black)]
       [(_ 'white) (Game b 'black)]))))

(check-expect (hypothetical-board (Game b1 'black) 1 4 'black)
              (Game (Board (list (Stack 4 '(black white white black white))
                                 (Stack 3 '(black white black black))
                                 (Stack 4 '(black white white black black))
                                 (Stack 2 '(white black black))
                                 (Stack 1 '(black white))
                                 (Stack 1 '(black white))
                                 (Stack 0 '(white))))'white))
(check-expect (hypothetical-board (Game b1 'black) 2 3 'black)
              (Game (Board (list (Stack 4 '(black white white black white))
                                 (Stack 3 '(black white black black))
                                 (Stack 4 '(black white white black black))
                                 (Stack 2 '(white black black))
                                 (Stack -1 '()) (Stack 1 '(black white))
                                 (Stack 0 '(white)))) 'white))
(check-expect (hypothetical-board (Game b1 'black) 3 3 'black)
              (Game (Board (list (Stack 4 '(black white white black white))
                                 (Stack 3 '(black white black black))
                                 (Stack 4 '(black white white black black))
                                 (Stack 3 '(black white black black))
                                 (Stack -1 '()) (Stack 1 '(black white))
                                 (Stack 0 '(white)))) 'white))
(check-expect (hypothetical-board (Game b1 'black) 4 3 'black)
              (Game (Board (list (Stack 4 '(black white white black white))
                                 (Stack 3 '(black white black black))
                                 (Stack 4 '(black white white black black))
                                 (Stack 4 '(black white white black black))
                                 (Stack -1 '()) (Stack 1 '(black white))
                                 (Stack 0 '(white)))) 'white))
(check-expect (hypothetical-board (Game b1 'black) 0 4 'black)
              (Game (Board (list (Stack 4 '(black white white black white))
                                 (Stack 3 '(black white black black))
                                 (Stack 4 '(black white white black black))
                                 (Stack 2 '(white black black))
                                 (Stack 0 '(black))
                                 (Stack 1 '(black white)) (Stack 0 '(white))))
                    'white))

(: count : Game Integer Integer Player -> Integer)
;; Counts the number of winning-lines in a hypothetical board
(define (count g rows cols p)
  (match (outcome (hypothetical-board g rows cols p))
    [(Winning-Line _ _ _)
     (+ 1 (cond
            [(< rows 5) (count g (+ rows 1) cols p)]
            [(< cols 6) (count g 0 (+ cols 1) p)]
            [else 0]))]
    [_ (cond
         [(< rows 5) (count g (+ rows 1) cols p)]
         [(< cols 6) (count g 0 (+ cols 1) p)]
         [else 0])]))

(check-expect (count (Game b9 'black) 0 0 'black) 3)
(check-expect (count (Game b9 'black) 0 0 'white) 2)
(check-expect (count (Game b9 'white) 4 1 'white) 2)
(check-expect (count (Game b12 'white) 3 0 'black) 0)

(: count-winning-positions : Heuristic)
;; Calculates number of empty spaces that would lead to black victory
;; minus the number of spaces that would lead to white victory.
(define (count-winning-positions g)
  (match* ((outcome (Game (Game-board g) 'black))
           (outcome (Game (Game-board g) 'white)))
    [((Winning-Line _ _ _) _) -1000]
    [(_ (Winning-Line _ _ _)) 1000]
    [(_ _)(- (count g 0 0 'black) (count g 0 0 'white))]))

(check-expect (count-winning-positions (Game b9 'black)) 1)
(check-expect (count-winning-positions (Game b13 'white)) 0)
(check-expect (count-winning-positions (Game b10 'white)) 1000)
(check-expect (count-winning-positions (Game b11 'black)) -1000)
(check-expect (count-winning-positions (apply-move-mod (Game b15 'white) 6)) -1)

(: did-I-win? : Heuristic)
;; Returns 1 if black has win or -1 if white has won. Returns 0 otherwise.
(define (did-I-win? g)
  (match* ((outcome (Game (Game-board g) 'black))
           (outcome (Game (Game-board g) 'white)))
    [((Winning-Line _ _ _) _) -1]
    [(_ (Winning-Line _ _ _)) 1]
    [(_ _) 0]))

(check-expect (did-I-win? (Game b9 'black)) 0)
(check-expect (did-I-win? (apply-move-mod (Game b9 'black) 0)) 1)

(: tree-build : Game Integer Integer -> (Listof Node))
;; Given a game state, a starting index, and a ply,
;; returns a tree of possible game states.
(define (tree-build g i ply)
  (cond
    [(>= i 7)  '()]
    [(= ply 0) (cons (Node g '()) '())]
    [(symbol? (board-ref (Game-board g) (Pos 5 i)))
     (local {(: apmov : Game)
             (define apmov (apply-move-mod g i))}
       ((inst cons Node) (Node apmov
                               (if (> ply 1)
                                   (tree-build apmov 0 (- ply 1))
                                   '()))
                         (tree-build g (+ i 1) ply)))]
    [else ((inst cons Node)
           (Node (match g
                   [(Game b 'black) (Game b 'white)]
                   [(Game b 'white) (Game b 'black)])
                 (if (> ply 1)
                     (tree-build (match g
                                   [(Game b 'black) (Game b 'white)]
                                   [(Game b 'white) (Game b 'black)])
                                 0 (- ply 1))
                     '()))
           (tree-build g (+ i 1) ply))]))

(check-expect (tree-build (Game b9 'black) 0 1) (Node-children t1))
(check-expect (tree-build new-game 0 1) new-game-tree-children)

(: largest-value : (Listof Node) Node Heuristic -> Node)
;; Given a list of nodes, a minimum node
;; (defined previously under variables section), and a heuristic,
;; returns the largest node based on its heuristic value.
(define (largest-value ns max h)
  (match ns
    [(cons n nr)
     (if (>= (h (Node-value n)) (h (Node-value max))) (largest-value nr n h)
         (largest-value nr max h))]
    [_ max]))

(check-expect (largest-value (tree-build new-game 0 1) smallest-node
                             count-winning-positions)
              (Node (Game (Board (list (Stack -1 '()) (Stack -1 '())
                                       (Stack -1 '()) (Stack -1 '())
                                       (Stack -1 '()) (Stack -1 '())
                                       (Stack 0 '(black)))) 'white) '()))
(check-expect
 (largest-value (tree-build (Game b9 'black) 0 1) smallest-node
                count-winning-positions)
 (Node (Game (Board (list (Stack -1 '()) (Stack 3 '(black black black black))
                          (Stack 0 '(black)) (Stack 2 '(black white black))
                          (Stack 2 '(black white white))
                          (Stack 1 '(white black)) (Stack -1 '())))
             'white) '()))
(check-expect
 (largest-value (tree-build (Game b9 'black) 0 2) smallest-node
                count-winning-positions)
 (Node
  (Game (Board (list (Stack -1 '()) (Stack 3 '(black black black black))
                     (Stack 0 '(black)) (Stack 2 '(black white black))
                     (Stack 2 '(black white white)) (Stack 1 '(white black))
                     (Stack -1 '()))) 'white)
  (list
   (Node (Game (Board (list (Stack 0 '(white))
                            (Stack 3 '(black black black black))
                            (Stack 0 '(black)) (Stack 2 '(black white black))
                            (Stack 2 '(black white white))
                            (Stack 1 '(white black)) (Stack -1 '())))
               'black) '())
   (Node (Game (Board (list (Stack -1 '())
                            (Stack 4 '(white black black black black))
                            (Stack 0 '(black)) (Stack 2 '(black white black))
                            (Stack 2 '(black white white))
                            (Stack 1 '(white black))
                            (Stack -1 '()))) 'black) '())
   (Node (Game (Board (list (Stack -1 '()) (Stack 3 '(black black black black))
                            (Stack 1 '(white black))
                            (Stack 2 '(black white black))
                            (Stack 2 '(black white white))
                            (Stack 1 '(white black))
                            (Stack -1 '()))) 'black) '())
   (Node (Game (Board (list (Stack -1 '()) (Stack 3 '(black black black black))
                            (Stack 0 '(black))
                            (Stack 3 '(white black white black))
                            (Stack 2 '(black white white))
                            (Stack 1 '(white black))
                            (Stack -1 '()))) 'black) '())
   (Node (Game (Board (list (Stack -1 '())
                            (Stack 3 '(black black black black))
                            (Stack 0 '(black)) (Stack 2 '(black white black))
                            (Stack 3 '(white black white white))
                            (Stack 1 '(white black)) (Stack -1 '()))) 'black)
         '())
   (Node (Game (Board (list (Stack -1 '()) (Stack 3 '(black black black black))
                            (Stack 0 '(black)) (Stack 2 '(black white black))
                            (Stack 2 '(black white white))
                            (Stack 2 '(white white black))
                            (Stack -1 '()))) 'black) '())
   (Node (Game (Board (list (Stack -1 '()) (Stack 3 '(black black black black))
                            (Stack 0 '(black)) (Stack 2 '(black white black))
                            (Stack 2 '(black white white))
                            (Stack 1 '(white black)) (Stack 0 '(white))))
               'black) '()))))

(: smallest-value : (Listof Node) Node Heuristic -> Node)
;; Functions similarly to largest-value, but finds smallest value instead.
(define (smallest-value ns min h)
  (match ns
    [(cons n nr)
     (if (<= (h (Node-value n)) (h (Node-value min))) (smallest-value nr n h)
         (smallest-value nr min h))]
    [_ min]))

(check-expect
 (smallest-value (tree-build new-game 0 1) largest-node count-winning-positions)
 (Node (Game (Board (list (Stack -1 '()) (Stack -1 '()) (Stack -1 '())
                          (Stack -1 '())
                          (Stack -1 '()) (Stack -1 '())
                          (Stack 0 '(black)))) 'white) '()))
(check-expect
 (smallest-value (tree-build (Game b9 'black) 0 1) largest-node
                 count-winning-positions)
 (Node (Game (Board (list (Stack -1 '()) (Stack 2 '(black black black))
                          (Stack 0 '(black))
                          (Stack 2 '(black white black))
                          (Stack 2 '(black white white))
                          (Stack 1 '(white black)) (Stack 0 '(black))))
             'white) '()))

(: nodes-eval : (Listof Node) Heuristic -> (Listof Node))
;; Given a list of nodes and a heuristic, returns a list of minimax
;; nodes in place of the original nodes. A node that evaluates to a winning
;; position does not explore further branches.
(define (nodes-eval ns0 h)
  (match ns0
    [(cons (Node (Game b 'white) '()) _)
     (match (outcome (Game b 'white))
       [(Winning-Line _ _ _) (match (Node-value (list-ref ns0 0))
                               [(Game _ 'black) (cons smallest-node '())]
                               [(Game _ 'white) (cons largest-node '())])]
       [_ ((inst cons Node) (largest-value ns0 smallest-node h) '())])]                                      
    [(cons (Node (Game b 'black) '()) _)
     (match (outcome (Game b 'black))
       [(Winning-Line _ _ _) (match (Node-value (list-ref ns0 0))
                               [(Game _ 'black) (cons smallest-node '())]
                               [(Game _ 'white) (cons largest-node '())])]
       [_ ((inst cons Node) (smallest-value ns0 largest-node h) '())])]
    [(cons (Node (Game b 'black) ns1) nr0)
     (match (outcome (Game b 'black))
       [(Winning-Line _ _ _) (match (Node-value (list-ref ns0 0))
                               [(Game _ 'black) (cons smallest-node '())]
                               [(Game _ 'white) (cons largest-node '())])]
       [_ ((inst cons Node) (largest-value (nodes-eval ns1 h) smallest-node h)
                            (nodes-eval nr0 h))])]
    [(cons (Node (Game b 'white) ns1) nr0)
     (match (outcome (Game b 'white))
       [(Winning-Line _ _ _) (match (Node-value (list-ref ns0 0))
                               [(Game _ 'black) (cons smallest-node '())]
                               [(Game _ 'white) (cons largest-node '())])]
       [_ ((inst cons Node) (smallest-value (nodes-eval ns1 h) largest-node h)
                            (nodes-eval nr0 h))])]
    [_ '()]))

(check-expect (nodes-eval (tree-build (Game b12 'white) 0 3)
                          count-winning-positions)
              (list
               (Node (Game (Board (list (Stack 1 '(white white))
                                        (Stack 3 '(white black black black))
                                        (Stack 0 '(black)) (Stack 0 '(black))
                                        (Stack 0 '(white)) (Stack 0 '(white))
                                        (Stack -1 '()))) 'black) '())
               (Node (Game (Board (list (Stack 0 '(white))
                                        (Stack 2 '(white black black))
                                        (Stack 0 '(black)) (Stack 0 '(black))
                                        (Stack 0 '(white)) (Stack 0 '(white))
                                        (Stack 1 '(white black)))) 'black) '())
               (Node (Game (Board (list (Stack 0 '(white))
                                        (Stack 1 '(black black))
                                        (Stack 1 '(white black))
                                        (Stack 0 '(black)) (Stack 0 '(white))
                                        (Stack 0 '(white))
                                        (Stack 1 '(white black)))) 'black) '())
               (Node (Game (Board (list (Stack 0 '(white))
                                        (Stack 1 '(black black))
                                        (Stack 0 '(black))
                                        (Stack 1 '(white black))
                                        (Stack 0 '(white)) (Stack 0 '(white))
                                        (Stack 1 '(white black)))) 'black) '())
               (Node (Game (Board (list (Stack 0 '(white))
                                        (Stack 1 '(black black))
                                        (Stack 0 '(black)) (Stack 0 '(black))
                                        (Stack 2 '(black white white))
                                        (Stack 0 '(white)) (Stack 0 '(white))))
                           'black) '())
               (Node (Game (Board (list (Stack 0 '(white))
                                        (Stack 1 '(black black))
                                        (Stack 0 '(black)) (Stack 0 '(black))
                                        (Stack 0 '(white))
                                        (Stack 2 '(black white white))
                                        (Stack 0 '(white)))) 'black) '())
               (Node (Game (Board (list (Stack 0 '(white))
                                        (Stack 1 '(black black))
                                        (Stack 0 '(black)) (Stack 0 '(black))
                                        (Stack 0 '(white)) (Stack 0 '(white))
                                        (Stack 2 '(white black white)))) 'black)
                     '())))
(check-expect
 (nodes-eval (tree-build new-game 0 1) count-winning-positions)
 (list (Node (Game (Board (list (Stack -1 '()) (Stack -1 '()) (Stack -1 '())
                                (Stack -1 '()) (Stack -1 '()) (Stack -1 '())
                                (Stack 0 '(black)))) 'white) '())))

(: minimax-eval : Heuristic Integer Game -> Integer)
;; Given a heuristic, ply, and game, returns the minimax evaluation of the game.
(define (minimax-eval h ply g)
  (match (outcome g)
    [(Winning-Line _ _ _) (match g
                            [(Game _ 'black) -1000]
                            [(Game _ 'white) 1000])]
    [_ (match g
         [(Game _ 'black) (h (Node-value (largest-value
                                          (nodes-eval (tree-build g 0 ply) h)
                                          smallest-node h)))]
         [(Game _ 'white) (h (Node-value (smallest-value
                                          (nodes-eval (tree-build g 0 ply) h)
                                          largest-node h)))])]))

(check-expect (minimax-eval count-winning-positions 3 (Game b9 'black)) 1000)
(check-expect (minimax-eval count-winning-positions 2 (Game b14 'white)) 0)
(check-expect (minimax-eval count-winning-positions 3 (Game b14 'white)) 0)
(check-expect (minimax-eval count-winning-positions 0 (Game b9 'white)) 1)
(check-expect (minimax-eval did-I-win? 3 (Game b14 'white)) 0)
(check-expect (minimax-eval did-I-win? 3 (Game b9 'black)) 1)
(check-expect (minimax-eval count-winning-positions 1 (Game b14 'white)) 0)

(: choose : Heuristic Game Integer Integer Integer Integer -> Integer)
;; Given a heuristic, game, and ply, returns the column number
;; which leads to the best minimax evaluation. Tie goes to the latter
;; column number (e.g. 6 over 3).
(define (choose h g ply best i saver)
  (if (< i 7)
      (local {
              (: mme : Integer)
              (define mme
                (if (symbol? (board-ref (Game-board g) (Pos 5 i)))
                    (minimax-eval h (- ply 1) (apply-move-mod g i))
                    (minimax-eval h (- ply 1) g)))}
        (match g
          [(Game _ 'black) (cond
                             [(= mme 1000) i]
                             [(and (symbol? (board-ref (Game-board g)
                                                       (Pos 5 i)))
                                   (<= best mme))
                              (choose h g ply mme (+ i 1) i)]
                             [else (choose h g ply best (+ i 1) saver)])]
          [(Game _ 'white) (cond
                             [(= mme -1000) i]
                             [(and (symbol? (board-ref (Game-board g)
                                                       (Pos 5 i)))
                                   (>= best mme))
                              (choose h g ply mme (+ i 1) i)]
                             [else (choose h g ply best (+ i 1) saver)])]))
      saver))

(check-expect (choose count-winning-positions (Game b12 'black) 2 -1000 0 0) 6)
(check-expect (choose count-winning-positions (Game b9 'white) 2 1000 0 0) 2)

(: make-minimax-strategy : Heuristic Integer -> Strategy)
;; Given a heuristic and ply, returns a strategy based on
;; minimax evaluation of the game.
(define (make-minimax-strategy h ply)
  (lambda ([g : Game]) 
    (if (<= ply 0) (error "ply must be greater than 0")
        (match* ((outcome (Game (Game-board g) 'black))
             (outcome (Game (Game-board g) 'white)))
      [((Winning-Line _ _ _) _) (error "Game already over")]
      [(_ (Winning-Line _ _ _)) (error "Game already over")]
      [(_ _) (match g
               [(Game _ 'black) (choose h g ply -1000 0 (first-available g))]
               [(Game _ 'white) (choose h g ply 1000 0
                                        (first-available g))])]))))

(check-error ((make-minimax-strategy count-winning-positions 3)
              (Game b1 'white)) "Game already over")
(check-expect ((make-minimax-strategy count-winning-positions 3)
               new-game) 6)
(check-expect ((make-minimax-strategy count-winning-positions 3)
               (Game b9 'white)) 2)
(check-expect ((make-minimax-strategy count-winning-positions 1)
               (Game b14 'white)) 6)
(check-expect ((make-minimax-strategy did-I-win? 2)
               (Game b14 'white)) 6)
(check-error ((make-minimax-strategy did-I-win? 0)
               (Game b14 'white)) "ply must be greater than 0")

;; BOARD VISUALIZATIONS
;;
(: grey-space : Stack Integer CircleDetails -> Image)
;; Given a stack, a distance between centers, a struct for stylistic details,
;; Returns an image of the empty circles above each stack.
(define (grey-space s d c)
  (match c
    [(CircleDetails r p1c p2c bg)
     (match s
       [(Stack 5 _) empty-image]
       [(Stack h ss) (above/align "center"
                                  (rectangle 1 (/ (- d (* 2 r)) 2) "solid" bg)
                                  (circle r "solid" "gray")
                                  (rectangle 1 (/ (- d (* 2 r)) 2) "solid" bg)
                                  (grey-space (Stack (+ h 1) ss) d c))])]))

;; (grey-space (Stack 4 (list 'black 'white 'white 'black 'black)) 50
;;             (CircleDetails 20 "blue" "red" "white"))
;; (grey-space (Stack 2 (list 'black 'white)) 50
;;             (CircleDetails 20 "blue" "red" "white"))
;; (grey-space (Stack 0 (list)) 50
;;             (CircleDetails 20 "blue" "red" "white"))
;; (grey-space (Stack 5 (list 'black 'white 'white 'black 'black 'white)) 50
;;             (CircleDetails 20 "blue" "red" "white"))

(: mk-stack : Stack Integer CircleDetails -> Image)
;; Given a Stack, a distance between centers, and a struct for stylistic details
;; returns an image of the stack.
(define (mk-stack s d c)
  (match c
    [(CircleDetails r p1c p2c bg)
     (match s
       [(Stack -1 _) empty-image]
       [(Stack h (cons p pr))
        (if (symbol=? p 'black)
            (above/align "center"
                         (rectangle 1 (/ (- d (* 2 r)) 2) "solid" bg)
                         (circle r "solid" p1c)
                         (rectangle 1 (/ (- d (* 2 r)) 2) "solid" bg)
                         (mk-stack (Stack (- h 1) pr) d c))
            (above/align "center"
                         (rectangle 1 (/ (- d (* 2 r)) 2) "solid" bg)
                         (circle r "solid" p2c)
                         (rectangle 1 (/ (- d (* 2 r)) 2) "solid" bg)
                         (mk-stack (Stack (- h 1) pr) d c)))])]))

;; (mk-stack (Stack 4 (list 'black 'white 'white 'black 'black)) 50
;;          (CircleDetails 20 "blue" "red" "black"))

(: horizontal-spacing : Integer CircleDetails -> Image)
;; Creates rectangle of background color to properly space stack images.
(define (horizontal-spacing d c)
  (rectangle (/ (- d (* 2 (CircleDetails-radius c))) 2) 1 "solid"
             (CircleDetails-bg-color c)))
         
(: board-image-aux : Board Integer CircleDetails -> Image)
;; Combines images of stacks and their spacing to create a board image.
(define (board-image-aux b d c)
  (match c
    [(CircleDetails r p1c p2c bg)
     (match b
       [(Board (cons s sr)) (beside/align "middle" (horizontal-spacing d c)
                                          (above/align "middle"
                                                       (grey-space s d c)
                                                       (mk-stack s d c))
                                          (horizontal-spacing d c)
                                          (board-image-aux (Board sr) d c))]
       [_ empty-image])]))

(: board-image : Board Integer -> Image)
;; Given a board and an integer indicating the distance between the centers of
;; adjacent spaces on the board, returns and image of the board.
(define (board-image b d)
  (overlay/align "left" "bottom"
                 (board-image-aux b d (CircleDetails 20 "blue" "red" "black"))
                 (rectangle (+ (* 7 d) 0) (+ (* 6 d) 0)
                            "solid" "black")))

;; (board-image b9 50)
;; (board-image b5 50)
;; (board-image b6 50)

(: draw-line : Image Pos Pos Integer -> Image)
;; Creates a line over an image from a
;; given position to a given position.
(define (draw-line b p1 p2 d)
  (match* (p1 p2)
    (((Pos row1 col1) (Pos row2 col2))
     (add-line b (+ (* (image-width b) (/ col1 7)) (/ d 2))
               (- (* (image-height b) (/ (- 6 row1) 6)) (/ d 2))
               (+ (* (image-width b) (/ col2 7)) (/ d 2))
               (- (* (image-height b) (/ (- 6 row2) 6)) (/ d 2)) "white"))))

;; (draw-line (board-image b4 50) (Pos 3 3) (Pos 3 6) 50)
;; (draw-line (board-image b4 50) (Pos 0 0) (Pos 3 3) 50)

(: controller-to-name : Controller -> String)
;; Takes in a Controller and outputs its name as a string.
(define (controller-to-name c)
  (match c
    [(Human n)  (if (symbol? n) (symbol->string n) n)]
    [(Bot n _) (if (symbol? n) (symbol->string n) n)]))

(: game-image : World -> Image)
;; Given a game and the spacing between the centers of adjacent pieces, produces
;; an image of the game, including the board. If someone has won the game, shows
;; this visually by drawing a line segment on top of their line of four, and
;; also says in text who has won. If nobody has won yet, give an indication of
;; whose turn it is. If it's a tie, indicates so.
(define (game-image w)
  (match w
    [(World g d c1 c2 e)
     (local {
             (: out : (U Winning-Line 'ongoing 'tie))
             (define out (outcome g))}
       (if e (overlay/align "middle" "middle" (text "Illegal Move!" 40 "red")
                            (game-image (World g d c1 c2 #f)))
           (match* (g out)
             [((Game b 'black) 'ongoing)
              (above/align "center" (board-image b d)
                           (beside/align "center"
                                         (text (controller-to-name c1)
                                               40 "black")
                                         (text " to move" 40 "black")))]
             [((Game b 'white) 'ongoing)
              (above/align "center" (board-image b d)
                           (beside/align "center"
                                         (text (controller-to-name c2)
                                               40 "black")
                                         (text " to move" 40 "black")))]
             [((Game b Player) 'tie) (above/align "center" (board-image b d)
                                                  (text "Tie!" 40 "black"))]
             [((Game b 'black) (Winning-Line plyr p1 p2))
              (above/align "center" (draw-line (board-image b d) p1 p2 d)
                           (beside/align "center"
                                         (text (controller-to-name c2)
                                               40 "black")
                                         (text " wins!" 40 "black")))]
             [((Game b 'white) (Winning-Line plyr p1 p2))
              (above/align "center" (draw-line (board-image b d) p1 p2 d)
                           (beside/align "center"
                                         (text (controller-to-name c1)
                                               40 "black")
                                         (text " wins!" 40 "black")))])))]))

;; (game-image (World (Game b8 'black) 50 (Human 'Tim) (Human 'Milo) #f))

;; INTERACTIVE INTERFACE
;;
(: column? : Integer Integer Integer Integer -> Boolean)
;; Checks if click is within a column.
(define (column? x y i d)
  (<= (- (+ (* i d) (/ d 2)) 20) x (+ (+ (* i d) (/ d 2)) 20)))

(: in-column : World Integer Integer Natural -> Integer)
;; returns the index value of the clicked column.
;; If no column is clicked, it returns -1.
;;
(define (in-column w x y i)
  (match w
    [(World g d c1 c2 e)
     (cond
       [(column? x y i d) i]
       [(< i 7) (in-column w x y (+ i 1))]
       [else -1])]))

(: change : World Integer Integer -> Game)
;; determines whether mouse-click is inside or outside a circle
;;
(define (change w x y)
  (match w
    [(World g d c1 c2 e)
     (match g 
       [(Game b plyr) (if (= (in-column w x y 0) -1) g
                          (apply-move g plyr (in-column w x y 0)))])]))

(: react-to-mouse : World Integer Integer Mouse-Event -> World)
;; If the user clicks on a column, add a player's token to that column.
;; If the user clicks outside any circle, don't change anything.
;; If the user attempts to add a token in a full stack, tell them it's illegal.
(define (react-to-mouse w x y e)
  (match w
    [(World g d c1 c2 errboard)
     (match g
       [(Game _ 'black)
        (match c1
          [(Human _) 
           (match e
             ["button-down"
              (if (valid-move? g (Game-next g) (in-column w x y 0))
                  (World (change w x y) d c1 c2 #f) (World g d c1 c2 #t))]
             [_ w])]
          [(Bot _ s) (World (apply-move g (Game-next g) (s g)) d c1 c2 #f)])]
       [(Game _ 'white)
        (match c2
          [(Human _) 
           (match e
             ["button-down"
              (if (valid-move? g (Game-next g) (in-column w x y 0))
                  (World (change w x y) d c1 c2 #f) (World g d c1 c2 #t))]
             [_ w])]
          [(Bot _ s) (World (apply-move g (Game-next g) (s g)) d c1 c2 #f)
                     ])])]))

(: play : Controller Controller Integer -> World)
;; creates a world and updates it according to inputs. Stops when game is over.
(define (play c1 c2 d)
  (big-bang (World new-game d c1 c2 #f) : World
    [name "connect four"]
    [to-draw game-image]
    [on-mouse react-to-mouse]
    [stop-when game-over game-image]))

(play (Human "Milo")
      (Bot "EvilBotMilo" (make-minimax-strategy count-winning-positions 3)) 50)

(test)
