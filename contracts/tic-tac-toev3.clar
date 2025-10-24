;; Tic Tac Toe Contract with Timeout Mechanism

;; Constants
(define-constant THIS_CONTRACT (as-contract tx-sender))
(define-constant ERR_MIN_BET_AMOUNT u100)
(define-constant ERR_INVALID_MOVE u101)
(define-constant ERR_GAME_NOT_FOUND u102)
(define-constant ERR_GAME_CANNOT_BE_JOINED u103)
(define-constant ERR_NOT_YOUR_TURN u104)
(define-constant ERR_TIMEOUT_NOT_REACHED u105) ;; NEW
(define-constant ERR_NOT_A_PLAYER u106) ;; NEW
(define-constant ERR_GAME_ALREADY_OVER u107) ;; NEW

;; NEW: 24 hours in blocks (assuming ~10 second block time after Nakamoto)
;; 24 hours = 86400 seconds / 10 seconds per block = 8640 blocks
;; For testing purposes, we can set this lower, e.g., 10 blocks
(define-constant TIMEOUT_BLOCKS u10)

;; The Game ID to use for the next game
(define-data-var latest-game-id uint u0)

(define-map games 
    uint ;; Key (Game ID)
    { ;; Value (Game Tuple)
        player-one: principal,
        player-two: (optional principal),
        is-player-one-turn: bool,
        bet-amount: uint,
        board: (list 9 uint),
        winner: (optional principal),
        last-move-block: uint  ;; NEW: Block height of last move
    }
)

;; Private Functions

(define-private (validate-move (board (list 9 uint)) (move-index uint) (move uint))
    (let (
        ;; Validate that the move is being played within range of the board
        (index-in-range (and (>= move-index u0) (< move-index u9)))

        ;; Validate that the move is either an X or an O
        (x-or-o (or (is-eq move u1) (is-eq move u2)))

        ;; Validate that the cell the move is being played on is currently empty
        (empty-spot (is-eq (unwrap! (element-at? board move-index) false) u0))
    )

    ;; All three conditions must be true for the move to be valid
    (and (is-eq index-in-range true) (is-eq x-or-o true) empty-spot)
))

;; Given a board and three cells to look at on the board
;; Return true if all three are not empty and are the same value (all X or all O)
;; Return false if any of the three is empty or a different value
(define-private (is-line (board (list 9 uint)) (a uint) (b uint) (c uint)) 
    (let (
        ;; Value of cell at index a
        (a-val (unwrap! (element-at? board a) false))
        ;; Value of cell at index b
        (b-val (unwrap! (element-at? board b) false))
        ;; Value of cell at index c
        (c-val (unwrap! (element-at? board c) false))
    )

    ;; a-val must equal b-val and must also equal c-val while not being empty (non-zero)
    (and (is-eq a-val b-val) (is-eq a-val c-val) (not (is-eq a-val u0)))
))

;; Given a board, return true if any possible three-in-a-row line has been completed
(define-private (has-won (board (list 9 uint))) 
    (or
        (is-line board u0 u1 u2) ;; Row 1
        (is-line board u3 u4 u5) ;; Row 2
        (is-line board u6 u7 u8) ;; Row 3
        (is-line board u0 u3 u6) ;; Column 1
        (is-line board u1 u4 u7) ;; Column 2
        (is-line board u2 u5 u8) ;; Column 3
        (is-line board u0 u4 u8) ;; Left to Right Diagonal
        (is-line board u2 u4 u6) ;; Right to Left Diagonal
    )
)

;; Public Functions

(define-public (create-game (bet-amount uint) (move-index uint) (move uint))
    (let (
        ;; Get the Game ID to use for creation of this new game
        (game-id (var-get latest-game-id))
        ;; The initial starting board for the game with all cells empty
        (starting-board (list u0 u0 u0 u0 u0 u0 u0 u0 u0))
        ;; Updated board with the starting move played by the game creator (X)
        (game-board (unwrap! (replace-at? starting-board move-index move) (err ERR_INVALID_MOVE)))
        ;; Create the game data tuple (player one address, bet amount, game board, and mark next turn to be player two's turn)
        (game-data {
            player-one: contract-caller,
            player-two: none,
            is-player-one-turn: false,
            bet-amount: bet-amount,
            board: game-board,
            winner: none,
            last-move-block: stacks-block-height  ;; NEW: Record creation block
        })
    )

    ;; Ensure that user has put up a bet amount greater than the minimum
    (asserts! (> bet-amount u0) (err ERR_MIN_BET_AMOUNT))
    ;; Ensure that the move being played is an `X`, not an `O`
    (asserts! (is-eq move u1) (err ERR_INVALID_MOVE))
    ;; Ensure that the move meets validity requirements
    (asserts! (validate-move starting-board move-index move) (err ERR_INVALID_MOVE))

    ;; Transfer the bet amount STX from user to this contract
    (try! (stx-transfer? bet-amount contract-caller THIS_CONTRACT))
    ;; Update the games map with the new game data
    (map-set games game-id game-data)
    ;; Increment the Game ID counter
    (var-set latest-game-id (+ game-id u1))

    ;; Log the creation of the new game
    (print { action: "create-game", data: game-data})
    ;; Return the Game ID of the new game
    (ok game-id)
))

(define-public (join-game (game-id uint) (move-index uint) (move uint))
    (let (
        ;; Load the game data for the game being joined, throw an error if Game ID is invalid
        (original-game-data (unwrap! (map-get? games game-id) (err ERR_GAME_NOT_FOUND)))
        ;; Get the original board from the game data
        (original-board (get board original-game-data))

        ;; Update the game board by placing the player's move at the specified index
        (game-board (unwrap! (replace-at? original-board move-index move) (err ERR_INVALID_MOVE)))
        ;; Update the copy of the game data with the updated board and marking the next turn to be player two's turn
        (game-data (merge original-game-data {
            board: game-board,
            player-two: (some contract-caller),
            is-player-one-turn: true,
            last-move-block: stacks-block-height  ;; NEW: Update timestamp
        }))
    )

    ;; Ensure that the game being joined is able to be joined
    ;; i.e. player-two is currently empty
    (asserts! (is-none (get player-two original-game-data)) (err ERR_GAME_CANNOT_BE_JOINED)) 
    ;; Ensure that the move being played is an `O`, not an `X`
    (asserts! (is-eq move u2) (err ERR_INVALID_MOVE))
    ;; Ensure that the move meets validity requirements
    (asserts! (validate-move original-board move-index move) (err ERR_INVALID_MOVE))

    ;; Transfer the bet amount STX from user to this contract
    (try! (stx-transfer? (get bet-amount original-game-data) contract-caller THIS_CONTRACT))
    ;; Update the games map with the new game data
    (map-set games game-id game-data)

    ;; Log the joining of the game
    (print { action: "join-game", data: game-data})
    ;; Return the Game ID of the game
    (ok game-id)
))

(define-public (play (game-id uint) (move-index uint) (move uint))
    (let (
        ;; Load the game data for the game being joined, throw an error if Game ID is invalid
        (original-game-data (unwrap! (map-get? games game-id) (err ERR_GAME_NOT_FOUND)))
        ;; Get the original board from the game data
        (original-board (get board original-game-data))

        ;; Is it player one's turn?
        (is-player-one-turn (get is-player-one-turn original-game-data))
        ;; Get the player whose turn it currently is based on the is-player-one-turn flag
        (player-turn (if is-player-one-turn (get player-one original-game-data) (unwrap! (get player-two original-game-data) (err ERR_GAME_NOT_FOUND))))
        ;; Get the expected move based on whose turn it is (X or O?)
        (expected-move (if is-player-one-turn u1 u2))

        ;; Update the game board by placing the player's move at the specified index
        (game-board (unwrap! (replace-at? original-board move-index move) (err ERR_INVALID_MOVE)))
        ;; Check if the game has been won now with this modified board
        (is-now-winner (has-won game-board))
        ;; Merge the game data with the updated board and marking the next turn to be player two's turn
        ;; Also mark the winner if the game has been won
        (game-data (merge original-game-data {
            board: game-board,
            is-player-one-turn: (not is-player-one-turn),
            winner: (if is-now-winner (some player-turn) none),
            last-move-block: stacks-block-height  ;; NEW: Update timestamp
        }))
    )

    ;; Ensure that the function is being called by the player whose turn it is
    (asserts! (is-eq player-turn contract-caller) (err ERR_NOT_YOUR_TURN))
    ;; Ensure that the move being played is the correct move based on the current turn (X or O)
    (asserts! (is-eq move expected-move) (err ERR_INVALID_MOVE))
    ;; Ensure that the move meets validity requirements
    (asserts! (validate-move original-board move-index move) (err ERR_INVALID_MOVE))

    ;; if the game has been won, transfer the (bet amount * 2 = both players bets) STX to the winner
    (if is-now-winner (try! (as-contract (stx-transfer? (* u2 (get bet-amount game-data)) tx-sender player-turn))) false)

    ;; Update the games map with the new game data
    (map-set games game-id game-data)

    ;; Log the action of a move being made
    (print {action: "play", data: game-data})
    ;; Return the Game ID of the game
    (ok game-id)
))

;; NEW: Cancel game due to timeout
(define-public (cancel-game-timeout (game-id uint))
    (let (
        (game-data (unwrap! (map-get? games game-id) (err ERR_GAME_NOT_FOUND)))
        (player-one (get player-one game-data))
        (player-two (unwrap! (get player-two game-data) (err ERR_GAME_CANNOT_BE_JOINED)))
        (is-player-one-turn (get is-player-one-turn game-data))
        (last-move-block (get last-move-block game-data))
        (blocks-passed (- stacks-block-height last-move-block))
        (bet-amount (get bet-amount game-data))
        
        ;; The player who can cancel is the one waiting for opponent's move
        (can-cancel-player (if is-player-one-turn player-two player-one))
        ;; The inactive player who timed out
        (inactive-player (if is-player-one-turn player-one player-two))
    )

    ;; Ensure game is not already over
    (asserts! (is-none (get winner game-data)) (err ERR_GAME_ALREADY_OVER))
    
    ;; Ensure caller is a player in this game
    (asserts! (or (is-eq contract-caller player-one) (is-eq contract-caller player-two)) (err ERR_NOT_A_PLAYER))
    
    ;; Ensure caller is the player waiting for their opponent
    (asserts! (is-eq contract-caller can-cancel-player) (err ERR_NOT_YOUR_TURN))
    
    ;; Ensure timeout has been reached
    (asserts! (>= blocks-passed TIMEOUT_BLOCKS) (err ERR_TIMEOUT_NOT_REACHED))

    ;; Transfer both bets to the active player (winner by timeout)
    (try! (as-contract (stx-transfer? (* u2 bet-amount) tx-sender can-cancel-player)))

    ;; Mark game as finished with the active player as winner
    (map-set games game-id (merge game-data { winner: (some can-cancel-player) }))

    (print { 
        action: "cancel-game-timeout", 
        winner: can-cancel-player,
        inactive-player: inactive-player,
        game-id: game-id 
    })
    (ok true)
))

;; Read-only Functions

(define-read-only (get-game (game-id uint))
    (map-get? games game-id)
)

(define-read-only (get-latest-game-id)
    (var-get latest-game-id)
)

;; NEW: Check if a game can be cancelled due to timeout
(define-read-only (can-cancel-game (game-id uint))
    (match (map-get? games game-id)
        game-data 
        (let (
            (last-move-block (get last-move-block game-data))
            (blocks-passed (- stacks-block-height last-move-block))
            (has-winner (is-some (get winner game-data)))
            (has-both-players (is-some (get player-two game-data)))
        )
        (ok {
            can-cancel: (and 
                (not has-winner)
                has-both-players
                (>= blocks-passed TIMEOUT_BLOCKS)
            ),
            blocks-until-timeout: (if (>= blocks-passed TIMEOUT_BLOCKS) 
                u0 
                (- TIMEOUT_BLOCKS blocks-passed)
            ),
            blocks-passed: blocks-passed
        }))
        (err ERR_GAME_NOT_FOUND)
    )
)