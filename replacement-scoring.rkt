#lang racket

(provide import-yearly-scoring
         lookup-scoring)

(require csv-reading)

(require racket/runtime-path)

(module+ test
  (require rackunit))


(define POSITIONS '("QB" "RB" "WR" "TE"))

(define REPLACEMENT-LEVEL (hash "QB" 25
                                "RB" 40
                                "WR" 60
                                "TE" 13))

(define YEARS '(2015 2016 2017 2018 2019 2020 2021 2022))

(define-runtime-path DATA/ "Data")

;; CSVTable -> (Listof (List PlayerName FPPG))
(define (analyze-scoring table)
  (match-define (cons headers rows) table)
  (define name-index (index-of headers "Player"))
  (define ppg-index (index-of headers "FPTS/G"))
  (for/list ([row (in-list rows)]
             ;; for some reason there are some dumb almost-empty rows
             #:when (> (length row) 2))
    (define name-cell (list-ref row name-index))
    (define ppg-cell (list-ref row ppg-index))
    (define name (fantasy-pros-name name-cell))
    (list name (string->number ppg-cell))))

(define PLAYER-RX #px"^(.*) \\(.*\\)$")
(define (fantasy-pros-name cell)
  (second (regexp-match PLAYER-RX cell)))

(module+ test
  (check-equal? (fantasy-pros-name "Patrick Mahomes II (KC)")
                "Patrick Mahomes II"))

;; Year -> (Hash Position (List ReplacementPPG (Hashof PlayerName FPPG)))
(define (import-yearly-scoring year)
  (for/hash ([posn (in-list POSITIONS)])
    (define fantasy-pros-data (build-path DATA/ (~a year) (format "~a_Scoring.csv" posn)))
    (define rows (with-input-from-file fantasy-pros-data
                   (lambda () (csv->list (current-input-port)))))
    (define players+scoring (analyze-scoring rows))
    (define ranked (sort players+scoring > #:key second))
    (define replacement-ppg (second (list-ref ranked (sub1 (hash-ref REPLACEMENT-LEVEL posn)))))
    (printf "~a ~a replacement PPG: ~a\n" year posn replacement-ppg)
    (define player# (for/hash ([item (in-list players+scoring)])
                      (values (first item) (second item))))
    (values posn (list replacement-ppg player#))))

(define NAME# (hash "Patrick Mahomes" "Patrick Mahomes II"
                    "Mecole Hardman" "Mecole Hardman Jr."
                    "Robbie Anderson" "Robbie Chosen"
                    "Robby Anderson" "Robbie Chosen"
                    "DJ Chark" "DJ Chark Jr."
                    "Gabriel Davis" "Gabe Davis"
                    "Duke Johnson" "Duke Johnson Jr."
                    "Will Fuller V" "William Fuller V"
                    "Mitchell Trubisky" "Mitch Trubisky"
                    "Melvin Gordon" "Melvin Gordon III"
                    "Ronald Jones" "Ronald Jones II"
                    "Allen Robinson" "Allen Robinson II"
                    "Paul Richardson" "Paul Richardson Jr."
                    "Terrelle Pryor" "Terrelle Pryor Sr."
                    "Mark Ingram" "Mark Ingram II"
                    "Rob Kelley" "Robert Kelley"
                    "Willie Snead" "Willie Snead IV"
                    "Mohamed Sanu" "Mohamed Sanu Sr."
                    "Todd Gurley" "Todd Gurley II"
                    "Marvin Jones" "Marvin Jones Jr."
                    "Phillip Dorsett" "Phillip Dorsett II"
                    "Steve Smith Sr." "Steve Smith"
                    "Daniel Herron" "Dan Herron"
                    ))

(define (lookup-scoring player# year name)
  (define name* (hash-ref NAME# name name))
  (match* (year name*)
    [(2021 "Taysom Hill")
     0]
    [(2021 "Gus Edwards")
     0]
    [(2021 "Michael Thomas")
     0]
    [(2020 "Ryquell Armstead")
     0]
    [(2019 "Dwayne Haskins Jr.")
     (hash-ref player# "Dwayne Haskins")]
    [(2018 "Le'Veon Bell")
     0]
    [(2018 "Jerick McKinnon")
     0]
    [(2018 "Derrius Guice")
     0]
    [(2018 "Mohamed Sanu Sr.")
     0]
    [(2018 "Eric Decker")
     0]
    [(2018 "Marqise Lee")
     0]
    [(2018 "Dez Bryant")
     0]
    [(2017 "Andrew Luck")
     0]
    [(2017 "Spencer Ware")
     0]
    [(2017 "Julian Edelman")
     0]
    [(2017 "Cameron Meredith")
     0]
    [(2017 "Anquan Boldin")
     0]
    [(2016 "Teddy Bridgewater")
     0]
    [(2016 "Zach Mettenberger")
     0]
    [(2016 "Josh Gordon")
     0]
    [(2015 "Robert Griffin")
     0]
    [(2015 "Montee Ball")
     0]
    [(2015 "Jordy Nelson")
     0]
    [(2015 "Kelvin Benjamin")
     0]
    [(2015 "Victor Cruz")
     0]
    [(2015 "Breshad Perriman")
     0]
    [(2015 "Kevin White")
     0]
    [(_ _)
     (hash-ref player# name*)]))
