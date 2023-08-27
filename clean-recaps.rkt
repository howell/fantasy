#lang racket

(require csv-reading
         csv-writing)
(require racket/runtime-path
         )

(module+ test
  (require rackunit))

(define-runtime-path draft-recaps "Data/DraftRecaps.csv")

(struct pick (by who pos amt num) #:transparent)

(define (transpose lst)
  (apply map list lst))

;; expected format of each column:
;;
;; <year>
;; <team-name>
;; NO.
;; PLAYER
;; OFFER AMOUNT
;; <nomination number>
;; <player name> <team>, <position>
;; $<price>

(struct team (nm) #:transparent)
(struct player (nom info price) #:transparent)

(define (dollar? s)
  (string-prefix? s "$"))

(define (search-with-history col)
  (let loop ([prev (list #f #f)]
             [col col])
    (match col
      [(list* "NO." rst)
       (list (team (first prev)) rst)]
      [(list* (? dollar? bid) rst)
       (list (player (second prev) (first prev) bid) rst)]
      [(cons nxt rst)
       (loop (list nxt (first prev)) rst)]
      [_
       #f])))

;; (Listof CSV) -> (Setof Pick)
(define (process-year year-col)
  (define year (string->number (first year-col)))
  (let loop ([current-team #f]
             [picks (set)]
             [col (rest year-col)])
    (match (search-with-history col)
      [(list (team nm) rst)
       #;(printf "team: ~a\n" nm)
       (loop nm picks rst)]
      [(list (player nom info price) rst)
       (define p (make-pick current-team nom info price))
       #;(printf "pick: ~a\n" p)
       (loop current-team (set-add picks p) rst)]
      [_
       (list year picks)])))

;; String String String String -> Pick
(define (make-pick current-team nom info $price)
  (match-define (list player pos) (parse-player-info info))
  (define num (string->number nom))
  (define price (string->number (substring $price 1)))
  (pick current-team player pos price num))

(define PLAYER-RX #px"^(.+?) \\S+, (\\S+)$")
(define (parse-player-info info)
  (rest (regexp-match PLAYER-RX info)))

(module+ test
  (check-equal? (parse-player-info "Austin Ekeler LAC, RB")
                (list "Austin Ekeler" "RB"))
  (check-equal? (parse-player-info "Broncos D/ST Den, D/ST")
                (list "Broncos D/ST" "D/ST"))

  )

(define pos-order '("QB" "RB" "WR" "TE" "D/ST" "K"))

;; Pick Pick -> Bool
(define (pick>? p1 p2)
  (define pos1 (pick-pos p1))
  (define pos2 (pick-pos p2))
  (define i1 (index-of pos-order pos1))
  (define i2 (index-of pos-order pos2))
  (or (< i1 i2)
      (and (= i1 i2)
           (> (pick-amt p1) (pick-amt p2)))
      (and (= i1 i2)
           (= (pick-amt p1) (pick-amt p2))
           (> (pick-num p1) (pick-num p2)))))

;; Year (Setof Pick) -> Table
(define (build-year-table year picks)
  (define ordered-picks (sort (set->list picks) pick>?))
  (list* (list year ""        ""          ""        "")
         (list "By" "Player" "Position" "Price" "Nominated")
         (for/list ([p (in-list ordered-picks)])
           (list (pick-by p) (pick-who p) (pick-pos p) (pick-amt p) (pick-num p)))))

;; (Listof Table) -> Table
(define (append-tables tables)
  (define row-len (length (first (first tables))))
  (define mt-row (make-list row-len ""))
  (define longest-col (apply max (map length tables)))
  (define mt-col (make-list longest-col (list "")))
  (define normalized-tables
    (for/list ([tbl tables])
      (if (< (length tbl) longest-col)
          (append tbl (make-list (- longest-col (length tbl)) mt-row))
          tbl)))
  (apply map (lambda tbls (apply append tbls)) (add-between normalized-tables mt-col)))

(module+ main
  (define rows (csv->list (open-input-file draft-recaps)))
  (define years (transpose rows))
  (define year-picks (map process-year years))
  (define year-tables
    (for/list ([yr (in-list year-picks)])
      (define year (first yr))
      (build-year-table year (second yr))))
  (define all-years (append-tables year-tables))
  (define output-file (format "Data/Recaps.csv"))
  (with-output-to-file output-file
    #:exists 'replace
    (lambda ()
      (display-table all-years)))
  #;(for ([yr (in-list year-picks)])
    (define year (first yr))
    (define year-table (build-year-table year (second yr)))
    (define output-file (format "Data/Recap~a.csv" year))
    (with-output-to-file output-file
      #:exists 'replace
      (lambda ()
        (display-table year-table)))))
