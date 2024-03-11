#lang racket


(require csv-reading)
(require data/maybe)

(define (display-and-read prompt)
  (display prompt)
  (display prompt)
  (read-line (current-input-port) 'any))

(define (sort-by-rating games)
  (sort games (lambda (a b) (string>? (list-ref a 12) (list-ref b 12)))))

(define (display-entry index entry)
  (fprintf (current-output-port) "~a. ~a - ~a MiLlion globally~n North America - ~a million~n Europe - ~a million~n Japan - ~a million~n Rest of World - ~a million~n~n"
           (add1 index)
           (third entry)
           (second (reverse entry))
           (eighth entry)
           (ninth entry)
           (tenth entry)
           (list-ref entry 10)))

(define (display-sorted-by-sales games region)
  (for ([entry games])
    (display-entry (index-of games entry) entry)))

(define (display-sorted-by-rating games region)
  (for ([entry (sort-by-rating games)])
    (fprintf (current-output-port) "~a. ~a - Rating: ~a/100~n" 
             (add1 (index-of (sort-by-rating games) entry))
             (third entry)
             (last entry))
    (fprintf (current-output-port) "Sales, ")
    (if (null? region)
        (fprintf (current-output-port) "Global - ~a million~n Platform - ~a~n Year - ~a~n Genre - ~a~n~n" (second (reverse entry)) (fourth entry) (fifth entry) (sixth entry))
        (fprintf (current-output-port) "~a - ~a million~n Platform - ~a~n Year - ~a~n Genre - ~a~n~n" (first region) (list-ref entry (second region)) (fourth entry) (fifth entry) (sixth entry)))))

(define (get-region-index region)
  (cond [(equal? region "North America") 7]
        [(equal? region "Europe") 8]
        [(equal? region "Japan") 9]
        [(equal? region "Rest of World") 10]))

(define (filter-by-name games name)
  (filter (lambda (n) (string-contains? (string-foldcase (caddr n)) (string-foldcase name))) games))

(define (filter-by-date games date-range)
  (filter (lambda (n) (and (string>=? (car (cddddr n)) (first date-range))
                           (string<=? (car (cddddr n)) (second date-range))))
          games))

(define (filter-by-publisher games publisher)
  (filter (lambda (n) (string-ci=? (caddr (cddddr n)) publisher)) games))

(define (filter-by-region games region)
  (filter (lambda (n) (string-ci=? (caddr n) region)) games))

(define (filter-by-genre games genre)
  (filter (lambda (n) (string-ci=? (cadr (cddddr n)) genre)) games))

(define (get-date-range)
  (list (display-and-read "Enter starting year YYYY (min: 1983): ")
        (display-and-read "Enter ending year YYYY (max: 2018): ")))

(define (region-search region)
  (list region (get-region-index region)))

(define (display-filtered games [region '()])
  (display "Would you like to order by sales or rating?:\nS) - Sales\nR) - Rating\n")
  (let ([sort-by (display-and-read "")])
    (cond
     [(string-ci=? sort-by "S") (display-sorted-by-sales games region)]
     [(string-ci=? sort-by "R") (display-sorted-by-rating games region)]
     [else (displayln "Invalid!") (display-filtered games)])))

(define (menu)
  (let loop ([search-criteria '()])
    (if (equal? (length search-criteria) 6) search-criteria 
    (let ([search (display-and-read "Select a criteria to be searched:\nN) - Name\nD) - Date Range\nP) - Publisher (e.g. Nintendo)\nR) - Region\nG) - Genre\nS) - Search or Quit\n")])
      (cond
        [(string-ci=? search "N") (loop (append search-criteria (list "Name" (display-and-read "Enter the name of the game:\n"))))]
        [(string-ci=? search "D") (loop (append search-criteria (list "Date" (get-date-range))))]
        [(string-ci=? search "P") (loop (append search-criteria (list "Publisher" (display-and-read "Enter the name of the Publisher:\n"))))]
        [(string-ci=? search "R") (loop (append search-criteria (list "Region" (region-search (display-and-read "Enter the region:\n")))))]
        [(string-ci=? search "G") (loop (append search-criteria (list "Genre" (display-and-read "Enter the genre:\n"))))]
        [(string-ci=? search "S") (display "Searching...") search-criteria]
        [else (displayln "Invalid entry") (loop (search-criteria))])))))

(define (filter-from-criteria search-list games)
  (if (null? search-list) nothing
      (let loop ([search-by search-list]
                 [filt-games games]
                 [region '()])
        (cond
          [(null? search-by) (display-filtered filt-games region)]
          [(equal? (first search-by) "Name")
           (loop (rest (cdr search-by)) (filter-by-name filt-games (second search-by)) region)]
          [(equal? (first search-by) "Date")
           (loop (rest (cdr search-by)) (filter-by-date filt-games (second search-by)) region)]
          [(equal? (first search-by) "Publisher")
           (loop (rest (cdr search-by)) (filter-by-publisher filt-games (second search-by)) region)]
          [(equal? (first search-by) "Region")
           (loop (rest (cdr search-by)) filt-games (second search-by))]
          [(equal? (first search-by) "Genre")
           (loop (rest (cdr search-by)) (filter-by-genre filt-games (second search-by)) region)]
          ))))

(define path "sales.csv")
(define games-reader (make-csv-reader-maker '((strip-leading-whitespace? . #t)
                                              (strip-trailing-whitespace? . #t))))
(define games-list (csv->list (games-reader (open-input-file path))))

(let loop ([results (filter-from-criteria (menu) games-list)])
  (if (equal? results nothing) (display "\nGoodbye...") (loop (filter-from-criteria (menu) games-list))))
