#lang racket

(require csv-reading)

; Load the data set from a CSV file
(define data (call-with-input-file "sales.csv" csv->list))

; Helper functions for filtering and sorting
(define (filter-by-name data game-name)
  (filter (lambda (row)
            (regexp-match? (regexp (string-ci=? game-name))
                           (string-ci=? (list-ref row 2))))
          data))

(define (filter-by-date data start-year end-year)
  (filter (lambda (row)
            (let ([year (string->number (list-ref row 4))])
              (displayln year)
              (and (>= year start-year)
                   (<= year end-year))))
          data))

(define (filter-by-publisher data publisher)
  (filter (lambda (row)
            (regexp-match? (regexp (string-ci=? publisher))
                           (string-ci=? (list-ref row 6))))
          data))

(define (filter-by-region data region)
  (filter (lambda (row)
            (case region
              ['north-america (> (string->number (list-ref row 7)) 0)]
              ['europe (> (string->number (list-ref row 8)) 0)]
              ['japan (> (string->number (list-ref row 9)) 0)]
              ['rest-of-world (> (string->number (list-ref row 10)) 0)]
              ['global (> (string->number (list-ref row 11)) 0)]))
          data))

(define (filter-by-genre data genre)
  (filter (lambda (row)
            (string-ci=? genre (list-ref row 5)))
          data))

(define (sort-by-rank data)
  (sort data < #:key (lambda (row) (string->number (list-ref row 1)))))

(define (sort-by-review data)
  (sort data > #:key (lambda (row) (string->number (list-ref row 12)))))

; Menu function
(define (menu)
  (printf "Enter up to 3 search criteria (or 'q' to quit):\n")
  (let loop ([criteria '()])
    (cond
      [(= (length criteria) 3) criteria]
      [else
       (printf "~a. Enter 'name', 'date', 'publisher', 'region', or 'genre' (or 'q' to quit): " (add1 (length criteria)))
       (let ([choice (string->symbol (read-line))])
         (cond
           [(eq? choice 'q) criteria]
           [(member choice '(name date publisher region genre)) (loop (cons choice criteria))]
           [else (printf "Invalid choice. Please try again.\n")
                 (loop criteria)]))])))

; Filter data based on user input
(define (filter-data criteria)
  (let loop ([criteria criteria] [filtered-data data])
    (cond
      [(null? criteria) filtered-data]
      [else
       (case (car criteria)
         ['name
          (let ([game-name (read-line)])
            (loop (cdr criteria)
                  (filter-by-name filtered-data game-name)))]
         ['date
          (printf "Enter the start year: ")
          (let ([start-year (string->number (read-line))])
            (printf "Enter the end year: ")
            (let ([end-year (string->number (read-line))])
              (if (> start-year end-year)
                  (loop (cdr criteria)
                        (filter-by-date filtered-data end-year start-year))
                  (loop (cdr criteria)
                        (filter-by-date filtered-data start-year end-year)))))]
         ['publisher
          (let ([publisher (read-line)])
            (loop (cdr criteria)
                  (filter-by-publisher filtered-data publisher)))]
         ['region
          (printf "Enter the region (north-america, europe, japan, rest-of-world, or global): ")
          (let ([region (string->symbol (read-line))])
            (loop (cdr criteria)
                  (filter-by-region filtered-data region)))]
         ['genre
          (printf "Enter the genre: ")
          (let ([genre (read-line)])
            (loop (cdr criteria)
                  (filter-by-genre filtered-data genre)))])])))

; Display filtered results
(define (display-results filtered-data)
  (printf "Sort results by 'rank' or 'review'? ")
  (let ([sort-choice (string->symbol (read-line))])
    (cond
      [(eq? sort-choice 'rank)
       (for-each (lambda (row)
                   (printf "~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a\n"
                           (list-ref row 1) ; Rank
                           (list-ref row 2) ; Game Title
                           (list-ref row 3) ; Platform
                           (list-ref row 4) ; Year
                           (list-ref row 5) ; Genre
                           (list-ref row 6) ; Publisher
                           (list-ref row 11) ; Global
                           (list-ref row 12))) ; Review
                 (sort-by-rank filtered-data))]
      [(eq? sort-choice 'review)
       (for-each (lambda (row)
                   (printf "~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a\n"
                           (list-ref row 1)
                           (list-ref row 2)
                           (list-ref row 3)
                           (list-ref row 4)
                           (list-ref row 5)
                           (list-ref row 6)
                           (list-ref row 11)
                           (list-ref row 12)))
                 (sort-by-review filtered-data))]
      [else (printf "Invalid sort choice. Displaying unsorted results.\n")
            (for-each (lambda (row)
                        (printf "~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a\n"
                                (list-ref row 1)
                                (list-ref row 2)
                                (list-ref row 3)
                                (list-ref row 4)
                                (list-ref row 5)
                                (list-ref row 6)
                                (list-ref row 11)
                                (list-ref row 12)))
                      filtered-data)])))

; Main function
(define (main)
  (let loop ()
    (let ([criteria (menu)])
      (unless (null? criteria)
        (let ([filtered-data (filter-data criteria)])
          (unless (null? filtered-data)
            (display-results filtered-data)))))
    (printf "Enter 'q' to quit or any other key to continue: ")
    (let ([choice (read-line)])
      (unless (string-ci=? choice "q")
        (loop)))))

; Run the main function
(main)