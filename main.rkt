#lang racket

(struct sale (index rank title year platform genre publisher na-sales europe japan world global review) #:transparent)

(define (import-record port)
  (let ([rec (read-line port)])
    (if (eof-object? rec)
        eof
        (match (split-string rec ",")
          [(index rank title year platform genre publisher na-sales europe japan world global review)
           (sale data
                 (string->number index)
                 (string->number rank)
                 (string->string title)
                 (string->string platform)
                 (string->string year)
                 (string->string genre)
                 (string->number na-sales)
                 (string->number europe)
                 (string->number japan)
                 (string->number world)
                 (string->number global)
                 (string->number review))]
          [_ (error "Failed to load record.")]))))
                 
                 
                 

