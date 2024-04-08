#lang racket

(provide (all-defined-out))

;; În acest fișier vă definiți constructorii și
;; operatorii tipului Collection.
;; În etapele anterioare, colecțiile erau de fapt
;; liste.
;; În definițiile de mai jos, veți considera că
;; o colecție este implementată ca flux.

; Întrucât stream-cons nu este o funcție obișnuită, 
; ci este definită ca o sintaxă specială, astfel
; încât ea să nu își evalueze argumentele înainte 
; de apel (comportament pe care ni-l dorim și pentru 
; collection-cons), nu putem folosi o definiție
; de tipul
;    (define collection-cons stream-cons)
; (genul acesta de definiție generează o eroare).
; Nici varianta
;    (define (collection-cons x xs) (stream-cons x xs))
; nu este o soluție, întrucât funcțiile definite de noi
; în Racket sunt funcții stricte, iar x și xs vor fi
; evaluate înainte de a intra în corpul funcției
; collection-cons și a descoperi că ele vor fi
; argumentele unui stream-cons.
; Modul de a defini collection-cons pentru a reproduce
; întocmai comportamentul lui stream-cons este:
(define-syntax-rule (collection-cons x xs) (stream-cons x xs))
; Obs: puteți schimba numele funcției, dacă nu vă
; place "collection-cons". Este o funcție folosită doar
; de voi în fișierul etapa4.rkt, nu de checker.


; TODO
; Scrieți în continuare restul definițiilor
; (care nu necesită o sintaxă specială).

(define (collection? collection) (stream? collection))

(define empty-collection empty-stream)

(define (collection-empty? collection) (equal? collection empty-collection))

(define (collection-first collection) (stream-first collection))

(define (collection-rest collection) (stream-rest collection))

(define (collection-second collection) (collection-first (collection-rest collection)))

(define collection-append stream-append)

(define collection-filter stream-filter)

(define collection-map stream-map)

(define (collection-reverse collection)
  (if (collection-empty? collection)
      empty-collection
      (collection-cons (collection-reverse (collection-rest collection)) (collection-first collection))
      )
  )

(define-syntax-rule (collection-length collection) (stream-length collection))

(define collection-tail stream-tail)

(define (list->stream L)
  (if (null? L)
      empty-stream
      (stream-cons (car L) (list->stream (cdr L)))))

(define (qsort lt? strm)
  (if (stream-empty? strm)
      empty-stream
      (let (
            (x (stream-first strm))
            (xs (stream-rest strm))
            )
        (stream-append
          (qsort lt?
            (stream-filter
              (lambda (u) (lt? u x))
              xs))
          (stream x)
          (qsort lt?
            (stream-filter
              (lambda (u) (not (lt? u x)))
              xs)
            )
          )
        )
      )
  )

(define remove-duplicates-collection (λ (collection)
  (stream-fold (λ (x y) (cons x (filter (lambda (z) (not (equal? x z))) y))) empty-collection collection)))