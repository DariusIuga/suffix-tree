#lang racket
(require "suffix-tree.rkt")
(provide (all-defined-out))


; TODO 2
; Implementați o funcție care primește două cuvinte (liste
; de caractere) w1 și w2 și calculează cel mai lung prefix
; comun al acestora, împreună cu restul celor două cuvinte
; după eliminarea prefixului comun.
; ex:
; (longest-common-prefix '(#\w #\h #\y) '(#\w #\h #\e #\n))
; => '((#\w #\h) (#\y) (#\e #\n))
; Folosiți recursivitate pe coadă.
(define (longest-common-prefix w1 w2)
  (define (accumulate-prefix prefix w1 w2)
    (cond
      ((or (null? w1) (null? w2)) (list (reverse prefix) w1 w2))
      ((equal? (car w1) (car w2)) (accumulate-prefix (cons (car w1) prefix) (cdr w1) (cdr w2)))
      (else (list (reverse prefix) w1 w2))))
  (accumulate-prefix '() w1 w2))


; TODO 3
; Implementați recursiv o funcție care primește o listă nevidă 
; de cuvinte care încep cu același caracter și calculează cel 
; mai lung prefix comun al acestora.
; Opriți căutarea (parcurgerea) în momentul în care aveți garanția 
; că prefixul comun curent este prefixul comun final.
(define (longest-common-prefix-of-list words)
  (define (helper prefix words)
    (if (null? prefix)
       (if (= (length words) 1)
          (car words)
          (helper (car (longest-common-prefix (car words) (cadr words))) (cddr words)))
       (if (null? words)
           prefix
           (helper (car (longest-common-prefix prefix (car words))) (cdr words)))
       )
    )
  (helper '() words)
)


;; Următoarele două funcții sunt utile căutării unui șablon
;; (pattern) într-un text cu ajutorul arborelui de sufixe.
;; Ideea de căutare este următoarea:
;; - dacă șablonul există în text, atunci există un sufix care
;;   începe cu acest șablon, deci există o cale care începe din
;;   rădăcina arborelui care se potrivește cu șablonul
;; - vom căuta ramura a cărei etichetă începe cu prima literă
;;   din șablon
;; - dacă nu găsim această ramură, șablonul nu apare în text
;; - dacă șablonul este conținut integral în eticheta ramurii,
;;   atunci el apare în text
;; - dacă șablonul se potrivește cu eticheta dar nu este conținut
;;   în ea (de exemplu șablonul "nana$" se potrivește cu eticheta
;;   "na"), atunci continuăm căutarea în subarborele ramurii
;; - dacă șablonul nu se potrivește cu eticheta (de exemplu
;;   șablonul "numai" nu se potrivește cu eticheta "na"), atunci
;;   el nu apare în text (altfel, eticheta ar fi fost "n", nu
;;   "na", pentru că eticheta este cel mai lung prefix comun al
;;   sufixelor din subarborele său)

; TODO 4
; Implementați funcția match-pattern-with-label care primește un
; arbore de sufixe și un șablon nevid și realizează un singur pas 
; din procesul prezentat mai sus - identifică ramura arborelui a
; cărei etichetă începe cu prima literă din șablon, apoi
; determină cât de bine se potrivește șablonul cu eticheta,
; întorcând ca rezultat:
; - true, dacă șablonul este conținut integral în etichetă
; - lista (etichetă, nou pattern, subarbore), dacă șablonul se
;   potrivește cu eticheta dar nu este conținut în ea
;   (ex: ("na", "na$", subarborele de sub eticheta "na")
;   pentru șablonul inițial "nana$" și eticheta "na")
; - lista (false, cel mai lung prefix comun între etichetă și
;   șablon), dacă șablonul nu s-a potrivit cu eticheta sau nu
;   s-a găsit din start o etichetă care începe cu litera dorită
;   (ex1: (false, "n") pentru șablonul "numai" și eticheta "na")
;   (ex2: (false, "") pentru etichetă negăsită)
; Obs: deși exemplele folosesc stringuri pentru claritate, vă
; reamintim că în realitate lucrăm cu liste de caractere.
(define (match-pattern-with-label st pattern)
  (define result (get-ch-branch st (car pattern)))
  (if (false? result)
      (list #f '())
      (cond
        ((equal? (car (longest-common-prefix pattern (car result))) pattern)
         #t)
        ((and (< (length (car (longest-common-prefix pattern (car result)))) (length pattern)) (not (equal? (car (longest-common-prefix pattern (car result))) (car result))))
         (list #f (car (longest-common-prefix pattern (car result))))) 
        (else
         (list (car result) (cadr (longest-common-prefix pattern (car result))) (cdr result))))))

; TODO 5
; Implementați funcția st-has-pattern? care primește un
; arbore de sufixe și un șablon și întoarce true dacă șablonul
; apare în arbore, respectiv false în caz contrar.
(define (st-has-pattern? st pattern)
  (equal? (length pattern) (length (longest-match st pattern)))
  )

(define (longest-match st pattern)
  (let
      ((last-step (match-pattern-with-label st pattern)))
    (cond
      ; We stop searching, the string was found completely
      ((equal? last-step #t) pattern)
      ; We stop searching, the string wasn't matched fully, only a prefix of it
      ((equal? (car last-step) #f) (cadr last-step))
      ; We found a prefix of the string, and we keep searching for the rest using the new suffix tree and the rest of the pattern
      (else (append (car last-step) (longest-match (caddr last-step) (cadr last-step))))
      )
    )
  )

