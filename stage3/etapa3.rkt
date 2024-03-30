#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")
(require "etapa2.rkt")

(provide (all-defined-out))

;; Această etapă este dedicată aplicațiilor 
;; arborelui de sufixe:
;; - găsirea unui șablon într-un text
;; - cel mai lung subșir comun a două texte
;; - găsirea unui subșir de lungime dată care se
;;   repetă în text
;; Conform convenției din etapele anterioare, un text
;; este întotdeauna reprezentat ca listă de caractere.
;; Rezultatele funcțiilor de mai jos sunt de asemenea
;; reprezentate ca liste de caractere.


; TODO 1
; Implementați funcția substring? care primește un text și
; un șablon nevid și întoarce true dacă șablonul apare în 
; text, respectiv false în caz contrar.
; Observație: ați implementat deja logica principală a
; acestei căutări în etapa 1, în funcția st-has-pattern?,
; care este un operator al tipului ST. Acum aveți toate
; uneltele necesare implementării operatorului corespunzător
; pentru tipul text (pentru că în etapa 2 ați implementat
; construcția arborelui de sufixe asociat unui text).
(define (substring? text pattern) (st-has-pattern? (text->cst text) pattern))


; TODO 2
; Implementați funcția longest-common-substring care primește
; două texte și determină cel mai lung subșir comun al
; acestora, folosind algoritmul următor:
; 1. Construiește arborele de sufixe ST1 pentru primul text.
; 2. Pentru fiecare sufix din al doilea text (de la cel mai
;    lung la cel mai scurt), găsește cea mai lungă potrivire 
;    cu sufixele din primul text, urmând căile relevante în ST1.
; 3. Rezultatul final este cel mai lung rezultat identificat
;    la pasul 2 (în caz de egalitate a lungimii, păstrăm
;    primul șir găsit).
; Folosiți named let pentru a parcurge sufixele.
; Observație: pentru sufixele din al doilea text nu dorim 
; marcajul de final $ pentru a nu crește artificial lungimea 
; șirului comun cu acest caracter.
; Hint: Revizitați funcția match-pattern-with-label (etapa 1).
(define (longest-common-substring text1 text2)
  (let*
      (
       (st1 (text->cst text1))
       (best-matches (map (λ (suffix) (longest-match st1 suffix)) (get-suffixes text2)))
       )
    
    ; Returns the first of the longest lists found
    (let iter ((remaining best-matches) (longest '()))
      (if (null? remaining)
          ; We finished iterating
          longest
          (if (> (length (car remaining)) (length longest))
              ; Update the longest list
              (iter (cdr remaining) (car remaining))
              ; The current list wasn't longer
              (iter (cdr remaining) longest)
              )
          )
      )
    )
  )


; TODO 3
; Implementați funcția repeated-substring-of-given-length
; care primește un text și un număr natural len și
; parcurge arborele de sufixe al textului până găsește un
; subșir de lungime len care se repetă în text.
; Dacă acest subșir nu există, funcția întoarce false.
; Obs: din felul în care este construit arborele de sufixe
; (pe baza alfabetului sortat), rezultatul va fi primul 
; asemenea subșir din punct de vedere alfabetic.
; Ideea este următoarea: orice cale în arborele de sufixe
; compact care se termină cu un nod intern (un nod care 
; are copii, nu este o frunză) reprezintă un subșir care
; se repetă, pentru că orice asemenea cale reprezintă un
; prefix comun pentru două sau mai multe sufixe ale textului.
; Folosiți interfața definită în fișierul suffix-tree
; atunci când manipulați arborele.
(define (repeated-substring-of-given-length text len)
  ; All of the local variables are defined here. I use letrec to make recursive functions work
  (letrec (
         (generate-sublists (λ (lst len) (cond
                                           ((= len 1) (map list lst))
                                           ((= len (length lst)) (list lst))
                                           (else
                                            (if (< len (length lst))
                                                (cons (take lst len)
                                                      (generate-sublists (cdr lst) len))
                                                '()
                                                )
                                            )
                                           )
                              )
                            )
         (compare-char-lists (λ (lst1 lst2) (cond
                                              [(empty? lst1) #t]                               
                                              [(empty? lst2) #f]                               
                                              [(char<? (car lst1) (car lst2)) #t]
                                              [(char<? (car lst2) (car lst1)) #f]                    
                                              [else (compare-char-lists (cdr lst1) (cdr lst2))]
                                              )
                               )
                             )
         (sorted-substrings (sort (generate-sublists text len) compare-char-lists))
         (find-duplicate (λ (st string)
                           (let
                               ((last-step (match-and-give-rest st string)))
                             (cond
                               ; We stop searching, the string was found completely
                               ((equal? (car last-step) #t)
                                (if (not (equal? (cadr last-step) '()))
                                    #t
                                    #f)
                                )
                               ; We stop searching, the string wasn't matched fully, only a prefix of it
                               ((equal? (car last-step) #f) #f)
                               ; We found a prefix of the string, and we keep searching for the rest using the new suffix tree and the rest of the pattern
                               (else (find-duplicate (caddr last-step) (cadr last-step)))
                               )
                             )
                           )
                         )
         (match-and-give-rest (λ (st string)   (define result (get-ch-branch st (car string)))
                                (if (false? result)
                                    (list #f '())
                                    (cond
                                      ((equal? (car (longest-common-prefix string (car result))) string)
                                       (list #t (cdr result)))
                                      ((and (< (length (car (longest-common-prefix string (car result)))) (length string)) (not (equal? (car (longest-common-prefix string (car result))) (car result))))
                                       (list #f (car (longest-common-prefix string (car result))))) 
                                      (else
                                       (list (car result) (cadr (longest-common-prefix string (car result))) (cdr result))))
                                    )
                                )
                              )
         ; This function is more efficient than simply using map
         (return-first-true (λ (func st lst) (cond
                                          ((null? lst) #f) 
                                          ((func st (car lst)) (car lst)) 
                                          (else (return-first-true func st (cdr lst)))
                                          )
                              )
                            )
         )
    (return-first-true find-duplicate (text->cst text) sorted-substrings)
    )
  )

