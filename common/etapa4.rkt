#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")
;(require "checker.rkt")

(provide (all-defined-out))

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).

(define (longest-common-prefix w1 w2)
  (define (accumulate-prefix prefix w1 w2)
    (cond
      ((or (null? w1) (null? w2)) (list (reverse prefix) w1 w2))
      ((equal? (car w1) (car w2)) (accumulate-prefix (cons (car w1) prefix) (cdr w1) (cdr w2)))
      (else (list (reverse prefix) w1 w2))))
  (accumulate-prefix '() w1 w2))


; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection
(define (longest-common-prefix-of-collection words)
  (define (helper prefix words)
    (if (collection-empty? prefix)
        (if (= (collection-length words) 1)
            (collection-first words)
            (helper (car (longest-common-prefix (collection-first words) (collection-second words))) (collection-rest (collection-rest words))))
        (if (or (collection-empty? words) (null? (stream->list words)))
            prefix
            (helper (car (longest-common-prefix prefix (collection-first words))) (collection-rest words)))
        )

    )
  (helper empty-collection words)
  )


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


(define (get-suffixes text)
  (if (null? text)
      empty-collection
      (collection-cons text (get-suffixes (collection-rest text)))
      )
  )

(define (get-ch-words words ch)
  (collection-filter (λ (word) (if (collection? word)
                        (if (collection-empty? word)
                            #f
                            (equal? (collection-first word) ch)
                            )
                        (equal? word ch)
                        )
            )
          words)
  )


(define (ast-func suffixes)
  (if (collection-empty? suffixes)
      empty-collection
      (if (list? (collection-first suffixes))
          (cons (list (car (collection-first suffixes))) (collection-map collection-rest suffixes))
          (cons (list (collection-first suffixes)) (collection-rest suffixes)) 
          )
      )
  )


(define (cst-func suffixes)
  ; Folosesc functia asta din etapa intai ca sa gasesc cel mai lung prefix comun din lista
  (let ((prefix (longest-common-prefix-of-collection suffixes)))
    ; Pentru fiecare cuvant tai primele (length prefix) caractere din el
    (cons prefix (collection-map (λ (word) (collection-tail word (length prefix))) suffixes))))

; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)
(define (suffixes->st labeling-func suffixes alphabet)
  ; Steps 2 and 3
  (let ((prev-step (collection-map labeling-func (collection-filter (λ (el) (not (collection-empty? el))) (collection-map (λ (ch) (get-ch-words suffixes ch)) alphabet)))))
    (collection-map (λ (branch) 
           (if (equal? '(#\$) (car branch))
               ; The end of the branch
               (list (car branch))
               (collection-cons (car branch) (suffixes->st labeling-func (cdr branch) alphabet))
               )
           )
         prev-step)
    )
  )



; nu uitați să convertiți alfabetul într-un flux
(define text->st
  (λ (labeling-func)
    (λ (text)
      (let* (
             (stream-text (list->stream text))
             ($-terminated-text (stream-append stream-text '(#\$)))
             (suffixes (get-suffixes $-terminated-text))
             (debug (stream->list $-terminated-text))
             (alphabet (list->stream (remove-duplicates (stream->list (qsort char<? $-terminated-text)))))
             )
        (suffixes->st labeling-func suffixes alphabet)
        )
      )
    )
  )


(define text->ast
  (λ (text)
    ((text->st ast-func) text)
    )
  )

(define text->cst
  (λ (text)
    ((text->st cst-func) text)
    )
  )

(stream->list (text->ast (string->list "banana")))


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (substring? text pattern)
  'your-code-here)


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (repeated-substring-of-given-length text len)
  'your-code-here)