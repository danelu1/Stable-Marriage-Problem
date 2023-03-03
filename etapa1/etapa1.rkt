#lang racket

(provide (all-defined-out))

; TODO 1
; Implementați o funcție care primește lista preferințelor
; bărbaților și calculează lista bărbaților din problemă.
; Folosiți recursivitate pe stivă.

; Verificam initial daca lista este vida.
; In caz afirmativ intoarcem lista vida.
; Altfel vom adauga in mod recursiv in
; lista primul element din fiecare sublista
(define (get-men mpref)
  (cond
    ((null? mpref) '())
    (else (append (list (car (car mpref))) (get-men (cdr mpref))))))


; TODO 2
; Implementați o funcție care primește lista preferințelor
; femeilor și calculează lista femeilor din problemă.
; Folosiți recursivitate pe coadă.

; Similar task-ului anterior doar ca de data aceasta
; am folosit recursivitate pe coada.
(define (get-women-tail l acc)
  (cond
    ((null? l) (reverse acc))
    (else (append (list (car (car l))) (get-women-tail (cdr l) acc)))))

(define (get-women wpref)
  (get-women-tail wpref '()))


; TODO 3
; Implementați o funcție recursivă care primește o listă de liste
; de preferințe (ori lista preferințelor bărbaților, ori a
; femeilor) și o persoană ale cărei preferințe apar în listă,
; și întoarce lista preferințelor acelei persoane.
; Se garantează că persoana apare în listă.
; Exemplu: dacă în lista wpref apare linia (ana bobo adi cos),
; (get-pref-list wpref 'ana) => '(bobo adi cos)
; Observație: de fiecare dată când ne referim la lista
; preferințelor unei persoane p, ne referim la o listă care conține
; doar persoanele de sex opus, nu și pe p pe prima poziție.

; Cazul de baza este acela pentru care lista de liste este vida.
; In cazul in care am gasit lista ce contine persoana primita
; ca argument in antetul functiei, intoarcem restul listei respective.
; In caz contrar apelam recursiv functia.
(define (get-pref-list pref person)
  (cond
    ((null? pref) '())
    ((equal? (car (car pref)) person) (cdr (car pref)))
    (else (get-pref-list (cdr pref) person))))


; TODO 4
; Implementați o funcție recursivă care primește o listă de tipul
; întors la exercițiul precedent (lista preferințelor unei persoane),
; respectiv două persoane x și y care apar în lista respectivă,
; și întoarce true dacă x este mai sus decât y în topul preferințelor
; și false în caz contrar.
; Nu folosiți operatori condiționali, folosiți în schimb operatori
; logici pentru a obține același efect.

; Mai jos am comentat codul pentru functia scrisa folosind operatori
; conditionali, ghidandu-ma astfel dupa aceasta.
; Folosind operatori logici, vom intoarce fie faptul ca lista de
; preferinte este vida, fie faptul ca lista de preferinta nu este vida,
; primul element din lista este chiar "x" si functia va fi falsa
; pentru restul elementelor, fie faptul ca lista de preferinte nu
; este vida, primul element din lista nu este "y" si functia va ramane
; adevarata pentru restul elementelor din lista.
(define (preferable? pref-list x y)
  (or (null? pref-list)
      (and (not (null? pref-list))
           (equal? (car pref-list) x)
           (not (preferable? (cdr pref-list) x y)))
      (and (not (null? pref-list))
           (not (equal? (car pref-list) y))
           (preferable? (cdr pref-list) x y))))
   
  ;(cond
    ;((null? pref-list) #f)
    ;((equal? (car pref-list) x) #t)
    ;((equal? (car pref-list) y) #f)
    ;(else (preferable? (cdr pref-list) x y))))


; TODO 5
; Implementați o funcție recursivă care primește o listă de logodne
; (în care fiecare logodnă este o pereche cu punct între parteneri)
; și o persoană, și, în cazul în care această persoană apare pe prima
; poziție într-o logodnă, este întors partenerul său, altfel se
; întoarce false.
; Folosiți cond.

;rezolvare similara task-ului 3
(define (get-partner engagements person)
  (cond
    ((null? engagements) false)
    ((equal? (car (car engagements)) person) (cdr (car engagements)))
    (else (get-partner (cdr engagements) person))))


; TODO 6
; Implementați o funcție care primește 2 persoane logodite, p1 și p2,
; lista preferințelor lui p1, lista preferințelor tuturor persoanelor
; de același gen cu p2, respectiv lista tuturor logodnelor, și întoarce
; true dacă există un partener mai potrivit pentru p1, și false altfel.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - lista logodnelor este completă, este un posibil rezultat al problemei
; - logodnele din listă au pe prima poziție persoana de același gen cu p2
; - un partener p' este mai potrivit decât p2 dacă îndeplinește 2 condiții:
;   - p1 îl preferă pe p' în raport cu p2
;   - p' îl preferă pe p1 în raport cu persoana cu care este logodit

; Pentru rezolvarea acestui task am considerat un caz de baza care verifica
; daca primul element din lista preferintelor lui "p1" este chiar "p2"
; (partenerul lui "p1"), deoarece astfel suntem convinsi ca nu exista un
; partener mai bun pentru "p1" decat "p2", deci problema este rezolvata.
; In caz contrar, am tradus relatiile de "potrivire" descrise mai sus
; astfel:
; - "p1" prefera pe "p'" in detrimentul lui "p2" si "p'" prefera pe "p1"
;   in detrimentul partenerului sau <=> "p'" se afla mai sus in topul
;   preferintelor lui "p1" decat se afla "p2" SI "p1" se afla mai sus
;   in topul preferintelor lui "p'" decat partenerul sau. Aceste 2
;   relatii se traduc cu ajutorul functiilor implementate mai sus si
;   anume:
;   - "preferable?" pentru verificarea in lista de preferinte;
;   - "get-pref-list" pentru aflarea listei preferintelor lui "p'";
;   - "get-partner" pentru gasirea partenerului lui "p'".
(define (better-match-exists? p1 p2 p1-list pref2 engagements)
  (cond
    ((equal? (car p1-list) p2) #f)
    (else (or (and (preferable? p1-list (car p1-list) p2)
                   (preferable? (get-pref-list pref2 (car p1-list)) p1
                                (get-partner engagements (car p1-list))))
              (better-match-exists? p1 p2 (cdr p1-list) pref2 engagements)))))