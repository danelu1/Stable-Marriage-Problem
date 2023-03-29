#lang racket

(provide (all-defined-out))

; ATENȚIE - Veți avea de reimplementat mai multe funcții
; din etapa 1, însă cu un alt mod de rezolvare (folosind
; funcționale sau alte funcții solicitate în enunț).
; Enunțul acestor exerciții va debuta prin "Ca în etapa 1,
; dar este interzisă recursivitatea explicită".


; TODO 1
; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Implementați o funcție care primește lista preferințelor
; bărbaților și calculează lista bărbaților din problemă.
; Folosiți orice funcțională exceptând foldl/foldr.

; Intoarcem lista obtinuta prin functionala
; "map", ca aplicare a functiei "car" asupra
; fiecarui element(adica asupra fiecarei liste).
(define (get-men mpref)
  (map car mpref))


; TODO 2
; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Implementați o funcție care primește lista preferințelor
; femeilor și calculează lista femeilor din problemă.
; Folosiți foldl sau foldr, astfel încât să nu fie necesare
; operații de tip append sau reverse.
(define (get-women wpref)
  (foldr (λ(p res) (cons (car p) res)) '() wpref))


; TODO 3
; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Implementați o funcție care primește o listă de liste
; de preferințe (ori lista preferințelor bărbaților, ori a
; femeilor) și o persoană ale cărei preferințe apar în listă,
; și întoarce lista preferințelor acelei persoane.
; Se garantează că persoana apare în listă.
; Exemplu: dacă în lista wpref apare linia (ana bobo adi cos),
; (get-pref-list wpref 'ana) => '(bobo adi cos)
; Folosiți minim o funcțională și minim o funcție anonimă.

; Prin utilizarea functionalei "filter" vom
; intoarce doar lista ce contine persoana "person"
; data ca argument initial. Functia anonima
; primita de "filter" are ca argument o lista
; si verifica daca primul element din lista este
; chiar persoana noastra.
(define (get-pref-list pref person)
  (cdar (filter (λ (lst) (equal? (car lst) person)) pref)))


; TODO 4
; Ca în etapa 1, dar este interzisă recursivitatea explicită
; și sunt permiși operatorii condiționali:
; Implementați o funcție care primește o listă de tipul
; întors la exercițiul precedent (lista preferințelor unei persoane),
; respectiv două persoane x și y care apar în lista respectivă,
; și întoarce true dacă x este mai sus decât y în topul preferințelor
; și false în caz contrar.
; Folosiți funcția member.

; Avand in vedere ca functia "member" intoarce lista formata
; de la primul element primit ca parametru pana la finalul
; listei, putem intoarce doar rezultatul verificarii faptului
; ca lungimea listei formata cu "memebr" aplicat lui "x" e mai mare decat
; lungimea listei formata cu "member" aplicat lui "y".
(define (preferable? pref-list x y)
  (cond
    ((equal? (member x pref-list) #f) #f)
    ((equal? (member y pref-list) #f) #t)
    (else (>
           (length (member x pref-list))
           (length (member y pref-list))))
    ))


; TODO 5
; Implementați recursiv funcționala find-first, care primește
; un predicat și o listă și întoarce primul element al listei
; care satisface predicatul, sau false dacă un asemenea element
; nu există.
; Implementarea trebuie să fie eficientă în sensul că nu trebuie
; să continue explorarea listei odată ce s-a găsit elementul.

; In cazul in care lista este nula, intoarcem false.
; In cazul in care primul element din lista satisface
; predicatul "p", atunci il intoarcem si cautarea
; inceteaza.
; In caz contrar celor 2 conditii de mai sus intoarcem
; "or" aplicat intre false si rezultatul cautarii in
; restul listei.
; Functia este eficienta deoarece cautarea inceteaza in
; momentul in care am gasit primul element ce satisface
; predicatul "p", adica atunci cand avem o conditie true
; in "or".
(define (find-first p L)
  (cond
    ((null? L) false)
    ((p (car L)) (car L))
    (else (or false (find-first p (cdr L))))
    ))


; TODO 6
; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Implementați o funcție care primește o listă de logodne
; (în care fiecare logodnă este o pereche cu punct între parteneri)
; și o persoană, și, în cazul în care această persoană apare pe prima
; poziție într-o logodnă, este întors partenerul său, altfel se
; întoarce false.
; Folosiți find-first, fără să îl apelați de 2 ori (hint: define în define).

; Imi defines o functie auxiliara in interiorul functiei
; "get-partner", ce intoarce primul element din lista
; "L"(primita ca parametru) care este egal cu persoana
; "person".
; Functia o folosesc in continuare pentru a verifica
; daca exista partenerul lui "person". In caz afirmativ
; intorc al doilea element din perechea gasita cu
; functionala "find-first".
(define (get-partner engagements person)
  (define (aux L)
    (find-first (λ(p) (equal? (car p) person)) L))
  (if (equal? (aux engagements) #f)
      #f
      (cdr (aux engagements))
      ))
  

; TODO 7
; Implementați recursiv funcționala change-first care primește
; un predicat p, o listă L și o valoare val, și întoarce o nouă 
; listă în care primul element din L care satisface predicatul p
; a fost înlocuit cu valoarea val, celelalte rămânând la fel.
; Dacă niciun element din L nu satisface predicatul, lista L
; rămâne neschimbată.

; Folosesc un "named let" pentru a putea asocia(lega) elementului
; intors de "find-first" o valoare(daca nu foloseam asta, atunci
; la fiecare apel recursiv, functia "find-first" intorcea  alta
; valoare, lucru ce imi modifica lista, ce trebuia sa rezulte,
; in mod gresit) si listei primite ca parametru o alta valoare.
; Verific initial daca lista e vida, caz in care intorc lista vida.
; Verific daca valoarea intoarsa de "find-first" este false, caz in
; care intorc lista, conform enuntului.
; Verific daca primul element din lista nu e valoarea intoarsa de
; "find-first", caz in care vom adauga recursiv aceleasi elemente
; din lista initiala in rezultat.
; In final, inlocuiesc valoarea intoarsa de "find-first" in
; momentul in care ajung la pozitia respectiva in lista.
(define (change-first p L val)
  (let loop ((lst L)
             (first-val (find-first p L)))
    (cond
      ((null? lst) '())
      ((equal? first-val #f) lst)
      ((not (equal? first-val (car lst))) (cons (car lst) (loop (cdr lst) first-val)))
      (else (cons val (loop (cdr lst) first-val)))
      )))


; TODO 8
; Implementați funcția update-engagements care primește o listă de
; logodne engagements și două persoane p1 și p2, și întoarce lista
; actualizată de logodne, în care vechiul partener al lui p1 este
; înlocuit cu p2.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - p1 era logodită în prealabil
; - fiecare cuplu din lista engagements are pe prima poziție
;   persoanele de același gen cu p1
; Folosiți change-first.

; Folosim "change-first" cu un predicat ce verifica daca s-a
; gasit partenerul lui p1, iar perechea lor este inlocuita cu perechea
; formata din persoana initiala "p1" si cea care inlocuieste
; partenerul, "p2".
(define (update-engagements engagements p1 p2)
  (change-first
   (λ (p) (equal? (cdr p) (get-partner engagements p1)))
   engagements (cons p1 p2)))


; TODO
; Copiați implementarea funcției better-match-exists? din etapa 1.
; Funcția nu este repunctată de checker, dar este necesară pentru
; implementarea funcției stable-match? de mai jos.
; Dacă nu ați implementat better-match-exists? în etapa 1, solicitați 
; o rezolvare de la asistent, astfel încât să puteți continua.
(define (better-match-exists? p1 p2 p1-list pref2 engagements)
  (cond
    ((equal? (car p1-list) p2) #f)
    (else (or (and (preferable? p1-list (car p1-list) p2)
                   (preferable? (get-pref-list pref2 (car p1-list)) p1
                                (get-partner engagements (car p1-list))))
              (better-match-exists? p1 p2 (cdr p1-list) pref2 engagements)))))


; TODO 9
; Implementați funcția stable-match? care primește o listă 
; completă de logodne engagements, o listă de preferințe masculine 
; mpref și o listă de preferințe feminine wpref, și întoarce true 
; dacă toate cuplurile din engagements sunt stabile.
; Un cuplu este stabil dacă pentru niciunul din membrii cuplului
; nu există un alt partener mai potrivit (conform definiției de
; la funcția better-match-exists?).
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie

; Functie ce inverseaza perechile dintr-o lista de perechi.
(define (reverse-pair-list L)
  (map (λ (l) (cons (cdr l) (car l)))
       L))

; Verificam pentru fiecare pereche din "engagements"
; daca nu exista un partener mai bun atat
; pentru "sot" cat si pentru "sotie". Cu ajutorul
; functionalei "map" cream o lista in care adaugam
; rezultatul stabilitatii fiecarui cuplu in parte din
; "engagements". Avand in vedere ca pe prima pozitie din
; fiecare pereche a listei de logodne se afla o femeie,
; vom fi nevoiti sa inversam perechile din lista de logodne
; in momentul in care verificam faptul ca nu exista un
; partener mai potrivit pentru femeia de pe prima pozitie
; a perechii(conform specificatiei din "etapa1", unde se
; zice clar ca in lista de logodne trebuie sa fie pe prima
; pozitie in fiecare pereche persoanele de acelasi sex cu "p2").
; La final, cu ajutorul functionalei "foldr", vom face "and"
; intre toate elementele din lista rezultata.
(define (stable-match? engagements mpref wpref)
  (foldr (λ (x y) (and x y))
         #t
         (map (λ (p)
                (and (not (better-match-exists? (car p) (cdr p)
                                                (get-pref-list wpref (car p))
                                                mpref (reverse-pair-list engagements)))
                     (not (better-match-exists? (cdr p) (car p)
                                                (get-pref-list mpref (cdr p))
                                                wpref engagements))))
              engagements)))