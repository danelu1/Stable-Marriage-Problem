#lang racket

(require "etapa2.rkt")

(provide (all-defined-out))

; TODO 1
; După modelul funcției stable-match?, implementați funcția
; get-unstable-couples care primește o listă de logodne
; engagements, o listă de preferințe masculine mpref și o 
; listă de preferințe feminine wpref, și întoarce lista
; tuturor cuplurilor instabile din engagements.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
; Nu este permisă recursivitatea pe stivă.
; Nu sunt permise alte funcții ajutătoare decât
; better-match-exists? și funcțiile de manipulare a listelor de
; preferințe definite în etapele anterioare.
; Nu este permisă apelarea multiplă a aceleiași funcții pe
; aceleași argumente.
; Folosiți una sau mai multe dintre expresiile let, let*, letrec,
; named let pentru a vă putea conforma acestor restricții.

; Implementare similara cu functia "stable-match?" din etapa
; anterioara.
; In interiorul "let*"-ului am o functie ce intoarce  lista
; de perechi(cu perechile inversate) si o lista reprezentand
; lista initiala in care cuplurile stabile sunt inlocuite cu
; "'()", iar cele instabile sunt lasate sub forma initiala de
; pereche. La final se foloseste "filter" pentru a exatrage
; toaate elementele diferite de "'()" din lista finala.
(define (get-unstable-couples engagements mpref wpref)
  (let* ([rev (λ(L) (map (λ(p) (cons (cdr p) (car p))) L))]
         [lst (map (λ(p)
                     (let* ([woman (car p)]
                            [man (cdr p)]
                            [woman-pref-list (get-pref-list wpref woman)]
                            [man-pref-list (get-pref-list mpref man)]
                            [reversed-engagements (rev engagements)]
                            [check-for-woman (better-match-exists? woman man woman-pref-list mpref reversed-engagements)]
                            [check-for-man (better-match-exists? man woman man-pref-list wpref engagements)])
                       (if (and (not check-for-woman)
                                (not check-for-man))
                           '()
                           (cons woman man))
                       )
                     ) engagements)])
    (filter (λ(e) (not (equal? e '()))) lst)
    ))

; TODO 2
; Implementați funcția engage care primește o listă free-men
; de bărbați încă nelogodiți, o listă de logodne parțiale 
; engagements (unde fiecare cuplu are pe prima poziție o femeie),
; o listă de preferințe masculine mpref și o listă de preferințe 
; feminine wpref, și întoarce o listă completă de logodne stabile,
; obținută conform algoritmului Gale-Shapley:
; - cât timp există un bărbat m încă nelogodit
;   - w = prima femeie din preferințele lui m pe care m nu a cerut-o încă
;   - dacă w este nelogodită, adaugă perechea (w, m) la engagements
;   - dacă w este logodită cu m'
;     - dacă w îl preferă pe m lui m'
;       - m' devine liber
;       - actualizează partenerul lui w la m în engagements
;     - altfel, repetă procesul cu următoarea femeie din lista lui m
; Folosiți named let pentru orice proces recursiv ajutător (deci nu
; veți defini funcții ajutătoare recursive).
; Folosiți let și/sau let* pentru a evita calcule duplicate.

; Pentru implementarea acestei functii am urmat cu exactitate pasii
; algoritmului Gale-Shapley descrisi mai sus:
; -> am luat un "loop1", creat cu ajutorul unui "named let", in care
;    am retinut lista de "free-men" si pe cea de "engagements", iar
;    apoi am inceput sa iterez prin cea de "free-men", cat timp aceasta
;    este nevida.
; -> in interiorul lui "loop1" am considerat un "loop2", in care am
;    retinut lista actuala de "free-men" si lista preferintelor barbatului
;    curent prin care iteram si extragem de fiecare data(cu ajutorul unui
;    "let*") atat prima femeie din lista, cat si partenerul acesteia, in
;    cazul in care va trebui sa-l adaugam la lista de "free-men".
; -> verific la final daca femeia nu are partener, caz in care perechea,
;    formata din ea si barbatul care o cere, este adaugata la rezultat.
; -> in cazul in care aceasta are partener, verific daca barbatul care
;    o cere e mai sus in topul preferintelor acesteia decat partenerul
;    actual, caz in care partenerul devine liber, iar rezultatul este
;    actualizat cu ajutorul functiei "update-engagements".
(define (engage free-men engagements mpref wpref)
  (let loop1 ([L free-men]
              [acc engagements])
    (if (null? L)
        acc
        (let* ([man (car L)])
          (let loop2 ([l L]
                      [man-pref-list (get-pref-list mpref man)])
            (let* ([woman (car man-pref-list)]
                   [partner (get-partner acc woman)]
                   [woman-pref-list (get-pref-list wpref woman)])
              (if (equal? partner false)
                  (loop1 (cdr L) (append acc (list (cons woman man))))
                  (if (preferable? woman-pref-list man partner)
                      (loop1 (append (cdr L) (list partner)) (update-engagements acc woman man))
                      (loop2 l (cdr man-pref-list)))
                  )))))))


; TODO 3
; Implementați funcția gale-shapley care este un wrapper pentru
; algoritmul implementat de funcția engage. Funcția gale-shapley
; primește doar o listă de preferințe masculine mpref și o listă
; de preferințe feminine wpref și calculează o listă completă de
; logodne stabile conform acestor preferințe.
(define (gale-shapley mpref wpref)
  (engage (get-men mpref) '() mpref wpref))


; TODO 4
; Implementați funcția get-couple-members care primește o listă
; de perechi cu punct și întoarce o listă simplă cu toate elementele 
; care apar în perechi.
; Folosiți funcționale, fără recursivitate explicită.
(define (get-couple-members pair-list)
  (cond ((null? pair-list) '())
        ((pair? pair-list)
         (append (my-flatten (car pair-list)) (my-flatten (cdr pair-list))))
        (else (list pair-list))))