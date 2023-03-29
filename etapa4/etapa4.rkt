#lang racket

(require "etapa2.rkt")
(require "etapa3.rkt")

(provide (all-defined-out))

;; Preferințele bărbaților și femeilor din problemă se pot schimba
;; în timp, dar de obicei ele nu se schimbă radical de la un moment
;; la altul. De aceea, în loc să rulăm de la zero algoritmul
;; Gale-Shapley de fiecare dată când se schimbă ceva, preferăm să
;; pornim de la lista de logodne stabile obținută în pasul anterior
;; și să o actualizăm, conform algoritmului următor:
;; - eliminăm din engagements cuplurile care au devenit instabile
;;   în urma modificărilor de preferințe
;;   - cuplurile rămase sunt stabile între ele și considerăm că
;;     se găsesc împreună într-o cameră, în timp ce membrii cuplurilor
;;     destrămate stau la coadă la intrarea în cameră
;; - cât timp coada nu este goală
;;   - prima persoană p din coadă intră în cameră și încearcă să se
;;     cupleze cu cineva care este deja acolo, astfel:
;;     - p-list = lista de preferințe a lui p
;;     - determină prima persoană p' din p-list care este în cameră
;;     - dacă p' nu e logodită, logodește p' cu p
;;     - dacă p' e logodită
;;       - dacă p' îl preferă pe p partenerului actual p''
;;         - logodește p' cu p
;;         - încearcă să îl cuplezi pe p'' cu altcineva din cameră
;;           (folosind același algoritm)
;;       - altfel, treci la următoarea persoană din p-list (dacă
;;         aceasta există, altfel p rămâne temporar fără partener)


; TODO 1
; Implementați funcția match care primește o persoană person care
; intră în cameră, lista engagements a cuplurilor din cameră
; (cuplurile având pe prima poziție persoanele de gen opus lui 
; person), o listă pref1 care conține preferințele celor de același 
; gen cu person, o listă pref2 cu preferințele celor de gen diferit, 
; respectiv o coadă queue a persoanelor din afara camerei,
; și întoarce lista de cupluri actualizată astfel încât noile
; cupluri să fie stabile între ele.
; Această listă se obține ca rezultat al încercării de a cupla pe
; person cu cineva din cameră (person va încerca în ordine persoanele 
; din lista sa de preferințe), care poate duce la destrămarea
; unui cuplu și necesitatea de a cupla noua persoană rămasă singură
; cu altcineva din cameră, etc. Procesul continuă până când:
; - ori avem numai cupluri stabile între ele în cameră, nimeni
;   nefiind singur
; - ori toate persoanele rămase singure nu ar fi preferate de nimeni
;   altcineva din cameră, și în acest caz convenim să "logodim"
;   aceste persoane cu valoarea #f, astfel încât funcția să
;   întoarcă în aceeași listă atât informația despre cine din
;   cameră este logodit, cât și despre cine este singur

; Implementare cu similara cu "engage" de la etapa anterioara, dar
; de data aceasta verificam daca o persoana se afla sau nu in coada
; prin intermediul unei functii anonime din interiorul functiei noastre.
; In cazul in care se afla in coada persoana la care ne aflam la o
; iteratie din timpul algoritmului, atunci trecem la urmatoarea
; persoana din lista preferintelor.
; In cazul in care s-a golit lista preferintelor, atunci adauagam
; in rezultat perechea formata din persoana ramasa fara partener
; si "false".
(define (match person engagements pref1 pref2 queue)
  (let ([is-in-queue? (λ(p) (ormap (λ(e) (equal? e p)) queue))])
    (let loop ([pref-list (get-pref-list pref1 person)]
               [acc engagements])
      (if (null? pref-list)
          (append engagements (list (cons false person)))
          (if (is-in-queue? (car pref-list))
              (loop (cdr pref-list) acc)
              (let* ([person-in-pref-list (car pref-list)]
                     [partner (get-partner acc person-in-pref-list)]
                     [pref-list-for-person (get-pref-list pref2 person-in-pref-list)])
                (if (equal? partner false)
                    (update-engagements acc person-in-pref-list person)
                    (if (preferable? pref-list-for-person person partner)
                        (match partner (update-engagements acc person-in-pref-list person) pref1 pref2 queue)
                        (loop (cdr pref-list) acc)
                        ))))))))


; TODO 2
; Implementați funcția path-to-stability care primește lista
; engagements a cuplurilor din cameră, o listă de preferințe 
; masculine mpref, o listă de preferințe feminine wpref, respectiv
; coada queue a persoanelor din afara camerei, și întoarce lista
; completă de logodne stabile, obținută după ce fiecare persoană
; din queue este introdusă pe rând în cameră și supusă procesului
; descris de funcția match.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
; - persoanele nelogodite din cameră apar în engagements sub forma
;   (#f . nume-bărbat) sau (nume-femeie . #f)


; Functie ce determina o lista completa de logodne stabile.
; Fiecare persoana din coada intra in camera si este supusa procesului de
; "match".
; Pentru rezolvare am tinut cont atat de genul persoanei curente din coada
; ce urmeaza sa intre in camera, cat si de genul persoanei urmatoare ce
; urmeaza sa intre dupa aceasta. Cazurile in care restul cozii este vida sunt
; de asemenea tratate ca atare.
; Cazurile sunt urmatoarele:
; -> daca persoana actuala si persoana urmatoare eau acelasi gen
;    atunci modificam lista de logodne fara a o inversa.
; -> daca persoana actuala si persoana urmatoare au genuri diferite,
;    atunci modificam lista de logodne si o inversam pentru a putea
;    fi aplicate operatiile corespunzatoare(generate de "match")
;    in mod corect.
; Functie ajutatoare pentru a determina daca o persoana e
; barbat.
(define (is-man? person mpref)
  (ormap (λ(p) (equal? p person)) (get-men mpref)))

; La fel ca cea anterioara, dar pentru femei.
(define (is-woman? person wpref)
  (ormap (λ(p) (equal? p person)) (get-women wpref)))

; Functie care imi extrage toate femeile dintr-o coada.
(define (rearrange-queue-women queue wpref acc)
  (cond
    ((null? queue) acc)
    ((is-woman? (car queue) wpref) (rearrange-queue-women (cdr queue) wpref (append acc (list (car queue)))))
    (else (rearrange-queue-women (cdr queue) wpref acc))
    ))

; La fel, doar ca pentru barbati.
(define (rearrange-queue-men queue mpref acc)
  (cond
    ((null? queue) acc)
    ((is-man? (car queue) mpref) (rearrange-queue-men (cdr queue) mpref (append acc (list (car queue)))))
    (else (rearrange-queue-men (cdr queue) mpref acc))
    ))

; Functie care imi imbina functiile anterioare pentru a forma
; o noua coada in care femeile sunt pe primele pozitii si
; barbatii pe ultimele.
(define (rearrange-queue queue mpref wpref)
  (append (rearrange-queue-women queue wpref '())
          (rearrange-queue-men queue mpref '())))

(define (path-to-stability engagements mpref wpref queue)
  (let ([L (rearrange-queue queue mpref wpref)])
    (let loop ([acc engagements]
               [q L])
      (if (null? q)
          (filter (λ(p)
                    (and (not (equal? (car p) false))
                         (not (equal? (cdr p) false))
                         (not (is-man? (car p) mpref)))
                    )
                  acc)
          (let* ([person (car q)]
                 [reversed-engagements (reverse-pair-list acc)])
            (cond
              ((and (null? (cdr q))
                    (is-man? person mpref))
               (loop (match person acc mpref wpref (cdr q))
                     (cdr q)))
              ((and (null? (cdr q))
                    (is-woman? person wpref))
               (loop (match person reversed-engagements wpref mpref (cdr q))
                     (cdr q)))
              (else
               (let ([next-person (cadr q)])
                 (cond
                   ((and (not (null? next-person))
                         (is-man? person mpref)
                         (is-man? next-person mpref))
                    (loop (match person acc mpref wpref (cdr q))
                          (cdr q)))
                   ((and (not (null? next-person))
                         (is-woman? person wpref)
                         (is-woman? next-person wpref))
                    (loop (reverse-pair-list (match person reversed-engagements wpref mpref (cdr q)))
                          (cdr q)))
                   ((and (not (null? next-person))
                         (is-woman? person wpref)
                         (is-man? next-person mpref))
                    (loop (reverse-pair-list (match person reversed-engagements wpref mpref (cdr q)))
                          (cdr q)))
                   (else (loop acc (cdr q)))
                   )))))))))


; TODO 3
; Implementați funcția update-stable-match care primește o listă 
; completă de logodne engagements (soluția anterioară), o listă de 
; preferințe masculine mpref și o listă de preferințe feminine wpref 
; (adică preferințele modificate față de cele pe baza cărora s-a 
; obținut soluția engagements), și calculează o nouă listă de logodne 
; stabile - conform cu noile preferințe, astfel:
; - unstable = cuplurile instabile din engagements
; - room-engagements = engagements - unstable
; - queue = persoanele din unstable
; - aplică algoritmul path-to-stability
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie

; Pentru implementarea acestei functii am considerat o coada ce contine
; lista aplatizata a cuplurilor instabile din "engagements" si lista de
; logodne formata din cuplurile stabile ramase in camera, asupra carora
; am aplicat algoritmul "path-to-stability" implementat mai sus, pentru
; a obtine o lista completa de logodne stabile.
(define (update-stable-match engagements mpref wpref)
  (if (null? (get-unstable-couples engagements mpref wpref))
      (let* ([queue null]
             [room-engagements engagements])
        (path-to-stability room-engagements mpref wpref queue))
      (let* ([queue (get-couple-members (get-unstable-couples engagements mpref wpref))]
             [room-engagements (filter (λ(p)
                                         (and (not (better-match-exists? (car p) (cdr p) (get-pref-list wpref (car p))
                                                                         mpref (reverse-pair-list engagements)))
                                              (not (better-match-exists? (cdr p) (car p) (get-pref-list mpref (cdr p))
                                                                         wpref engagements))))
                                       engagements
                                       )])
        (path-to-stability room-engagements mpref wpref queue)
        )))


; TODO 4
; Implementați funcția build-stable-matches-stream care primește
; un flux pref-stream de instanțe SMP și întoarce fluxul de 
; soluții SMP corespunzător acestor instanțe.
; O instanță SMP este o pereche cu punct între o listă de preferințe
; masculine și o listă de preferințe feminine.
; Fluxul rezultat se va obține în felul următor:
; - primul element se calculează prin aplicarea algoritmului
;   Gale-Shapley asupra primei instanțe
; - următoarele elemente se obțin prin actualizarea soluției
;   anterioare conform algoritmului implementat în etapa 4 a temei
; Trebuie să lucrați cu interfața pentru fluxuri. Dacă rezolvați
; problema folosind liste și doar convertiți în/din fluxuri,
; punctajul pe acest exercițiu se anulează în totalitate.

; Functie auxiliara ce imi determina ultimul element al unei liste
(define (last-element l)
  (cond ((null? (cdr l)) (car l))
        (else (last-element (cdr l)))))

; Verific prima oara daca fluxul primit ca parametru este nul.
; In caz afirmativ returnez stream-ul nul, iar in caz contrar
; aplic algoritmul Gale-Shapley(implementat in etapa anterioara)
; asupra primei perechi de preferinte continute in flux.
; In continuare, generez fiecare lista de logodne obtinuta cu
; ajutorul iterarii prin fiecare pereche de preferinte din fluxul
; primit ca parametru, aplicand asupra fiecarei noi perechi algoritmul
; "update-stable-match" implementat in aceasta etapa. Astfel observam
; cum evolueaza logodnele in timp in functie de noile preferinte ale
; partenerilor.
(define (build-stable-matches-stream pref-stream)
  (if (stream-empty? pref-stream)
      empty-stream
      (let* ([mpref (stream-first (stream-first pref-stream))]
             [wpref (stream-rest (stream-first pref-stream))]
             [first-element (gale-shapley mpref wpref)])
        (if (stream-empty? (stream-rest pref-stream))
            first-element
            (let loop ([S (stream-rest pref-stream)]
                       [acc (list first-element)])
              (if (stream-empty? S)
                  acc
                  (let* ([men-preferences (stream-first (stream-first S))]
                         [women-preferences (stream-rest (stream-first S))]
                         [updated-engagements (update-stable-match (last-element acc) men-preferences women-preferences)])
                    (loop (stream-rest S) (append acc (list updated-engagements)))
                    )))))))