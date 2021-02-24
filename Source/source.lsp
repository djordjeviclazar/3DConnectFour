;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; pomocne funkcije;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; funkcija koja proverava da li je stap ispunjen
(defun ispunjenStap? (stap)
  (cond ((null stap) t)
        ((equal (car stap) '-) '())
        (t (ispunjenStap? (cdr stap)))
        )
  )

;; funkcija koja mapira koordinate(broj stapa) na slova
(defun stapKoordSlovo (koordStapa)
  (cond ((equal koordStapa 10) 'a)
        ((equal koordStapa 11) 'b)
        ((equal koordStapa 12) 'c)
        ((equal koordStapa 13) 'd)
        ((equal koordStapa 14) 'e)
        ((equal koordStapa 15) 'f)
        ((equal koordStapa 16) 'g)
        ((equal koordStapa 17) 'h)
        ((equal koordStapa 18) 'i)
        ((equal koordStapa 19) 'j)
        ((equal koordStapa 20) 'k)
        ((equal koordStapa 21) 'l)
        ((equal koordStapa 22) 'm)
        ((equal koordStapa 23) 'n)
        ((equal koordStapa 24) 'o)
        ((equal koordStapa 25) 'p)
        ((equal koordStapa 26) 'q)
        ((equal koordStapa 27) 'r)
        ((equal koordStapa 28) 's)
        ((equal koordStapa 29) 't)
        ((equal koordStapa 30) 'u)
        ((equal koordStapa 31) 'v)
        ((equal koordStapa 32) 'w)
        ((equal koordStapa 33) 'x)
        ((equal koordStapa 34) 'y)
        ((equal koordStapa 35) 'z)
        (t koordStapa)
        )
  )

; funkcija koja mapira slova na koordinate(broj) stapa
(defun stapSlovoKoord (slovoStapa)
  (cond ((equal slovoStapa 'a) 10)
        ((equal slovoStapa 'b) 11)
        ((equal slovoStapa 'c) 12)
        ((equal slovoStapa 'd) 13)
        ((equal slovoStapa 'e) 14)
        ((equal slovoStapa 'f) 15)
        ((equal slovoStapa 'g) 16)
        ((equal slovoStapa 'h) 17)
        ((equal slovoStapa 'i) 18)
        ((equal slovoStapa 'j) 19)
        ((equal slovoStapa 'k) 20)
        ((equal slovoStapa 'l) 21)
        ((equal slovoStapa 'm) 22)
        ((equal slovoStapa 'n) 23)
        ((equal slovoStapa 'o) 24)
        ((equal slovoStapa 'p) 25)
        ((equal slovoStapa 'q) 26)
        ((equal slovoStapa 'r) 27)
        ((equal slovoStapa 's) 28)
        ((equal slovoStapa 't) 29)
        ((equal slovoStapa 'u) 30)
        ((equal slovoStapa 'v) 31)
        ((equal slovoStapa 'w) 32)
        ((equal slovoStapa 'x) 33)
        ((equal slovoStapa 'y) 34)
        ((equal slovoStapa 'z) 35)
        ((typep slovoStapa 'real) slovoStapa)
        ((equal slovoStapa 'quit) 'quit)
        (t 'NIL)
        )
  )

;; funkcija koja ispisuje oznake koordinata za matricu igre
(defun pisiKoordinateKolona (n koord)
  (if (not (equal nNaKvadrat koord))
      (prog1 (format t "~a " (stapKoordSlovo koord)) (pisiKoordinateKolona n (1+ koord)))
      )
  )

;; funkcija koja stampa prosledjen red za stanje igre
(defun stampajRed (n stanje red)
  (loop for kolona from 0 to (1- nNaKvadrat)
      do (if (or (<= (+ (mod kolona n) red) (- n 2)) (>= (+ (mod kolona n) red) (1- (* n 2))))
             (format t "  ")
           (format t "~a " (nth (- (* (1- n) 2) (+ (mod kolona n) red)) (getStap kolona stanje)))
           )
        )
  )

;; funkcija koja na osnovu prosledjenog broja(koordinate, oznake) stapa vraca taj stap
(defun getStap (oznakaStapa stanjeStapova)
  (nth (stapSlovoKoord oznakaStapa) stanjeStapova)
  )

;; funkcija koja pravi prazan stap
(defun postavistap (n)
    (if (equal n 0) '();then
        ;else
        (cons '- (postavistap (- n 1)))
        )
  )

;; funkcija koja pravi tablu
(defun napravipomtablu (n len)
    (if (equal n 0) '();then
        ;else
        (cons (postavistap len) (napravipomtablu (- n 1) len))
        )
    )

; vraca null ako je stap popunjen
; ako stap nije skroz popunjen menja se prva pojava '-' sa 'X' ili 'O'
(defun vratistap (stap igrac)
    (if (null stap) '();then
        ;else
        (if (equal (car stap) '-) (cons igrac (cdr stap));then
            ;else
            (let ((novistap (vratistap (cdr stap) igrac)));dodela
                            ;izraz
                           (if (null novistap) '();then
                               ;else
                               (cons (car stap) novistap)
                               )
                 )
         )
        )
    )

; vraca null ako potez nije ispravan (ako nema mesta na stapu)
; stap kojim je definisan potez mora da postoji (potez ne sme da bude negativan ili veci od N)
; ako je sve u redu vraca poziciju nakon poteza
(defun vratipoziciju (pozicija potez igrac) 
    (if (equal potez 0) 
        ;then
        (let ((stap (vratistap (car pozicija) igrac)))
            (if (null stap) '();then
                ;else
                (cons stap (cdr pozicija))
                )
         )
        ;else
        (let ((novapozicija (vratipoziciju (cdr pozicija) (- potez 1) igrac)))
             (if (null novapozicija) '();then
                 ;else
                 (cons (car pozicija) novapozicija)
                 )
             )
     )
    )

;; funkcija za stampanje razmaka -- ne koristi se
;(defun stampajRazmake(brRazmaka trenutnoOdstampano)
;  (cond ((equal (1- brRazmaka) trenutnoOdstampano))
;        (t (progn (format t " ") (stampajRazmake brRazmaka (1+ trenutnoOdstampano))))
;        )
;  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; glavne funkcije ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; funkcija koja proverava da li su svi stapovi ispunjeni (kraj igre)
(defun tablaPuna? (stanjeTable)
  (cond ((null stanjeTable) t)
        ((ispunjenStap? (car stanjeTable)) (tablaPuna? (cdr stanjeTable)))
        (t '())
        )
  )

(defun fastTablaPuna? (N brojPoteza)
  (if (equal nNaTreci brojPoteza)
      ;then
      T
    ;else
    '()
    )
  )

;; funkcija koja stampa trenutno stanje na tabli
(defun stampajStanje (n stanje)
  (progn 
    (format t "~%")
    (pisiKoordinateKolona n 0)
    (format t "~%")
    (loop for red from 0 to (- (* 2 n) 2) ;; 0 <= red <= 2n - 2
        do (progn 
             (stampajRed n stanje red)
             (format t "~%")
             )
          )
    (pisiKoordinateKolona n 0)
    NIL
    )
  )

;; funkcija koja inicijalizuje dimenziju table, prvog igraca i postavlja tablu
(defun postaviIgru()
  (progn		
    (covekVsRacunar?)
    (ucitajDimenziju)
    (ucitajkoigra)
    (napraviTablu N)
    (stampajStanje N Tabla)
    )
  )

;; da li igraju covek vs covek ili covek vs racunar
(defun covekVsRacunar?()
  (progn 
    (format t "Da li igras protiv racunara? (Y/N)~%")
    (let ((unos (read)))
      (if (equal unos 'y)
          (setq cvr? T)
        (setq cvr? '())
        )
      )
    )
  )

;; funkcija koja ucitava dimenziju table
(defun ucitajDimenziju ()
    (progn
	(format t "Unesi dimenziju table:~%")
		(let ((unos (read)))
			(if (or (equal unos 4) (equal unos 6)) (progn (setq N unos) (setq nNaTreci (expt N 3)) (setq nNaKvadrat (expt N 2)));then
				;else
				(ucitajDimenziju)
			)
            )	
        )
)

;; funkcija koja ucitava da li igrac igra sa X ili O (X je uvek prvi)
(defun ucitajkoigra ()
    (progn
	(format t "Da li igrate sa 'X' ili 'O'?:~%")
		(let ((unos (read)))
			(if (or (equal unos 'X) (equal unos 'O)) (setq Player1 unos);then
				;else
				(ucitajkoigra)
			)
            )	
        )
)

;; funkcija koja postavlja tablu u promenljivu 'Tabla'
(defun napraviTablu(n)
    (let ((pomtabla (napravipomtablu nNaKvadrat n))) (setq Tabla pomtabla))
)

; potez je definisan  brojem ili znakom liste
; potez == 0 ako se koristi prvi stap
; potez je ispravan ako lista nije puna i broj liste > 0 i < n^2:
(defun odigraj (pozicija N potez igrac) 
    (if (or (equal potez '()) (< potez 0)) '() ;then
        ;else
        (if (> potez (- (* N N) 1)) '();then
            ;else
            (vratipoziciju pozicija potez igrac)
            )
     )
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;faza2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; pomocne funkcije;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; heuristika jedne liste (predstavlja jednu dijagonalu, štap, vertikalu ili horizontalu)
(defun prebroji (lista trenutni povezani)
    (let* ((heuristika (hprebroji lista trenutni povezani)))
          (cond
              ((> heuristika '3) (- heuristika '3))
              ((< heuristika '-3) (+ heuristika '3))
              (t '0)
              )
          )
    )

; osnovna f-ja koja je validna kod provere rezultata na kraju igre
(defun basicprebroji (lista trenutni povezani)
    (cond ( (eql trenutni '-) (prebroji (cdr lista) (car lista) '1) );test case
        ( (null lista)
         ;case 1
         (if (< povezani 4) '0;then
             ;else
             (if (eql trenutni 'X) '1;then
                 ;else 
                 '-1
                 )
             )
         )
        ( (< povezani 4)
         ; case 2:
         (if (eql trenutni (car lista)) (prebroji (cdr lista) trenutni (+ povezani 1));then
             ;else
             (prebroji (cdr lista) (car lista) '1)
             )
         )
        (t 
         ; default:
         (if (eql trenutni 'X) (+ 1 (prebroji lista trenutni (- povezani 1)));then
             ;else
             (- (prebroji lista trenutni (- povezani 1)) 1)
             )
         )
        )
    )

; proverava stanje na štapu
; pozicija - krajnja pozicija, broj - broj preostalih štapova za proveru
(defun proveristapove (pozicija broj dubina)
  (if (eql broj '0) '0;then
    ;else
    (+ (proveristapove pozicija (- broj 1) dubina) 
       (prebroji (nth (- broj 1) pozicija) '- '0)
       ;(heuristikastatic (nth (- broj 1) pozicija) broj)
       (if (or (eql dubina '-1) (> dubina (* nNaTreci 0.75)))
           ;then
           0
         ;else
         (heuristikastatic (nth (- broj 1) pozicija) broj)
         )
       )
    )
  )

; stap - broj štapa koji se trenutno posmatra
(defun izdvojihorizontalu (pozicija N visina horizontala stap)
    (if (<= (* N N) stap) '();then
        ;else
        (cons (nth visina (nth (+ stap horizontala) pozicija))
              (izdvojihorizontalu pozicija N visina horizontala (+ stap N))
              )
        )
    )

; visina - broj kuglice na štapu, horizontala - broj po?etnog štapa
(defun proverihorizontale (pozicija N visina horizontala)
    (if (>= visina N) '0;then
        ;else
        (+ (prebroji (izdvojihorizontalu pozicija N visina horizontala '0) '- '0)
           (proverihorizontale pozicija N 
                               (+ visina (floor (+ horizontala 1) N)) 
                               (mod (+ horizontala 1) N)
                               )
           )
     )
    )

; stap - broj štapa koji se trenutno posmatra
(defun izdvojivertikalu (pozicija N visina vertikala stap)
    (if (<= N stap) '();then
        ;else
        (cons (nth visina (nth (+ stap vertikala) pozicija) )
              (izdvojivertikalu pozicija N visina vertikala (+ stap 1))
              )
        )
    )

; visina - broj kuglice na štapu, vertikala - broj po?etnog štapa
(defun proverivertikale (pozicija N visina vertikala)
    (if (>= visina N) '0;then
        ;else
        (+ (prebroji (izdvojivertikalu pozicija N visina vertikala '0) '- '0)
           (proverivertikale pozicija N 
                               (+ visina (floor (+ vertikala N) (* N N))) 
                               (mod (+ vertikala N) (* N N))
                               )
           )
     )
    )
; Planarne Dijagonale:
; Pozitvine:
(defun izdvojiplanarnepozitivnehorizontalnedijagonale (pozicija N visina horizontala stap)
    (if (<= (- (* N N) (* N horizontala)) stap) '();then
        ;else
        (cons (nth visina (nth (+ stap horizontala) pozicija))
              (izdvojiplanarnepozitivnehorizontalnedijagonale pozicija N visina horizontala (+ (+ stap N) 1))
              )
     )
    )

(defun proveriplanarnepozitivnehorizontalnedijagonale (pozicija N visina horizontala)
    (if (>= visina N) '0;then
        ;else
        (+ (prebroji (izdvojiplanarnepozitivnehorizontalnedijagonale pozicija N visina (+ horizontala 1) '0) '- '0)
           (proveriplanarnepozitivnehorizontalnedijagonale pozicija N 
                               (+ visina (floor (+ horizontala 1) (- N 1))) 
                               (mod (+ horizontala 1) (- N 1))
                               )
           )
     )
    )

(defun izdvojiplanarnepozitivnevertikalnedijagonale (pozicija N visina vertikala stap)
    (if (<= (* N N) (+ stap vertikala)) '();then
        ;else
        (cons (nth visina (nth (+ stap vertikala) pozicija))
              (izdvojiplanarnepozitivnevertikalnedijagonale pozicija N visina vertikala (+ (+ stap N) 1))
              )
     )
    )

(defun proveriplanarnepozitivnevertikalnedijagonale (pozicija N visina vertikala)
    (if (>= visina N) '0;then
        ;else
        (+ (prebroji (izdvojiplanarnepozitivnevertikalnedijagonale pozicija N visina vertikala '0) '- '0)
           (proveriplanarnepozitivnevertikalnedijagonale pozicija N 
                               (+ visina (floor (+ vertikala N) (* N N))) 
                               (mod (+ vertikala N) (* N N))
                               )
           )
     )
    )

;Negativne
(defun izdvojiplanarnenegativnehorizontalnedijagonale (pozicija N visina horizontala stap)
    (if (<= (- (* N N) (* N (- N horizontala))) stap) '();then
        ;else
        (cons (nth visina (nth (+ stap horizontala) pozicija))
              (izdvojiplanarnenegativnehorizontalnedijagonale pozicija N visina horizontala (- (+ stap N) 1))
              )
     )
    )

(defun proveriplanarnenegativnehorizontalnedijagonale (pozicija N visina horizontala)
    (if (>= visina N) '0;then
        ;else
        (+ (prebroji (izdvojiplanarnenegativnehorizontalnedijagonale pozicija N visina (- (- N 1) (+ horizontala 1)) '0) '- '0)
           (proveriplanarnenegativnehorizontalnedijagonale pozicija N 
                               (+ visina (floor (+ horizontala 1) (- N 1))) 
                               (mod (+ horizontala 1) (- N 1))
                               )
           )
     )
    )

(defun izdvojiplanarnenegativnevertikalnedijagonale (pozicija N visina vertikala stap)
    (if (<= (* N N) (+ (+ stap 1) vertikala)) '();then
        ;else
        (cons (nth visina (nth (+ stap vertikala) pozicija))
              (izdvojiplanarnenegativnevertikalnedijagonale pozicija N visina vertikala (- (+ stap N) 1))
              )
     )
    )

(defun proveriplanarnenegativnevertikalnedijagonale (pozicija N visina vertikala)
    (if (>= visina N) '0;then
        ;else
        (+ (prebroji (izdvojiplanarnenegativnevertikalnedijagonale pozicija N visina vertikala (- N 1)) '- '0)
           (proveriplanarnenegativnevertikalnedijagonale pozicija N 
                               (+ visina (floor (+ vertikala N) (* N N))) 
                               (mod (+ vertikala N) (* N N))
                               )
           )
     )
    )

(defun proveriplanarnedijagonale (pozicija N)
    (+ (proveriplanarnepozitivnehorizontalnedijagonale pozicija N '0 '0)
       (proveriplanarnepozitivnevertikalnedijagonale pozicija N '0 '0)
       
       (proveriplanarnenegativnehorizontalnedijagonale pozicija N '0 '0) 
       '0
       (proveriplanarnenegativnevertikalnedijagonale pozicija N '0 '0)
     )
    )

; 3D Dijagonale:

; Pozitivne:
(defun izdvojipozitivnehorizontalnedijagonale (pozicija N visina horizontala stap)
    (if (or (<= (* N N) stap) (<= N visina)) '();then
        ;else
        (cons (nth visina (nth (+ stap horizontala) pozicija))
              (izdvojipozitivnehorizontalnedijagonale pozicija N (+ visina 1) horizontala (+ stap N))
              )
     )
    )

(defun proveripozitivnehorizontalnedijagonale (pozicija N visina horizontala matrica)
    (if (>= matrica (- N 3)) '0;then (3 == 4[broj povezanih u liniji] - 1)
        ;else
        (+ (prebroji (izdvojipozitivnehorizontalnedijagonale pozicija
                                                             N 
                                                             visina 
                                                             horizontala
                                                             (* matrica N)
                                                             ) 
                     '- 
                     '0
                     )
           (if (and (< matrica 1) (< (+ visina 1) N))
               ;then
               (proveripozitivnehorizontalnedijagonale pozicija N 
                               (mod (+ visina (floor (+ horizontala 1) N)) N) 
                               (mod (+ horizontala 1) N)
                               ;(+ matrica (floor (+ visina 1) N))
                               '0
                               )
               ;else
               (proveripozitivnehorizontalnedijagonale pozicija N 
                               '0 
                               (mod (- (+ horizontala 1) (floor (+ visina 1) N)) N)
                               (+ matrica (floor (+ horizontala 1) N) (floor (+ visina 1) N))
                               )
               )
           )
     )
    )

(defun izdvojipozitivnevertikalnedijagonale (pozicija N visina vertikala stap)
    (if (or (<= N stap) (<= N visina)) '();then
        ;else
        (cons (nth visina (nth (+ stap vertikala) pozicija))
              (izdvojipozitivnevertikalnedijagonale pozicija N (+ visina 1) vertikala (+ stap 1))
              )
     )
    )

(defun proveripozitivnevertikalnedijagonale (pozicija N visina vertikala matrica)
    (if (>= matrica (- N 3)) '0;then (3 == 4[broj povezanih u liniji] - 1)
        ;else
        (+ (prebroji (izdvojipozitivnevertikalnedijagonale pozicija
                                                             N 
                                                             visina 
                                                             vertikala
                                                             matrica
                                                             ) 
                     '- 
                     '0
                     )
           (if (< matrica 1)
               ;then
               (proveripozitivnevertikalnedijagonale pozicija N 
                               (mod (+ visina (floor (+ vertikala N) (* N N))) N) 
                               (mod (+ vertikala N) (* N N))
                               (+ matrica (floor (+ visina (floor (+ vertikala N) (* N N))) N))
                               )
               ;else
               (proveripozitivnevertikalnedijagonale pozicija N 
                               '0 
                               (mod (+ vertikala N) (* N N))
                               (+ matrica (floor (+ vertikala N) (* N N)))
                               )
               )
           )
        )
    )
; Negativne:
(defun izdvojinegativnehorizontalnedijagonale (pozicija N visina horizontala stap)
    (if (or (<= (* N N) stap) (< visina 0 )) '();then
        ;else
        (cons (nth visina (nth (+ stap horizontala) pozicija))
              (izdvojinegativnehorizontalnedijagonale pozicija N (- visina 1) horizontala (+ stap N))
              )
     )
    )

(defun proverinegativnehorizontalnedijagonale (pozicija N visina horizontala matrica)
    (if (>= matrica (- N 3)) '0;then (3 == 4[broj povezanih u liniji] - 1)
        ;else
        (+ (prebroji (izdvojinegativnehorizontalnedijagonale pozicija
                                                             N 
                                                             visina 
                                                             horizontala
                                                             (* matrica N)
                                                             ) 
                     '- 
                     '0
                     )
           (if (< matrica 1)
               ;then
               (proverinegativnehorizontalnedijagonale pozicija N 
                               (mod (+ visina (floor (+ horizontala 1) N)) N) 
                               (mod (- (+ horizontala 1) (floor (+ visina 1) N)) N)
                               (+ matrica (floor (+ visina 1) N))
                               )
               ;else
               (proverinegativnehorizontalnedijagonale pozicija N 
                               (- N 1) 
                               (mod (+ horizontala 1) N)
                               (+ matrica (floor (+ horizontala 1) N))
                               )
               )
           )
     )
    )


(defun izdvojinegativnevertikalnedijagonale (pozicija N visina vertikala stap)
    (if (or (<= N stap) (< visina 0)) '();then
        ;else
        (cons (nth visina (nth (+ stap vertikala) pozicija))
              (izdvojinegativnevertikalnedijagonale pozicija N (- visina 1) vertikala (+ stap 1))
              )
     )
    )

(defun proverinegativnevertikalnedijagonale (pozicija N visina vertikala matrica)
    (if (>= matrica (- N 3)) '0;then (3 == 4[broj povezanih u liniji] - 1)
        ;else
        (+ (prebroji (izdvojinegativnevertikalnedijagonale pozicija
                                                             N 
                                                             visina 
                                                             vertikala
                                                             matrica
                                                             ) 
                     '- 
                     '0
                     )
           (if (and (< matrica 1) (< (floor (+ visina (floor (+ vertikala N) (* N N))) N) 1))
               ;then
               (proverinegativnevertikalnedijagonale pozicija N 
                               (mod (+ visina (floor (+ vertikala N) (* N N))) N) 
                               (mod (+ vertikala N) (* N N))
                               (+ matrica (floor (+ visina (floor (+ vertikala N) (* N N))) N))
                               )
               ;else
               (proverinegativnevertikalnedijagonale pozicija N 
                               (- N 1) 
                               (mod (+ vertikala N) (* N N))
                               (+ matrica (floor (+ vertikala N) (* N N)))
                               )
               )
           )
        )
    )



; Glavne Dijagonale:
; Pozitivne
(defun izdvojiglavnepozitivnehorizontalnedijagonale (pozicija N visina horizontala stap)
    (if (or (<= (- (* N N) (* N horizontala)) stap) (<= N visina)) '();then
        ;else
        (cons (nth visina (nth (+ stap horizontala) pozicija))
              (izdvojiglavnepozitivnehorizontalnedijagonale pozicija N (+ visina 1) horizontala (+ (+ stap N) 1))
              )
     )
    )

(defun proveriglavnepozitivnehorizontalnedijagonale (pozicija N visina horizontala matrica)
    (if (>= matrica (- N 3)) '0;then (3 == 4[broj povezanih u liniji] - 1)
        ;else
        (+ (prebroji (izdvojiglavnepozitivnehorizontalnedijagonale pozicija
                                                             N 
                                                             visina 
                                                             (+ horizontala 1)
                                                             (+ (* matrica N) (* matrica 1))
                                                             ) 
                     '- 
                     '0
                     )
           (if (and (< matrica 1) (< (+ visina 1) N))
               ;then
               (proveriglavnepozitivnehorizontalnedijagonale pozicija N 
                               (mod (+ visina (floor (+ horizontala 1) (- N 1))) N) 
                               (mod (+ horizontala 1) (- N 1))
                               ;(+ matrica (floor (+ visina 1) N))
                               '0
                               )
               ;else
               (proveriglavnepozitivnehorizontalnedijagonale pozicija N 
                               '0 
                               (mod (- (+ horizontala 1) (floor (+ visina 1) N)) (- N 1))
                               (+ matrica (floor (+ horizontala 1) (- N 1)) (floor (+ visina 1) N))
                               )
               )
           )
     )
    )

(defun izdvojiglavnepozitivnevertikalnedijagonale (pozicija N visina vertikala stap)
    (if (or (<= (* N N) (+ stap vertikala)) (<= N visina)) '();then
        ;else
        (cons (nth visina (nth (+ stap vertikala) pozicija))
              (izdvojiglavnepozitivnevertikalnedijagonale pozicija N (+ visina 1) vertikala (+ (+ stap 1) N))
              )
     )
    )

(defun proveriglavnepozitivnevertikalnedijagonale (pozicija N visina vertikala matrica)
    (if (>= matrica (- N 3)) '0;then (3 == 4[broj povezanih u liniji] - 1)
        ;else
        (+ (prebroji (izdvojiglavnepozitivnevertikalnedijagonale pozicija
                                                             N 
                                                             visina 
                                                             vertikala
                                                             (+ (* matrica N) (* matrica 1))
                                                             ) 
                     '- 
                     '0
                     )
           (if (< matrica 1)
               ;then
               (proveriglavnepozitivnevertikalnedijagonale pozicija N 
                               (mod (+ visina (floor (+ vertikala N) (* N N))) N) 
                               (mod (+ vertikala N) (* N N))
                               (+ matrica (floor (+ visina (floor (+ vertikala N) (* N N))) N))
                               )
               ;else
               (proveriglavnepozitivnevertikalnedijagonale pozicija N 
                               '0 
                               (mod (+ vertikala N) (* N N))
                               (+ matrica (floor (+ vertikala N) (* N N)))
                               )
               )
           )
        )
    )

; Negativne:
(defun izdvojiglavnenegativnehorizontalnedijagonale (pozicija N visina horizontala stap)
    (if (or (<= (- (* N N) (* N (- (- N 1) horizontala))) (+ stap horizontala 1)) (< visina 0 )) '();then
        ;else
        (cons (nth visina (nth (+ stap horizontala) pozicija))
              (izdvojiglavnenegativnehorizontalnedijagonale pozicija N (- visina 1) horizontala (- (+ stap N) 1))
              )
     )
    )

(defun proveriglavnenegativnehorizontalnedijagonale (pozicija N visina horizontala matrica)
    (if (>= matrica (- N 3)) '0;then (3 == 4[broj povezanih u liniji] - 1)
        ;else
        (+ (prebroji (izdvojiglavnenegativnehorizontalnedijagonale pozicija
                                                             N 
                                                             (- (- N 1) visina) 
                                                             (- (- N 1) (+ horizontala 1))
                                                             (- (* matrica N) (ceiling matrica (+ matrica 1)))
                                                             ) 
                     '- 
                     '0
                     )
           (if (and (< matrica 1) (< (floor (+ visina 1) N) 1))
               ;then
               (proveriglavnenegativnehorizontalnedijagonale pozicija N 
                               (mod (+ visina (floor (+ horizontala 1) (- N 1))) N) 
                               (mod (- (+ horizontala 1) (floor (+ visina 1) N)) (- N 1))
                               (+ matrica (floor (+ visina 1) N))
                               )
               ;else
               (proveriglavnenegativnehorizontalnedijagonale pozicija N 
                               '0 
                               (mod (- (+ horizontala 1) (floor (+ visina 1) N)) (- N 1))
                               (+ matrica (floor (+ horizontala 1) (- N 1)) (floor (+ visina 1) N))
                               )
               )
           )
     )
    )

(defun izdvojiglavnenegativnevertikalnedijagonale (pozicija N visina vertikala stap)
    (if (or (<= (* N N) (+ (+ stap 1) vertikala)) (< visina 0)) '();then
        ;else
        (cons (nth visina (nth (+ stap vertikala) pozicija))
              (izdvojiglavnenegativnevertikalnedijagonale pozicija N (- visina 1) vertikala (+ (- stap 1) N))
              )
     )
    )

(defun proveriglavnenegativnevertikalnedijagonale (pozicija N visina vertikala matrica)
    (if (>= matrica (- N 3)) '0;then (3 == 4[broj povezanih u liniji] - 1)
        ;else
        (+ (prebroji (izdvojiglavnenegativnevertikalnedijagonale pozicija
                                                             N 
                                                             (- (- N 1) visina) 
                                                             vertikala
                                                             (+ (- N 1) (* matrica (- N 1)))
                                                             ) 
                     '- 
                     '0
                     )
           (if (< matrica 1)
               ;then
               (proveriglavnenegativnevertikalnedijagonale pozicija N 
                               (mod (+ visina (floor (+ vertikala N) (* N N))) N) 
                               (mod (+ vertikala N) (* N N))
                               (+ matrica (floor (+ visina (floor (+ vertikala N) (* N N))) N))
                               )
               ;else
               (proveriglavnenegativnevertikalnedijagonale pozicija N 
                               '0 
                               (mod (+ vertikala N) (* N N))
                               (+ matrica (floor (+ vertikala N) (* N N)))
                               )
               )
           )
        )
    )


(defun proveri3Ddijagonale (pozicija N)
    (+ '0
       (proveripozitivnehorizontalnedijagonale pozicija N '0 '0 '0)
       '0
       (proveripozitivnevertikalnedijagonale pozicija N '0 '0 '0)
       
       (proverinegativnehorizontalnedijagonale pozicija N '0 '0 '0)
       '0
       (proverinegativnevertikalnedijagonale pozicija N '0 '0 '0)
       
       
       (proveriglavnepozitivnehorizontalnedijagonale pozicija N '0 '0 '0)
        
       (proveriglavnepozitivnevertikalnedijagonale pozicija N '0 '0 '0)
       
       (proveriglavnenegativnehorizontalnedijagonale pozicija N '0 '0 '0)
       
       (proveriglavnenegativnevertikalnedijagonale pozicija N '0 '0 '0)
       
     )
    )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; glavne funkcije ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;quit kad se unese 'quit
(defun igra()
	(progn
		(postaviIgru)
   		(setq brPoteza '1)
		(glavna)
       ) 
)

(defun glavna()
  (progn
    (if (zerop (mod brPoteza 2)) (setq Player 'O);then 
      ;else
      (setq Player 'X)
      )
    (if (equal (potez player) 'kraj) 'kraj ;then
      ;else
      (progn
        (stampajStanje N Tabla)
        (if (equal (if (fastTablaPuna? N brPoteza)
                       ;then
                       (progn
                         (let ((r (rezultat Tabla N)))
                           (cond ((< r 0) (format t "~%~%~%Pobednik: O~%"))
                                 ((> r 0) (format t "~%~%~%Pobednik: X~%"))
                                 (t (format t "~%~%~%Remi~%"))
                                 )
                           )
                         'kraj
                         )
                     ;else
                     (setq brPoteza (+ brPoteza 1))
                     ) 'kraj)
            'kraj ;then
          (glavna) ;else
          )
        )
      )
    )
  )
		
(defun potez(igrac)
  (progn
    (if (or (not cvr?) (equal Player1 Player)) (format t "~%Unesi potez:~%"))
    (let ((unos (if (or (not cvr?) (equal Player1 Player))
                    ;then 
                    (stapSlovoKoord (read))
                  ;else
                  (format t "~%Na potezu je racunar...~%"))))
      (if (equal unos 'quit) 'kraj ;then
        ;else
        (let ((poz 
               (if (or (not cvr?) (equal Player1 Player)) 
                   ;then
                   (odigraj Tabla N unos Player)
                 ;else
                 (cdr (alfa-beta Player N 2 0 (mogucaStanja Tabla N Player 0 '()) -30000 30000))
                 )))
          (if (equal poz '()) (progn (format t "Nije ispravan potez, unesite ponovo.~%") (potez igrac));then
            ;else
            (progn
              (setq Tabla poz)
              (print brPoteza)
              (print Player)  
              )
            )
          )
        )
      )
    )
  )

;funkcija mogucaStanja vraca listu svih mogucih stanja/poteza koji se mogu dostici iz trenutnog stanja (trenutnoStanjeIgre) kada je na potezu igrac (igrac)
;stanja u povratnoj listi su poredjana tako da je prvo stanje u listi jednako potezu odigranom na poslednjem mogucem stapu (F za 4x4x4 ili Z za 6x6x6, ako su potezi moguci)
;za okretanje povratne liste koristimo reverse funkciju
;pocetniStap i trenutnaListaMogucihStanja su pomocne promenljive takve da se mogucaStanja poziva za pocetniStap = 0 i trenutaListaMoguichStanja = '()
;N je velicina table
(defun mogucaStanja (trenutnoStanjeIgre N igrac pocetniStap trenutnaListaMogucihStanja)
  (cond ((equal nNaKvadrat pocetniStap) trenutnaListaMogucihStanja)
        (t (let* ((potez (odigraj trenutnoStanjeIgre N pocetniStap igrac))
                  (listaSaOdigranimPotezom (cons potez trenutnaListaMogucihStanja)))
             (cond ((null potez) (mogucaStanja trenutnoStanjeIgre N igrac (1+ pocetniStap) trenutnaListaMogucihStanja))
                   (t (mogucaStanja trenutnoStanjeIgre N igrac (1+ pocetniStap) listaSaOdigranimPotezom))
                   )
             )
           )
        )
  )
  
  (defun rezultat (pozicija N &optional(dubina '-1))
    (+ (proveristapove pozicija (* N N) dubina)
       (proverihorizontale pozicija N '0 '0)
       (proverivertikale pozicija N '0 '0)
       '0
       (proveriplanarnedijagonale pozicija N)
       
       (proveri3Ddijagonale pozicija N)
     )
    )
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;faza3;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; pomocne funkcije;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun proverilistu (igrac N listapoteza alfa beta max dubina)
    (if (null listapoteza) (cons '-30000 '());then
        ;else
        (let* ((znak (if (eql igrac 'X) '1 '-1))
               (vrednost (cons (* znak (rezultat (car listapoteza) N dubina)) (car listapoteza)))
               (maxpom (if (< (car max) (car vrednost)) vrednost max))
               (alfapom (if (< alfa (car maxpom)) (car maxpom) alfa))
               )
              (if (>= alfapom beta) maxpom;then
                  ;else
                  (let* ((vrednostpom (proverilistu igrac N (cdr listapoteza) alfapom beta maxpom dubina)))
                        (if (< (car maxpom) (car vrednostpom)) vrednostpom;then
                            ;else
                            maxpom
                            )
                        )
                  )
              )
        )
    
    )

(defun alfa-beta-recursive (igrac N maxdubina dubina listapoteza alfa beta max)
    (if (null listapoteza) (cons '-30000 '());then
        ;else
        (let* ((novalistapoteza (mogucaStanja (car listapoteza) N (if (eql 'X igrac) 'O 'X) '0 '()))
               (vrednost (car (alfa-beta (if (eql 'X igrac) 'O 'X) N maxdubina dubina novalistapoteza (- beta) (- alfa))))
               (maxpom (if (< (car max) (- vrednost)) (cons (- vrednost) (car listapoteza)) max))
               (alfapom (if (< alfa (car maxpom)) (car maxpom) alfa))
               )
              (if (>= alfapom beta) maxpom;then
                  ;else
                  (let* ((vrednostpom (alfa-beta-recursive igrac N maxdubina dubina (cdr listapoteza) alfapom beta maxpom)))
                        (if (< (car maxpom) (car vrednostpom)) vrednostpom;then
                            ;else
                            maxpom
                            )
                        )
                  )
              )
        )
    
    )

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; glavne funkcije ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun alfa-beta (igrac N maxdubina dubina listapoteza alfa beta)
    (if (or (eql maxdubina dubina) (fastTablaPuna? N (+ brPoteza dubina)))
        ;then
        (proverilistu igrac N listapoteza alfa beta (cons '-30000 '()) (+ brPoteza dubina))
        ;else
        (alfa-beta-recursive igrac N maxdubina (+ dubina 1) listapoteza alfa beta (cons '-30000 '()))
     )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; faza 4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; globalne promenljive;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq vrednostiglobal '(
                        (0.0667 0.05 0.033);0
                        (0.05 0.05 0.0389);1
                        (0.033 0.0389 0.044);2
                        (0.05 0.061 0.055);3
                        (0.0389 0.055 0.072);4
                        (0.044 0.072 0.1);5
                        )
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; pomocne funkcije;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;nova heuristika ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; traži najduži niz povezanih elemenata ukljucujuci i prazna polja
; XXXX => 4, XXXX- => 4.5
(defun hprebroji (lista trenutni povezani)
    (if (null lista) povezani;then
        ;else
        (let* ((sledeci (car lista)))
              ;(progn (print sledeci)
              (cond 
                  ((eql trenutni 'X)
                   ;case X:
                   (cond 
                       ((eql sledeci 'X) (hprebroji (cdr lista) sledeci (+ povezani 1)));X X
                       ((eql sledeci 'O) (if (> povezani '3);X O
                                             ;then
                                             povezani
                                             ;else
                                             (hprebroji (cdr lista) sledeci '-1)
                                             )
                                         )
                       (t ;(if (< povezani '3);X -
                              ;then
                              ;(hprebroji (cdr lista) sledeci '0.5)
                              ;else
                              (hprebroji (cdr lista) sledeci (+ povezani '0.5))
                              ;)
                          )
                       )
                   )
                  ((eql trenutni 'O)
                   ;case O:
                   (cond 
                       ((eql sledeci 'O) (hprebroji (cdr lista) sledeci (- povezani 1)));? ?
                       ((eql sledeci 'X) (if (< povezani '-3);O X
                                             ;then
                                             povezani
                                             ;else
                                             (hprebroji (cdr lista) sledeci '1)
                                             )
                                         )
                       (t ;(if (> povezani '-3);O -
                              ;then
                              ;(hprebroji (cdr lista) sledeci '0.5)
                              ;else
                              (hprebroji (cdr lista) sledeci (- povezani '0.5))
                              ;)
                          )
                       )
                   )
                  ((eql trenutni '-)
                   ;case -
                   (cond 
                       ((eql sledeci 'O) (cond ;- O
                                             ((and (< '1 (length lista)) (eql (cadr lista) '-)) (if (<= povezani '-3) (- povezani '0.5);then
                                                                        ;else
                                                                        (hprebroji (cdr lista) (car lista) '-1)
                                                                        )
                                                                    )
                                             ((>= povezani '3) povezani)
                                             ((> povezani '0) (hprebroji (cdr lista) sledeci '-1.5))
                                             (t (hprebroji (cdr lista) sledeci (- povezani '1)))
                                             )
                                         )
                       ((eql sledeci 'X)  (cond ;- X
                                              ((and (< '1 (length lista)) (eql (cadr lista) '-)) (if (>= povezani '3) (+ povezani '0.5);then
                                                                        ;else
                                                                        (hprebroji (cdr lista) (car lista) '1)
                                                                        )
                                                                    )
                                             ((<= povezani '-3) povezani)
                                             ((< povezani '0) (hprebroji (cdr lista) sledeci '1.5))
                                             (t (hprebroji (cdr lista) sledeci (+ povezani '1)))
                                             )
                                          )
                       (t (if (or (< povezani '3) (> povezani '3));- -
                              ;then
                              povezani
                              ;else
                              (hprebroji (cdr lista) sledeci '0.5))
                              )
                       ;(t 'a)
                       )
                   )
                  (t 
                   ;default
                   (cond 
                       ((eql sledeci 'O) (hprebroji (cdr lista) (car lista) '-1))
                       ((eql sledeci 'X) (hprebroji (cdr lista) (car lista) '1))
                       (t (hprebroji (cdr lista) (car lista) '0.5))
                       )
                   )
                  )
                  ;)
              )
        )
    )


;;;;;;;;;;;;;;;;;;staticka heuristika ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; lista - trenutni štap, broj - redni broj štapa
; vraca heuristiku za poziciju kuglica na štapu
(defun heuristikastatic (lista broj)
    (let* ((listavrednosti (mapiraj (length lista) broj)))
          (returnstatic lista ; štap
                        (copyvaluesoptimal (nth listavrednosti vrednostiglobal) (length lista)); kopiranje liste vrednosti
                        '0;pocetni indeks
                        )
          )
    )

; mapira broj štapa na indeks iz vrednostiglobal
(defun mapiraj (N broj)
    (let* ((indeks (- broj 1)); indeks štapa
           ;osnovne koord.
           (dubina (floor indeks N))
           (sirina (mod indeks N))
           
           ;prave dimenzije(prva simetrija)
           ;minimum ostatka i nedostatka
           (moduo (floor N 2))
           
           (pravadubina (if (< dubina moduo) dubina;then
                            ;else
                            (- (- N dubina) 1)
                            )
                        )
           
           (pravasirina (if (< sirina moduo) sirina;then
                            ;else
                            (- (- N sirina) 1)
                            )
                        )
           )
          ;dimenzije nakon dijagonalne simtrije
          (cond 
              ((eql pravadubina '0) pravasirina)
              ((> pravadubina pravasirina) (+ pravadubina pravasirina pravasirina))
              ;((eql pravadubina '1) (+ (- moduo 1) (max pravadubina pravasirina)))
              ((eql pravadubina (- moduo 1)) 5)
              (t (+ (- moduo 1) (max pravadubina pravasirina)))
              )
          )
    )


(defun mapirajbasic (N broj)
    (cond 
        ((eql broj 1) '0);pocetak/cošak/prvi/zadnji
        ((eql (floor N 2) broj) '2);sredina/centar
        (t '1);ostalo
        )
    )

; mapira staticke vrednosti na svaku poziciju i vraca heuristiku (<0 ako je 'O' u prednosti)
(defun returnstatic (lista vrednosti i)
    (if (null lista) '0;then
        ;else
        (let* ((sledeci (car lista))
               (vrednost (car vrednosti)))
               (if (eql '- sledeci) '0;then
                   ;else
                   (+ (returnstatic (cdr lista) (cdr vrednosti) (+ i 1))
                      (if (eql 'X sledeci)
                          ;then
                          ;pozitivna vrednost za X
                          vrednost
                          ;else
                          ;negativna vrednost za O
                          (- vrednost)
                          )
                      )
                   )
              )
        )
    )




(defun copyvalues (values N i)
            (if (eql i N) '();then
                ;else
                (cons (nth (if (< i (floor N 2)) i (- (- N i) 1)) values)
                      (copyvalues values N (+ i 1))
                      )
                )
            )

; primenjuje simetriju izmedu gornjeg i donjeg dela štapa za proizvoljnu dimenziju            
(defun copyvaluesoptimal (values N)
    (if (eql N '6) (append values (reverse values));then 6*6*6
        ;else
        ; 4*4*4
        (append (cons (first values) (last values)) (reverse (cons (first values) (last values))))
        )
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; glavne funkcije ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;U ovoj fazi je izmenjena pomocna f-ja proveristapove
;F-ja prebroji je promenjena, staroj definiciji je promenjeno ime u basicprebroji

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;test
(igra)
