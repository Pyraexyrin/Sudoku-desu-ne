;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                    ;;;
;;;                     GRID.LISP                      ;;;
;;;                                                    ;;;
;;; Ce fichier contient toutes les m�thodes relatives  ;;;
;;;      � l'�dition et la gestion d'une grille.       ;;;
;;;                                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                    ;;;
;;; Variables :                                        ;;;
;;;                                                    ;;;
;;; +alphabet+                                         ;;;
;;; +n+                                                ;;;
;;; +size+                                             ;;;
;;; +subalpha+                                         ;;;
;;;                                                    ;;;
;;; Fonctions :                                        ;;;
;;;                                                    ;;;
;;; (make-grid)                                        ;;;
;;; (get-grid-value grid x y)                          ;;;
;;; (get-grid-possibilities grid x y)                  ;;;
;;; (set-grid-value grid x y c)                        ;;;
;;; (remove-from-possibilities grid x y c)             ;;;
;;; (remove-all-possibilities grid x y)                ;;;
;;; (is-playable grid x y)                             ;;;
;;; (has-been-played grid x y)                         ;;;
;;; (play grid x y c)                                  ;;;
;;; (draw-coordonated-line)                            ;;;
;;; (draw-numbers-line grid n x)                       ;;;
;;; (draw-first-line n)                                ;;;
;;; (draw-between-line n)                              ;;;
;;; (draw-between-regions n)                           ;;;
;;; (draw-last-line n)                                 ;;;
;;; (draw-grid grid)                                   ;;;
;;;                                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    VARIABLES                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; +alphabet+ est l'ensemble des caract�res jouables
;; (1 � 9 pour une grille 9x9)
(defparameter +alphabet+
  '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
    #\A #\B #\C #\D #\E #\F #\G #\H #\I
    #\J #\K #\L #\M #\N #\O #\P))

;; +n+ est la taille d'une r�gion (donnant une grille n^2)
(defparameter +n+ 3)

;; Variables permettant d'�viter des calculs (utilis�es
;; fr�quemment)
(defparameter +size+ (* +n+ +n+))
(defparameter +subalpha+ (subseq +alphabet+ 0 +size+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    FONCTIONS                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Constructeur de grille vide.
;; Chaque case contient '(<valeur_jou�e> <liste_des_�l�ments_jouables>)
;; Quand la case est vide (pas de valeur_jou�e), on met le caract�re " "
;; (espace) qui permet de ne pas s'en pr�occuper lors de l'affichage.
(defun make-grid ()
  (let ((grid (make-array (list +size+ +size+) :initial-element nil)))
    (dotimes (i +size+)
      (dotimes (j +size+)
	(let ((copy (copy-list (cons #\Space +subalpha+))))
	  (setf (aref grid i j) copy))))
    grid))

;; Retourne la valeur jou�e dans la case (x, y)
;; /!\ x est la ligne, y la colonne
(defun get-grid-value (grid x y)
  (car (aref grid x y)))

;; Retourne la liste des �l�ments jouables dans la case (x, y)
(defun get-grid-possibilities (grid x y)
  (cdr (aref grid x y)))

;; Modifie la valeur jou�e d'une case.
;; Renvoie NIL si c'�tait d�j� la valeur jou�e
(defun set-grid-value (grid x y c)
  (if (eq (car (aref grid x y)) c)
      nil
      (setf (car (aref grid x y)) c)))

;; Retire une valeur de la liste des �l�ments jouables d'une case
(defun remove-from-possibilities (grid x y c)
  (let ((case-car (car (aref grid x y)))
	(case-cdr (cdr (aref grid x y))))
    (setf (aref grid x y) (cons case-car (remove c case-cdr)))))

;; Vide la liste des �l�ments jouables d'une case
(defun remove-all-possibilities (grid x y)
  (let ((case-car (car (aref grid x y))))
    (setf (aref grid x y) (list case-car))))

;; Retourne T s'il ne reste qu'une valeur jouable possible
(defun is-playable (grid x y)
  (let ((list (aref grid x y)))
    (and (endp (cddr list)) (not (endp (cdr list))))))

;; Retourne T si la case est d�j� jou�e (liste de possibilit�s vide)
(defun has-been-played (grid x y)
  (endp (cdr (aref grid x y))))

;; Joue une valeur dans une case, et supprime cette valeur des �l�ments
;; jouables de sa ligne, colonne et r�gion
(defun play (grid x y c)
  (if (position c (get-grid-possibilities grid x y))
      (let ((x0 (* +n+ (floor (/ x +n+))))
	    (y0 (* +n+ (floor (/ y +n+)))))
	(dotimes (i +size+)
	  (remove-from-possibilities grid x i c)
	  (remove-from-possibilities grid i y c))
	(dotimes (i +n+)
	  (dotimes (j +n+)
	    (remove-from-possibilities grid (+ x0 i) (+ y0 j) c)))
	(remove-all-possibilities grid x y)
	(set-grid-value grid x y c))
      nil))

;; Fonctions servant � l'affichage d'une grille.
;; La fonction g�n�rale est en bas.

(defun draw-coordonates-line ()
  (format t "~C   " #\linefeed)
  (dotimes (i +size+)
    (format t "   ~D" (nth i +alphabet+)))
  (format t "~C" #\linefeed))

(defun draw-numbers-line (grid n x)
  (format t " ~D  *" (nth x +alphabet+))
  (dotimes (i n)
    (dotimes (j n)
      (format t " ~D |" (get-grid-value grid x (+ (* i +n+) j))))
    (format t " ~D *" (get-grid-value grid x (+ (* i +n+) n))))
  (dotimes (k n)
    (format t " ~D |" (get-grid-value grid x (+ (* +n+ n) k))))
  (format t " ~D *~C" (get-grid-value grid x (+ (* +n+ n) n)) #\linefeed))

(defun draw-first-line (n)
  (format t "    *")
  (dotimes (i n)
    (dotimes (j n)
      (format t "****"))
    (format t "****"))
  (dotimes (k n)
    (format t "****"))
  (format t "****~C" #\linefeed))

(defun draw-between-lines (n)
  (format t "    *")
  (dotimes (i n)
    (dotimes (j n)
      (format t "----"))
    (format t "---*"))
  (dotimes (k n)
    (format t "----"))
  (format t "---*~C" #\linefeed))

(defun draw-between-regions (n)
  (format t "    *")
  (dotimes (i n)
    (dotimes (j n)
      (format t "****"))
    (format t "****"))
  (dotimes (k n)
    (format t "****"))
  (format t "****~C" #\linefeed))

(defun draw-last-line (n)
  (format t "    *")
  (dotimes (i n)
    (dotimes (j n)
      (format t "****"))
    (format t "****"))
  (dotimes (k n)
    (format t "****"))
  (format t "****~C" #\linefeed))

;; Elle appelle les milliards de fonctions qui travaillent derri�re,
;; selon les besoins. Il s'agit essentiellement de codes similaires,
;; comprendre une des fonctions suffit, et de cette fa�on, c'est plus lisible.
;; Passer num en param�tre sert uniquement � ne pas recalculer (1- +n+).
(defun draw-grid (grid)
  (let ((num (1- +n+)))
    (draw-coordonates-line)
    (draw-first-line num)
    (dotimes (i num)
      (dotimes (j num)
	(draw-numbers-line grid num (+ (* i +n+) j))
	(draw-between-lines num))
      (draw-numbers-line grid num (+ (* i +n+) num))
      (draw-between-regions num))
    (dotimes (k num)
      (draw-numbers-line grid num (+ (* +n+ num) k))
      (draw-between-lines num))
    (draw-numbers-line grid num (+ (* +n+ num) num))
    (draw-last-line num)
    (format t "~C" #\linefeed)))
