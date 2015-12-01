;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GRID.LISP                                          ;;;
;;;                                                    ;;;
;;; Ce fichier contient toutes les méthodes relatives  ;;;
;;; à l'édition et la gestion d'une grille.            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Variabes globales qui seront exportées plus haut

;; +alphabet+ est l'ensemble des caractères jouables (1 à 9 pour une
;; grille 9x9)
(defparameter +alphabet+ '(1 2 3 4 5 6 7 8 9 A B C D E F G H I J K L M N O P))

;; +n+ est la taille d'une région (donnant une grille n^2)
(defparameter +n+ 3)

;;; Fonctions d'interface (accès direct aux champs de la grille)

;; Constructeur de grille vide.
;; Chaque case contient '(<valeur_jouée> <liste_des_éléments_jouables>)
;; Quand la case est vide (pas de valeur_jouée), on met le caractère " "
;; (espace) qui permet de ne pas s'en préoccuper lors de l'affichage.
;; subseq permet de ne garder qu'une partie de l'alphabet.
(defun make-grid ()
  (let* ((size (* +n+ +n+))
	 (grid (make-array (list size size) :initial-element nil)))
    (dotimes (i size)
      (dotimes (j size)
	(let ((copy (copy-list (cons #\Space (subseq +alphabet+ 0 +n+)))))
	  (setf (aref grid i j) copy))))
    grid))

;; Retourne la valeur jouée dans la case (x, y)
;; /!\ x en vertical, y en horizontal
(defun get-grid-value (grid x y)
  (car (aref grid x y)))

;; Retourne la liste des éléments jouables dans la case (x, y)
(defun get-grid-possibilities (grid x y)
  (cdr (aref grid x y)))

;; Modifie la valeur jouée d'une case.
;; Renvoie NIL si c'était déjà la valeur jouée
(defun set-grid-value (grid x y c)
  (if (eq (car (aref grid x y)) c)
      nil
      (setf (car (aref grid x y)) c)))

;; Retire une valeur de la liste deséléments jouables d'une case
(defun remove-from-possibilities (grid x y c)
  (let ((case-car (car (aref grid x y)))
	(case-cdr (cdr (aref grid x y))))
    (setf (aref grid x y) (cons case-car (remove c case-cdr)))))

;; Vide la liste des éléments jouables d'une case
(defun remove-all-possibilities (grid x y)
  (let ((case-car (car (aref grid x y))))
    (setf (aref grid x y) (list case-car))))

;; Retourne T s'il ne reste d'une valeur jouable possible
(defun is-playable (grid x y)
  (let ((list (aref grid x y)))
    (and (endp (cddr list)) (not (endp (cdr list))))))

;; Retourne T si la case est déjà jouée (liste de possibilités vide)
(defun has-been-played (grid x y)
  (endp (cdr (aref grid x y))))
  
;;; Fonctions composées (via les fonctions d'interface)

;; Joue une valeur dans une case, et supprime cette valeur des éléments
;; jouables de sa ligne, colonne et région
(defun play (grid x y c)
  (if (eq c (get-grid-value grid x y))
      nil
      (let ((size (* +n+ +n+))
	    (x0 (* +n+ (floor (/ x +n+))))
	    (y0 (* +n+ (floor (/ y +n+)))))
	(dotimes (i size)
	  (remove-from-possibilities grid x i c)
	  (remove-from-possibilities grid i y c))
	(dotimes (i +n+)
	  (dotimes (j +n+)
	    (remove-from-possibilities grid (+ x0 i) (+ y0 j) c)))
	(remove-all-possibilities grid x y)
	(set-grid-value grid x y c))))

;; Joue une valeur dans une case si une seule possibilité demeure.
;; Renvoie NIL si aucune case n'a été jouée
(defun solve-one (grid)
  (let ((size (* +n+ +n+))
	(end-here nil))
    (dotimes (i size)
      (dotimes (j size)
	(if (is-playable grid i j)
	    (progn
	      (play grid i j (first (get-grid-possibilities grid i j)))
	      (setq end-here T)
	      (setq i size)
	      (setq j size)))))
    end-here))

;; Résoud la grille entière si elle peut l'être.
;; Renvoie NIL si la grille est insoluble (avec notre algorithme).
(defun solve-grid (grid)
  (loop until (not (solve-one grid)))
  (let ((size (* +n+ +n+))
	(end-here t))
    (dotimes (i size)
      (dotimes (j size)
	(if (not (has-been-played grid i j))
	    (progn
	      (setq i size)
	      (setq j size)
	      (setq end-here nil)))))
    end-here))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables et fonctions pour les tests en cours de développement

(defparameter *grid* (make-grid))

(defun init-grid ()
  (setq *grid* (make-grid)))

(defun solvable-grid ()
  (init-grid)
  (play *grid* 0 0 1)
  (play *grid* 2 1 1)
  (play *grid* 2 3 'A)
  (play *grid* 0 2 4)
  *grid*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fonctions servant à l'affichage d'une grille.
;; La fonction générale est en bas.

(defun draw-numbers-line (n x)
  (format t "║")
  (dotimes (i n)
    (dotimes (j n)
      (format t " ~D │" (get-grid-value *grid* x (+ (* i +n+) j))))
    (format t " ~D ║" (get-grid-value *grid* x (+ (* i +n+) n))))
  (dotimes (k n)
    (format t " ~D │" (get-grid-value *grid* x (+ (* +n+ n) k))))
  (format t " ~D ║~C" (get-grid-value *grid* x (+ (* +n+ n) n)) #\linefeed))

(defun draw-first-line (n)
  (format t "╔")
  (dotimes (i n)
    (dotimes (j n)
      (format t "═══╤"))
    (format t "═══╦"))
  (dotimes (k n)
    (format t "═══╤"))
  (format t "═══╗~C" #\linefeed))

(defun draw-between-lines (n)
  (format t "╟")
  (dotimes (i n)
    (dotimes (j n)
      (format t "───┼"))
    (format t "───╫"))
  (dotimes (k n)
    (format t "───┼"))
  (format t "───╢~C" #\linefeed))

(defun draw-between-regions (n)
  (format t "╠")
  (dotimes (i n)
    (dotimes (j n)
      (format t "═══╪"))
    (format t "═══╬"))
  (dotimes (k n)
    (format t "═══╪"))
  (format t "═══╣~C" #\linefeed))

(defun draw-last-line (n)
  (format t "╚")
  (dotimes (i n)
    (dotimes (j n)
      (format t "═══╧"))
    (format t "═══╩"))
  (dotimes (k n)
    (format t "═══╧"))
  (format t "═══╝~C" #\linefeed))

;; Elle appelle les milliards de fonctions qui travaillent derrière,
;; selon les besoins. Il s'agit essentiellement de codes similaires,
;; comprendre une des fonctions suffit, et de cette façon, c'est plus lisible.
;; Passer num en paramètre sert uniquement à ne pas recalculer (1- +n+).
;; "~C" #\linefeed
;; "~D" nb
(defun draw-grid ()
  (let ((num (1- +n+)))
    (draw-first-line num)
    (dotimes (i num)
      (dotimes (j num)
	(draw-numbers-line num (+ (* i +n+) j))
	(draw-between-lines num))
      (draw-numbers-line num (+ (* i +n+) num))
      (draw-between-regions num))
    (dotimes (k num)
      (draw-numbers-line num (+ (* +n+ num) k))
      (draw-between-lines num))
    (draw-numbers-line num (+ (* +n+ num) num))
    (draw-last-line num)))
