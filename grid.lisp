;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GRID.LISP                                          ;;;
;;;                                                    ;;;
;;; Ce fichier contient toutes les m�thodes relatives  ;;;
;;; � l'�dition et la gestion d'une grille.            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Variabes globales qui seront export�es plus haut

;; +alphabet+ est l'ensemble des caract�res jouables (1 � 9 pour une
;; grille 9x9)
(defparameter +alphabet+ '(1 2 A 4))

;; +n+ est la taille d'une r�gion (donnant une grille n^2)
(defparameter +n+ 2)

;;; Fonctions d'interface (acc�s direct aux champs de la grille)

;; Constructeur de grille vide.
;; Chaque case contient '(<valeur_jou�e> <liste_des_�l�ments_jouables>)
(defun make-grid ()
  (let* ((size (* +n+ +n+))
	 (grid (make-array (list size size) :initial-element nil)))
    (dotimes (i size)
      (dotimes (j size)
	(let ((copy (copy-list (cons 0 +alphabet+))))
	  (setf (aref grid i j) copy))))
    grid))

;; Retourne la valeur jou�e dans la case (x, y)
;; /!\ x en vertical, y en horizontal
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

;; Retire une valeur de la liste des�l�ments jouables d'une case
(defun remove-from-possibilities (grid x y c)
  (let ((case-car (car (aref grid x y)))
	(case-cdr (cdr (aref grid x y))))
    (setf (aref grid x y) (cons case-car (remove c case-cdr)))))

;; Vide la liste des �l�ments jouables d'une case
(defun remove-all-possibilities (grid x y)
  (let ((case-car (car (aref grid x y))))
    (setf (aref grid x y) (list case-car))))

;; Retourne T s'il ne reste d'une valeur jouable possible
(defun is-playable (grid x y)
  (let ((list (aref grid x y)))
    (and (endp (cddr list)) (not (endp (cdr list))))))
  
;;; Fonctions compos�es (via les fonctions d'interface)

;; Joue une valeur dans une case, et supprime cette valeur des �l�ments
;; jouables de sa ligne, colonne et r�gion
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

;; Joue une valeur dans une case si une seule possibilit� demeure.
;; Renvoie NIL si aucune case n'a �t� jou�e
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

;;; Variables et fonctions pour les tests en cours de d�veloppement

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
  
