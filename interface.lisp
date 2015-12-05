;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                    ;;;
;;;                  INTERFACE.LISP                    ;;;
;;;                                                    ;;;
;;;   Ce fichier est le fichier "point d'entrée". Il   ;;;
;;;  est l'interface entre le joueur et le programme.  ;;;
;;;                                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                    ;;;
;;; Variables :                                        ;;;
;;;                                                    ;;;
;;; +grid+                                             ;;;
;;; +solved-grid+                                      ;;;
;;; +loaded-grid+                                      ;;;
;;; gtest (grille temporaire)                          ;;;
;;;                                                    ;;;
;;; Fonctions :                                        ;;;
;;;                                                    ;;;
;;; (ask-player)                                       ;;;
;;; (play-sudoku)                                      ;;;
;;; (step-by-step-sudoku)                              ;;;
;;; (sudoku grid :solve :steps)                        ;;;
;;;                                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    VARIABLES                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Grilles
(defparameter +grid+ (make-grid)) ; Grille à jouer
(defparameter +solved-grid+ nil) ; Grille résolue
(defparameter +loaded-grid+ nil) ; ???

;; Variables de test
(defparameter gtest #2A((1 0 0 0 0 4 0 0 5)
			(0 0 0 9 5 0 0 8 0)
			(0 0 0 0 0 3 0 9 0)
			(0 0 5 0 0 2 0 0 4)
			(0 0 1 0 6 0 7 0 0)
			(7 0 0 3 0 0 2 0 0)
			(0 6 0 5 0 0 0 0 0)
			(0 8 0 0 1 6 0 0 0)
			(5 0 0 2 0 0 0 0 7)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    FONCTIONS                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lecture de l'entrée standard du joueur.
;; Une fois d'entrée lue, on vérifie qu'elle est correcte,
;; sinon, on affiche une erreur. Si l'entrée est
;; correcte, on joue la valeur donnée.
(defun ask-player ()
  (let (x (y nil) c l)
    (format t "Entrez une combinaison colonne-ligne sous la forme C L :~C" #\linefeed)
    
    (setq l (read-line))
    (if (= (length l) 3)
	(progn
	  (setq y (char l 0))
	  (setq x (char l 2))))

    (format t "Entrez une valeur à inscrire :~C" #\linefeed)
    (setq l (read-line))
    (if (= (length l) 1)
	(setq c (char l 0)))
	  
    (setq x (position x +subalpha+))
    (setq y (position y +subalpha+))
    
    (cond
      ((not y)
       (format t "La colonne que vous avez entrée n'est pas valide.~C" #\linefeed))
      ((not x)
       (format t "La ligne que vous avez entrée n'est pas valide.~C" #\linefeed))
      ((not (play +grid+ x y c))
       (format t "La valeur que vous avez entrée n'a pas pu être jouée (cette case est déjà remplie, ou la valeur n'est pas valide).~C" #\linefeed))
      (t
       t))))

;; Le joueur joue jusqu'à la fin.
;; Tant qu'une case n'a pas été jouée, on demande
;; au joueur de jouer.
(defun play-sudoku (g)
  ; On crée une copie de la grille, qu'on résoud avec le solveur.
  (let ((end-here nil))
    (setf +solved-grid+ (make-grid))
    (dotimes (i +size+)
      (dotimes (j +size+)
	(if (not (= 0 (aref g i j)))
	    (play +solved-grid+ i j (nth (1- (aref g i j)) +subalpha+)))))
    (solve-grid +solved-grid+)

    ; Boucle permettant au joueur de jouer sur sa grille.
    (loop until
	 (progn
	   (setq end-here t)
	   (draw-grid +grid+)
	   (ask-player)
	   (dotimes (i +size+)
	     (dotimes (j +size+)
	       (if (not (has-been-played +grid+ i j))
		   (progn
		     (setq end-here nil)
		     (setq i +size+)
		     (setq j +size+)))))
	   end-here))

    ; Fin de la partie : il faut comparer les deux grilles.
    (format t "#findugame")
    (dotimes (i +size+)
      (dotimes (j +size+)
	(if (not (eq (get-grid-value +grid+ i j) (get-grid-value +solved-grid+ i j)))
	    (progn
	      (format t "VOUS ETES NULS A CHIER. VOILA LA REPONSE.")
	      (draw-grid +solved-grid+)
	      (setq i +size+)
	      (setq j +size+)))))))

;; Résoud la grille étape par étape.
(defun step-by-step-sudoku ()
  (format t "Vous avez choisi de résoudre la grille automatiquement, étape par étape. Appuyez sur \"Entrée\" pour afficher la grille, puis chaque appui sur \"Entrée\" résoudra une case.~C" #\linefeed)
  (read-char)
  (draw-grid +grid+)	
  (format t "Appuyez sur \"Entrée\" pour résoudre une case.~C" #\linefeed)
  (read-char)
  (loop until (not (solve-one +grid+))
     do (progn
	  (draw-grid +grid+)	
	  (format t "Appuyez sur \"Entrée\" pour résoudre une case.~C" #\linefeed)
	  (read-char))))

;; Résoud la grille d'un trait.
(defun auto-solve-sudoku ()
  (format t "Vous avez choisi de résoudre la grille automatiquement. Appuyez sur \"Entrée\" pour afficher la grille, puis de nouveau sur \"Entrée\" pour la résoudre complètement.~C" #\linefeed)
  (read-char)
  (draw-grid +grid+)
  (read-char)
  (solve-grid +grid+))

;; Fonction principale du programme.
;; Transforme la grille en paramètre en grille jouable.
;; Aucune vérification pour l'instant.
(defun sudoku (&key (grid nil) (solve nil) (steps nil))
  (if grid
      (progn
	(setq +n+ (isqrt (array-dimension grid 0)))
	(setq +size+ (* +n+ +n+))
	(setq +subalpha+ (subseq +alphabet+ 0 +size+))
	
	(setq +grid+ (make-grid))
	(dotimes (i +size+)
	  (dotimes (j +size+)
	    (if (not (= 0 (aref grid i j)))
		(play +grid+ i j (nth (1- (aref grid i j)) +subalpha+)))))
	
	(cond
	  (steps
	   (step-by-step-sudoku))
	  (solve
	   (auto-solve-sudoku))
	  (t
	   (play-sudoku grid))))
      (format t "Ici, ce sera la notice."))
)
