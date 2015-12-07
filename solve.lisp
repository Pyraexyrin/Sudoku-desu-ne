;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                    ;;;
;;;                     SOLVE.LISP                     ;;;
;;;                                                    ;;;
;;; Ce fichier contient toutes les fonctions relatives ;;;
;;;        au processus de solving d'une grille.       ;;;
;;;                                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                    ;;;
;;; Variables :                                        ;;;
;;;                                                    ;;;
;;; +verbose+                                          ;;;
;;;                                                    ;;;
;;; Fonctions :                                        ;;;
;;;                                                    ;;;
;;; (solve-only-possibility grid)                      ;;;
;;; (solve-line grid x c)                              ;;;
;;; (solve-column grid y c)                            ;;;
;;; (solve-region grid x y c)                          ;;;
;;; (solve-one grid)                                   ;;;
;;; (solve-grid grid)                                  ;;;
;;;                                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    VARIABLES                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Afficher ou non les cases jouées pendant le solving.
(defparameter +verbose+ t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    FONCTIONS                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Joue une valeur dans une case si une seule possibilité
;; demeure. Renvoie NIL si aucune case n'a été jouée.
(defun solve-only-possibility (grid)
  (let ((end-here nil))
    (dotimes (i +size+)
      (dotimes (j +size+)
	(if (is-playable grid i j)
	    (progn
	      (if +verbose+ (format t "Case résolue : [~D, ~D] = ~D~C" (nth j +subalpha+) (nth i +subalpha+) (first (get-grid-possibilities grid i j)) #\linefeed))
	      (play grid i j (first (get-grid-possibilities grid i j)))
	      (setq end-here T)
	      (setq i +size+)
	      (setq j +size+)))))
    end-here))

;; Vérifie si le caractère c peut être jouée dans la ligne
;; x. S'il peut l'être, il l'est.
;; Renvoie T si c a été joué.
(defun solve-line (grid x c)
  (let ((cpt 0))
    (dotimes (i +size+)
      (if (position c (get-grid-possibilities grid x i))
	  (setq cpt (1+ cpt))))
    (if (= 1 cpt)
	(dotimes (i +size+)
	  (if (position c (get-grid-possibilities grid x i))
	      (progn
		(if +verbose+ (format t "Case résolue : [~D, ~D] = ~D~C" (nth i +subalpha+) (nth x +subalpha+) c #\linefeed))
		(play grid x i c)
		(setq i +size+)))))
    (= 1 cpt)))

;; Vérifie si le caractère c peut être jouée dans la colonne
;; y. S'il peut l'être, il l'est.
;; Renvoie T si c a été joué.
(defun solve-column (grid y c)
  (let ((cpt 0))
    (dotimes (i +size+)
      (if (position c (get-grid-possibilities grid i y))
	  (setq cpt (1+ cpt))))
    (if (= 1 cpt)
	(dotimes (i +size+)
	  (if (position c (get-grid-possibilities grid i y))
	      (progn
		(if +verbose+ (format t "Case résolue : [~D, ~D] = ~D~C" (nth y +subalpha+) (nth i +subalpha+) c #\linefeed))
		(play grid i y c)
		(setq i +size+)))))
    (= 1 cpt)))

;; Vérifie si le caractère c peut être jouée dans la région
;; débutant en (x, y). S'il peut l'être, il l'est.
;; Renvoie T si c a été joué.
(defun solve-region (grid x y c)
  (let ((cpt 0))
    (dotimes (i +n+)
      (dotimes (j +n+)
	(if (position c (get-grid-possibilities grid (+ x i) (+ y j)))
	    (setq cpt (1+ cpt)))))
    (if (= 1 cpt)
	(dotimes (i +n+)
	  (dotimes (j +n+)
	    (if (position c (get-grid-possibilities grid (+ x i) (+ y j)))
		(progn
		  (if +verbose+ (format t "Case résolue : [~D, ~D] = ~D~C" (nth (+ y j) +subalpha+) (nth (+ x i) +subalpha+) c #\linefeed))
		  (play grid (+ x i) (+ y j) c)
		  (setq i +n+)
		  (setq j +n+))))))
    (= 1 cpt)))

;; Résoud une case en utilisant les fonctions
;; ci-dessus. S'arrête dès que l'une d'entre-elles
;; a joué une valeur.
(defun solve-one (grid)
  (let ((solved (solve-only-possibility grid)))
    (if (not solved)
	(loop for c in +subalpha+ until solved
	   do (progn
		(dotimes (i +size+)
		  (if (or (solve-line grid i c)
			  (solve-column grid i c))
		      (progn
			(setq solved t)
			(setq i +size+))))
		(if (not solved)
		    (dotimes (i +n+)
		      (dotimes (j +n+)
			(if (solve-region grid (* i +n+) (* j +n+) c)
			    (progn
			      (setq solved t)
			      (setq i +n+)
			      (setq j +n+)))))))))
    solved))

;; Résoud la grille entière.
;; C'est agréable, une fonction moins compliquée.
(defun solve-grid (grid)
  (setq +verbose+ nil)
  (loop until (not (solve-one grid)))
  (setq +verbose+ t))
