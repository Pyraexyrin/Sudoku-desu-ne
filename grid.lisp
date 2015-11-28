;;;
;;; GRID.LISP
;;;
;;; Ce fichier contient toutes les méthodes relatives
;;; à l'édition et la gestion d'une grille.
;;;

;; Variabes globales qui seront exportées plus haut

(defparameter +alphabet+ '(1 2 A 4))

(defparameter +n+ 2)

;; Variables locales pour les tests

(defparameter *grid* (make-grid +n+ +alphabet+))

;; Fonctions

(defun make-grid (n list)
  (let* ((size (* n n))
	 (grid (make-array (list size size) :initial-element nil)))
    (dotimes (i size)
      (dotimes (j size)
	(let ((copy (copy-list (cons 0 list))))
	  (setf (aref grid i j) copy))))
    grid))

(defun get-grid-value (grid x y)
  (car (aref grid x y)))

(defun get-grid-possibilities (grid x y)
  (cdr (aref grid x y)))

(defun remove-from-possibilities (grid x y c)
  (let (
	(case-car (car (aref grid x y)))
	(case-cdr (cdr (aref grid x y)))
	)
    (setf (aref grid x y) (cons case-car (remove c case-cdr)))))
