;; Emily Vogel
;; Assignment 9
;; November 24th, 2019
;; COSC 455 - 101

;; #1 - Finds the dot product of two equal lists
(defun dot-product (a b)
  (cond
   ((and (null a) (null b)) 0)
   (t (+ (* (car a) (car b)) (dot-product (cdr a) (cdr b))))
   )
  )

(dot-product '(1 2 3) '(3 2 4))


;; #2 - Checks if a list 'a' is a subset in list 'b'
(defun in (a b)
  (cond
   ((null b) b)
   ((eq a (car b)) t)
   (t (in a (cdr b)))
   )
  )

(defun subset (a b)
  (cond
   ((null a) t)
   (t (and (in (car a) b) (subset (cdr a) b)))
   )
  )

(subset '() '(3 1 1))


;; #3 - Inserts a new element to the left of the old atom in a list
(defun insertL (new old l)
  (cond
   ((null l) l)
   ((eq old (car l)) (cons new l))
   (t (cons (car l) (insertL new old (cdr l))))
   )
  )

(insertL 'a 'b '(c b d))


;; #4 - Replaces all occurances of a old atom with a new one
(defun substall (new old l)
  (cond
   ((null l) l)
   ((eq old (car l)) (cons new (substall new old (cdr l))))
   (t (cons (car l) (substall new old (cdr l))))
   )
  )

(substall 'a 'b '(a b b b a))


;; #5 - Replaces the second occurance of a given atom in a list
(defun rember (a l)
  (cond
   ((null l) l)
   ((eq a (car l)) (cdr l))
   (t (cons (car l) (rember a (cdr l))))
   )
  )

(defun rember2 (a l)
  (cond
   ((null l) l)
   ((eq a (car l)) (cons (car l) (rember a (cdr l))))
   (t (cons (car l) (rember2 a (cdr l))))
   )
  )

(rember2 'fruit '(ice cream with fruit for desert fruit oranges fruit bananas fruit))


;; #6 - Finds the total occurance of a list of atoms in another list
(defun occur (a l)
  (cond
   ((null l) 0)
   ((eq a (car l)) (+ 1 (occur a (cdr l))))
   (t (+ 0 (occur a (cdr l))))
   )
  )

(defun occurN (l1 l2)
  (cond
   ((null l1) 0)
   (t (+ (occur (car l1) l2) (occurN (cdr l1) l2)))
   )
  )

(occurN '(a b c) '())


;; #7 - Pairs a atom to another atom, like a map
(defun pair (l1 l2)
  (cond
   ((or (null l1) (null l2)) ())
   (t (cons (list (car l1) (car l2)) (pair (cdr l1) (cdr l2))))
   )
  )

(pair '(a b c) '(1 2 3))


;; #8 - Finds the association of a given pair of atoms, like a map
(defun assoc (x l)
  (cond
   ((null l) l)
   ((eq x (caar l)) (cdar l))
   (t (assoc x (cdr l)))
   )
  )

(assoc 'c '((a 1) (b 2) (c 3)))
