


;; Very brittle helper function. bottom <= top 
(defun ints (bottom top)
  (loop for x from bottom to top collect x))

;; Very brittle helper function. Lists must be same length
(defun list_less_than (list_one list_two)
  (cond ((eq nil list_one) t)
        ((> (car list_one) (car list_two)) nil)
        ((<= 1 (length list_one)) (list_less_than (cdr  list_one) (cdr list_two)))
        ))


(defun sums (die_one die_two)
  "All possible sums of a side from one die plus a side from the other."
  (sort (loop for x in die_one
           append (loop for y in die_two
                     collect (+ x y)))
        #'<))



(defun pairs (collection)
  (incf-cl:lc (list a b)
              (incf-cl:<- a collection)
              (incf-cl:<- b collection)
              (list_less_than a b)
              ))

;; Using incf list comprehension library
(defun all_dice ()
  (incf-cl:lc (list 1 b c d e f)
              (incf-cl:<- b (incf-cl:range 2 8))
              (incf-cl:<- c (incf-cl:range b 8))
              (incf-cl:<- d (incf-cl:range c 8))
              (incf-cl:<- e (incf-cl:range d 8))
              (incf-cl:<- f (incf-cl:range (+ e 1) 9))
              ))

(defvar regular_die (ints 1 6))
(defvar regular_pair (list regular_die regular_die))
(setq regular_sums (sums regular_die regular_die))

(defun sicherman ()
  (incf-cl:lc pair
              (incf-cl:<- pair (pairs (all_dice)))
              (and (equal (sums (car pair) (cadr pair)) regular_sums)
                   (not (equal regular_pair pair)))
              ))
