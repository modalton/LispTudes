
(defun rotate-r (n lst)
  (append (nthcdr n lst) (butlast lst (- (length lst) n))))

(defun powerset (s) 
  (if s (mapcan (lambda (x) (list (cons (car s) x) x)) 
                (powerset (cdr s)))
      '(())))


(defun combinations (list)
  (if (null list) '(nil)
      (let* ((a (car list))
             (d (cdr list))
             (s (combos d))
             (v (mapcar (lambda (x) (cons a x)) s)))
        (append s v))))


(defun permutations (bag)
  "Return a list of all the permutations of the input."
  ;; If the input is nil, there is only one permutation:
  ;; nil itself
  (if (null bag)
      '(())
      ;; Otherwise, take an element, e, out of the bag.
      ;; Generate all permutations of the remaining elements,
      ;; And add e to the front of each of these.
      ;; Do this for all possible e to generate all permutations.
      (mapcan #'(lambda (e)
                  (mapcar #'(lambda (p) (cons e p))
                          (permutations
                            (remove e bag :count 1 :test #'eq))))
              bag))) 

(defun generate-rotations (lst)
  (loop for i from 0 to (- (length lst) 1)
     collect (rotate-r i lst)))




(defun flip (str positions)
  (if (string= str "HHHH")
      (print "hereio")
      (mapcar #'(lambda (x)
                  (cond
                    ((char= #\T (char str x)) (setf (char str x) #\H))
                    (t (setf (char str x) #\T)))
                  ) positions))
  str)

(defun altering-flip (str positions)
 (mapcar #'(lambda (x)(setf (char str x) #\H)) positions)
str)
  
  
