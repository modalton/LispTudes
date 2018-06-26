
(declaim (optimize (debug 3) (speed 0) (space 0)))

;;(declaim (optimize (safety 1)(speed 3)))

(defun rotate-str (str places)
  (concatenate 'string
               (subseq str places)
               (subseq str 0 places)))

(defun powerset (s)
  (if s
      (mapcan (lambda (x)
                (list (cons (car s) x)
                      x))
              (powerset (cdr s)))
    '(())))


(defun combinations (list)
  (if (null list)
      '(nil)
    (let* ((a (car list))
           (d (cdr list))
           (s (combinations d))
           (v (mapcar (lambda (x)
                        (cons a x))
                      s)))
      (append s v))))



(defun all-coin-permutations (n)
  (if (= n 1)
      '("H" "T")
    (reduce (lambda (acc str)
              (append acc
                      (list (concatenate 'string "T" str)
                            (concatenate 'string "H" str))))
            (all-coin-permutations (- n 1))
            :initial-value '())))



(defun generate-rotations (lst)
  (loop for
        i
        from
        0
        to
        (- (length lst)
           1)
        collect
        (rotate-str lst i)))



;;Flip in specified positions, unless we have solution (all heads)
(defun flip (str positions)
  (if (find #\T str)
      (mapcar #'(lambda (x)
                  (cond
                   ((char= #\T
                           (char str x))
                    (setf (char str x) #\H))
                   (t (setf (char str x) #\T))))
              positions))
  str)


;; For each possible state generate all rotations. Then remove the duplicates
;; and flip them with the move. We remove duplicates again bc you wouldn't flip
;; "HHHH" bc the game would already be over
(defun update (possible_states flips)
  (remove-duplicates (mapcar (lambda (coin_set)
                               (flip coin_set flips))
                             (remove-duplicates (reduce (lambda (acc coin_set)
                                                          (append acc
                                                                  (generate-rotations coin_set)))
                                                        possible_states
                                                        :initial-value '())
                                                :test #'string=))
                     :test #'string=))


(defun test (solution)
  (reduce (lambda (prev move)
            (update prev move))
          solution
          :initial-value (all-coin-permutations 4)))



(defmacro push_end (lst elem)
  `(if (< 0 (length ,lst))
      (push ,elem (cdr (last ,lst)))
     (push ,elem ,lst)))


(defun strategy_search (start goal actions result)
  (let ((explored (list))
        (queue (list (list '()start))))
    (loop while queue do
         (destructuring-bind (strategy cur_state)(pop queue)
           (if (equal cur_state goal) (return strategy))
           (mapcar (lambda (action)
                     (let ((next_states (sort (funcall result cur_state action) #'string-lessp)))
                       (if (not (member next_states explored :test #'equal))
                           (progn
                             (let ((next_q_elem (list (append strategy (list action)) next_states)))
                               (push_end queue next_q_elem)
                               )
                             (setq explored (adjoin next_states explored :test #'equal))))))
                   actions)))))
