
(defvar dates (list '( "May"  "15") '("May" "16") '("May" "19") '("June" "17") '("June" "18") '("July" "14") '("July" "16") '("August" "14") '("August" "15") '("August" "17")))


(defun month (date) (car date))

(defun day (date) (cdr date))

(defun tell (part &optional(possible_dates dates))
  (loop for (month day) in possible_dates
     when (or (string= month part) (string= day part))
     collect (list month day)))

(defun know (possible_dates)
  (= 1 (length possible_dates)))


(defun statements3to5 (date) (equal '("May" "15") date))

(defun cheryls_bday (&optional(possible_dates dates))
  (remove-if-not #'statements3to5 possible_dates))
