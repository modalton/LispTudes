
(defvar dates (list '( "May"  "15") '("May" "16") '("May" "19") '("June" "17") '("June" "18") '("July" "14") '("July" "16") '("August" "14") '("August" "15") '("August" "17")))


(defun month (date) (car date))

(defun day (date) (cadr date))

(defun tell (part &optional(possible_dates dates))
  (loop for (month day) in possible_dates
     when (or (string= month part) (string= day part))
     collect (list month day)))

(defun know (possible_dates)
  (= 1 (length possible_dates)))



(defun statement3 (date)
"Albert: I don't know when Cheryl's birthday is, but I know that Bernard does not know too."
  (let ((possible_dates (tell (month date))))
    (and (not (know possible_dates))
         (= (length possible_dates) (length (remove-if #'(lambda (x)(<= (length(tell (cadr x))) 1)) possible_dates )))  )))

(defun statement4 (date)
  "Bernard: At first I don't know when Cheryl's birthday is, but I know now."
  (let ((at_first (tell (day date))))
    (and (not (know at_first))
         (know (remove-if-not #'statement3 at_first)))
    ))

(defun statement5 (date)
  "Albert: Then I also know when Cheryl's birthday is."
  (know (remove-if-not #'statement4 (tell (month date)))))

(defun statements3to5 (date) (and (statement3 date) (statement4 date) (statement5 date)))

(defun cheryls_bday (&optional(possible_dates dates))
  (remove-if-not #'statements3to5 possible_dates))
