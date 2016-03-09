(setf vcounter 0)
(setf ccounter 0)
(setf fcounter 0)
(setf num-cons 0)
(setf num-forms 0)
(setf prev-cons nil)
(setf prev-single-forms nil)
(setf prev-double-forms nil)
(setf prev-var nil)
(setf *random-state* (make-random-state t))

(defun gen-var ()
  (setf vcounter (+ vcounter 1))
  (let ((new (intern (concatenate 'string "$V" (Write-to-string vcounter)))))
    (setf prev-var (cons new prev-var))
    new
    )
  )

(defun gen-cons ()
  (setf ccounter (+ ccounter 1))
  (setf num-cons (- num-cons 1))
  (let ((new (intern (concatenate 'string "C" (Write-to-string ccounter)))))
    (setf prev-cons (cons new prev-cons))
    new
    )
  )

(defun gen-single-form ()
  (setf fcounter (+ fcounter 1))
  (setf num-forms (- num-forms 1))
  (let ((new (intern (concatenate 'string "F" (Write-to-string fcounter)))))
    (setf prev-single-forms (cons new prev-single-forms))
    new
    )
  )
(defun gen-double-form ()
  (setf fcounter (+ fcounter 1))
  (setf num-forms (- num-forms 1))
  (let ((new (intern (concatenate 'string "F" (Write-to-string fcounter)))))
    (setf prev-double-forms (cons new prev-double-forms))
    new
    )
  )

(defun recurse-gen-KB (num-clauses)
  (if (> num-clauses 0)
      (let ((new-clause t)
	    (cur-KB nil))
	(loop while (equal new-clause t) do
	      (setf new-clause (gen-clause))
	      (if (equal new-clause nil)
		  (setf cur-KB (recurse-gen-KB num-clauses))
		(let  ((rec-KB (recurse-gen-KB (- num-clauses 1))))
		  (if (TIresolution rec-KB nil new-clause)
		      (setf new-clause t)
		    (setf cur-KB (cons new-clause rec-KB))
		    )
		  )
		)
	      )
	cur-KB
	)
    nil
    )
  )

(defun gen-KB (num-clauses _num-cons _num-forms)
  (setf vcounter 0)
  (setf ccounter 0)
  (setf fcounter 0)
  (setf prev-cons nil)
  (setf prev-single-forms nil)
  (setf prev-double-forms nil)
  (setf prev-var nil)
  (setf num-cons _num-cons)
  (setf num-forms _num-forms)
  (recurse-gen-KB num-clauses)
  )

(defun gen-clause ()
  (let ((clause nil))
    (setf prev-var nil)
    (loop while (< (random 100) 50) do
	  (setf clause (cons (gen-literal) clause))
	  )
    clause
    )
  )

(defun gen-literal ()
  (if (< (random 10) 5)
      (create-single)
    (create-double)
    )
  )

(defun pick-single-form ()
  (if (equal (list-length prev-single-forms) 0)
      (gen-single-form)
    (nth (random (list-length prev-single-forms)) prev-single-forms)
    )
  )

(defun pick-double-form ()
  (if (equal (list-length prev-double-forms) 0)
      (gen-single-form)
    (nth (random (list-length prev-double-forms)) prev-double-forms)
    )
  )

(defun pick-cons ()
  (if (equal (list-length prev-cons) 0)
      (gen-cons)
    (nth (random (list-length prev-cons)) prev-cons)
    )
  )

(defun pick-var ()
  (if (equal (list-length prev-var) 0)
      (gen-var)
    (nth (random (list-length prev-var)) prev-var)
    )
  )

(defun choose-var ()
  (if (< (random 10) 5)
      (gen-var)
    (pick-var)
    )
  )

(defun choose-cons ()
  (if (> num-cons 0)
      (if (< (random 10) 5)
	  (gen-cons)
	(pick-cons)
	)
    (pick-cons)
    )
  )

(defun create-single ()
  (let ((new-form nil)
	(new-cons nil)
	(rand nil))
    (if (> num-forms 0)
	(setf new-form (gen-single-form))
      (setf new-form (pick-single-form))
      )
    (setf rand (random 18))
    (setf new-form (cons new-form (cons (cond ((< rand 2) (create-single))
					      ((< rand 4) (create-double))
					      ((< rand 11) (choose-var))
					      (t (choose-cons))
					      )
					nil))
	  )
    (if (< (random 10) 5)
	(cons 'NOT (cons new-form nil))
      new-form
      )
    )
  )

(defun create-double ()
  (let ((new-form nil)
	(new-first nil)
	(new-second nil)
	(rand nil))
    (if (> num-forms 0)
	(setf new-form (gen-double-form))
      (setf new-form (pick-double-form))
      )
    (setf rand (random 18))
    (setf new-first (cond ((< rand 2) (create-single))
			       ((< rand 4) (create-double))
			       ((< rand 11) (choose-var))
			       (t (choose-cons))
			       ))
    (setf rand (random 18))
    (setf new-second (cond ((< rand 2) (create-single))
			  ((< rand 4) (create-double))
			  ((< rand 11) (choose-var))
			  (t (choose-cons))
			  ))
    (setf new-form (cons new-form (cons new-first (cons new-second nil))))
    (if (< (random 10) 5)
	(cons 'NOT (cons new-form nil))
      new-form
      )
    )
  )

(defun gen-query ()
  (let ((clause nil))
    (setf prev-var nil)
    (loop while (< (random 100) 50) do
	  (setf clause (cons (gen-literal) clause))
	  )
    clause
    )
  )


(defun gen-query-literal ()
  (if (< (random 10) 5)
      (create-query-single)
    (create-query-double)
    )
  )

(defun create-query-single ()
  (let ((new-form nil)
	(new-cons nil)
	(rand nil))
    (setf new-form (pick-single-form))
    (setf rand (random 18))
    (setf new-form (cons new-form (cons (cond ((< rand 2) (create-query-single))
					      ((< rand 4) (create-query-double))
					      ((< rand 11) (choose-var))
					      (t (pick-cons))
					      )
					nil))
	  )
    (if (< (random 10) 5)
	(cons 'NOT (cons new-form nil))
      new-form
      )
    )
  )

(defun create-query-double ()
  (let ((new-form nil)
	(new-first nil)
	(new-second nil)
	(rand nil))
    (setf new-form (pick-double-form))
    (setf rand (random 18))
    (setf new-first (cond ((< rand 2) (create-query-single))
			  ((< rand 4) (create-query-double))
			  ((< rand 11) (choose-var))
			  (t (pick-cons))
			  ))
    (setf rand (random 18))
    (setf new-second (cond ((< rand 2) (create-query-single))
			  ((< rand 4) (create-query-double))
			  ((< rand 11) (choose-var))
			  (t (pick-cons))
			  ))
    (setf new-form (cons new-form (cons new-first (cons new-second nil))))
    (if (< (random 10) 5)
	(cons 'NOT (cons new-form nil))
      new-form
      )
    )
  )

(defun test-Iter (KB query)
  (Iresolution KB nil query)
  )

(defun test-Thread-Iter (KB query)
  (TIresolution KB nil query)
  )

(defun test-Classic (KB query)
  (Cresolution KB nil query)
  )

(defun test-Thread-Classic (KB query)
  (TCresolution KB nil query)
  )


