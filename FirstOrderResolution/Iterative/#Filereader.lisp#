(in-package "COMMON-LISP-USER")

(defun readfile (filePath)
  (let ((full (read-line (open filePath)))
        (split nil))
    (setf split (split-by-one-space full))
    (print split)       
    )
  )


(defun split-by-one-space (string)
    "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

(setf mylist (readfile "/Users/BS/Programs/Thesis/FirstOrderResolution/Iterative/Testlist.txt"))

(defun convertlist (l)
  (if (eq l nil)
      nil
      (car (if (equal (car l) "(") 
               (let ((temp (convertlist2 (cdr l))))
                 (cons (car temp) (convertlist (cadr temp)))
                 )
               (cons (intern (car l)) (convertlist2 (cdr l)))
               )
           )
      )
  )

(defun convertlist2 (l)
  (if (equal (car l) "(")
      (let ((temp (convertlist2 (cdr l)))
            (temp2 nil))
        (setf temp2 (convertlist2 (cadr temp)))
        (cons (cons (car temp) (car temp2)) (cons (cadr temp2) nil))
        )
      (if (equal (car l) ")")
          (cons nil (cons (cdr l) nil)) 
          (let ((temp (convertlist2 (cdr l))))
            (cons (cons (intern (car l)) (car temp)) (cons (cadr temp) nil))
            )
          )
      )
  )
