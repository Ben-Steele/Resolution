
(setf counter 0)
(setf counter2 0)

(defun compare (sentence clause)                 ;see if the complement of literal is in clause, return literal if it is (if literal is a negation (not "literal") it will not find the complement)
    (dolist (i clause)                             ;for each literal in clause
      (if (listp i)                                ;if the literal is a list (this means it is a negation because it has the form (not "literal"))
          (if (and (eql (car i) 'not) (not (eql (car sentence) 'not))) 
              (let ((newbindings (unifier sentence (cadr i))))
                (if (equal newbindings nil)  
                    nil
                    (return (cons i (cons sentence (cons newbindings '()))))                       ;return the unification bindings
                    )
                )
            nil
            )
        nil                                    ;do nothing
        )
    )
  )                                            ;the dolist returns nil unless the return literal line is executed


(defun factorRecurse (a clause)
  (if (equal a (car clause))
      nil
      (let ((unified (unifier a (car clause))))
        (if (equal unified nil)
            (if (equal (list-length clause) 1)
                nil
                (factorRecurse a (cdr clause))
                )
            (cons a (cons a (cons unified '())))
            )
        )
      )
  )

(defun factor (clause restclause)
  (let ((factored (factorRecurse (car restclause) clause)))
    (if (eq factored nil)
        (if (equal (list-length restclause) 1)   
            clause
            (factor clause (cdr restclause))
            )
        (newClause clause clause factored)
        )
    )
  )

(defun factorall (clauses)
  (let ((factored (factor (car clauses) (car clauses))))
    (if (equal factored nil)
        (if (equal (list-length clauses) 1) 
            nil
            (factorall (cdr clauses))
            )
        (if (equal (list-length clauses) 1) 
            (cons factored nil)
            (cons factored (factorall (cdr clauses)))
            )
        )
    )
  )

(defun newClause (c1 c2 complement)                         ;given two clauses and a shared complement pair, return the resolution of those clauses, or T if it evaluates to true
  (setf c1 (combine c2 c1))                                 ;concatenate the two clauses and store it in c1 without repeating literals
  (setf c1 (remove (car complement) c1 :test #'equal))                    ;remove one complement
  (setf c1 (remove (cadr complement) c1 :test #'equal))                   ;remove the other complement
  (setf c1 (subst-bindings (caddr complement) c1))          ;apply bindings to the clause
  (let ((flag 1))                                           ;a flag to determine if c1 evaluates to true (useless clause)
    (dolist (a c1)                                          ;both dolists compare each literal to every other literal
      (dolist (b c1)                                        
        (if (listp a)                                       ;if a is a list (not "literal")
            (if (eq (first a) 'not)
                (if (unifier (cadr a) b)                             ;if the "literal" part of a equals b
                    (setf flag 0)                               ;the clause evaluates to true and the flag is set to 0
                    nil                                         ;otherwise do nothing
                    )
                nil
                )
            nil                                             ;do nothing
            )
        )
      )
    (if (eq flag 0)                                         ;if the flag equals 0, change c1 to true
        (setf c1 T)
        nil
        )
    )
  c1                                                        ;return c1
)

(defun resolve (clause1 clause2)                                          ;take in two clauses return the resolution of those clauses if there is one that does not evaluate to true
  (let ((newclauses '())                                                  ;newclauses will store the resolved clause
        (newbindings '())) 
    (dolist (i clause1)                                                   ;for all literals in the first clause
      (let ((complement (compare i clause2)))                             ;store the complement found by compare in complement
        (if (equal complement nil)                                            ;if there was no complement
            nil                                                           ;do nothing
            (let ((tempclause (newClause clause1 clause2 complement)))    ;resolve the two clauses with their complement
              (if (equal tempclause T)                                    ;if the resolved clause is true 
                  nil                                                     ;don't add it to the list of new clauses
                  (if (equal tempclause nil)                              ;if the list is empty, the empty set was resolved
                      (setf newclauses T)                                 ;return true for the whole resolution
                      (setf newclauses tempclause)                        ;otherwise add the resolved clause to newclauses
                      )
                  )
              )
            )
        )
      )
    newclauses
    )
)

(defun secondClauseLoop (a copy)                                     ;second layer of the iteration
  ;(if (eq counter 481)
  ;    (print (first copy))
  ;    nil
  ;    )
  (let ((new (resolve a (first copy)))                               ;resolve two clauses of the KB
        (temp '()))                                                  ;create temp to add new clauses to
    (if (equal new T)                                                ;if the resolved clause is true
        '(T)                                                         ;the empty set was reached so the resolution is true
        (if (equal (list-length copy) 1)                           ;if this is the last clause to check with a (the last recursive step)
              (if (equal new nil)                                    ;if there are no new clauses to add
                  new                                                ;return an empty list
                  (cons new temp)                                    ;otherwise concatenate new to an empty list and return that
                  )
              (if (equal new nil)                                    ;if there are no new clauses to add
                  (secondClauseLoop a (rest copy))                   ;return the next iteration of the recursion
                  (cons new (secondClauseLoop a (rest copy)))        ;return the new clauses concatenated onto the next iteration of the recursion
                  )
              )
        )
    )
)

(defun combine (list1 list2)                          ;combine two lists without repeating any elements
  (dolist (r list1 list2)                             ;iterate over the first list, return the second list
    (let ((flag 0))
      (dolist (w list2)
        (if (equal (set-exclusive-or r w :test #'equal) nil) 
            (setf flag 1)                                                                 
            nil
            )
        )
      (if (eq flag 1)
          nil
          (setf list2 (cons r list2))                       ;add the current element to list2
          )
      )
    )
  )

(defun firstClauseLoop (real copy)                                  ;first layer of recursive iteration
  (setf counter (+ counter 1))
  (print counter)
  (print (list-length real))
  (let ((a (first copy))                                            ;pick out a clause of the KB
        (new '())                                                   ;this will hold all the new clauses that are resolved
        (new2 '()))                                                 ;a temporary list for formating
    (setf new (secondClauseLoop a real))                            ;send a to secondClauseLoop to compare it with all the other clauses
    (if (equal (find T new) T)                                      ;if one of the new clauses is T
        T                                                           ;the KB resolved successfully so return T
        (progn
          (setf new (combine new real))                             ;Otherwise combine the new clauses with the KB
          (if (equal (list-length copy) 1)                          ;if this is the last step of iteration
              new                                                   ;return new
              (progn  
                (setf new2 (firstClauseLoop real (rest copy)))      ;otherwise save the next step of iteration in new2
                (setf counter2 (+ counter2 1))
                (print counter2)
                (if (equal new2 T)                                  ;if new2 is true
                    new2                                            ;return true because the KB resolved successfully
                    (combine new new2)                              ;return the combination of the next iteration with this iteration of recursion
                    )
                )
              )
          )
        )
    )
  )

(defun recurseResolution (clauses)                                               ;takes in full KB including ~a
  (let ((resolved (firstClauseLoop clauses clauses)))                            ;save resolved KB into resolved
    (setf counter 0)
    (setf counter2 0)
    (print "================================")
    (dolist (i clauses)
      (print i)
      )
    (if (eq resolved T)                                                          ;if resolved is true, 
        T                                                                        ;the KB resolved successfully so return true
        (progn
          (setf resolved (combine (factorall resolved) resolved))
          (if (equal (set-exclusive-or resolved clauses :test #'equal) nil)        ;if clauses has not changed (equal to resolved)
            nil                                                                  ;KB did not resolve so return nil
            (recurseResolution resolved)                                         ;otherwise resolve again
            )
          )
        )
    )
  )

(defun resolution (KB s q)                           ;KB is the knowledge base, a is the current state and query (if "state", then "query")
  (setf KB (append s KB))                            ;add the state to the KB
   (if (equal 'not (first q))                         ;if a is a negative sentence
      (setf KB (cons (rest q) KB))                   ;concatenate the positive sentence onto the KB
      (setf KB (cons (cons (cons 'not q) '()) KB))   ;concatenate the negative sentence onto the KB
      )
  (recurseResolution KB)                            ;recursively resolve until the solution is found
)

"(let ((CNF '(((not (hound $x1)) (howl $x1)) 
             ((not (have $x2 $y2)) (not (cat $y2)) (not (have $x2 $z2)) (not (mouse $z2)))
             ((not (ls $x3)) (not (have $x3 $y3)) (not (howl $y3)))
             ((have john $a1)) 
             ((cat $a2) (hound $a2))
             )))           ;CNF form of an example KB
  (print (resolution CNF '(((ls john)) ((mouse $b))) '((have john $b))))       ;either of the form (not (a)) or ((a))
)"

;(print (newclause '((have $x john) (have dave john) (gave $x dave)) '((have dave $y) (not (have dave john)) (not (gave $x $y))) '(((have dave john)) (not (have dave john)) ((nil)))))
;(print (factorall '(((NOT (KILLS $X3 $Z3)) (NOT (ANIMAL $Z3)) (NOT (ANIMAL $X3))))))
;(print (factor '((NOT (KILLS $X3 $Z3)) (NOT (ANIMAL $Z3)) (NOT (ANIMAL $X3))) '((NOT (KILLS $X3 $Z3)) (NOT (ANIMAL $Z3)) (NOT (ANIMAL $X3)))))


(let ((CNF '(((animal (f $x)) (loves (g $x) $x)) 
            ((not (loves $x2 (f $x2))) (loves (g $x2) $x2)) 
            ((not (loves $y3 $x3)) (not (animal $z3)) (not (kills $x3 $z3)))
            ((not (animal $x4)) (loves jack $x4))
            ((kills jack tuna) (kills curiosity tuna))
            ((cat tuna))
            ((not (cat $x7)) (animal $x7))
            )
           )
      )
  (print (resolution CNF nil '(kills curiosity tuna)))
)

"(let (( CNF '(((not (animal $x)) (loves jack $x))
              ((not (loves $y2 $x2)) (not (animal $z2)) (not (kills $x2 $z2)))
              
              )
            )
      )
  (print (resolution CNF '(((animal cat)) ((kills john cat))) '(not (loves jack john))))
  )"