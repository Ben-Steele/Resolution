(defun compare (sentence clause)                  ;see if the complement of literal is in clause, return literal if it is (if literal is a negation (not "literal") it will not find the complement)
  (dolist (i clause)                             ;for each literal in clause
    (if (listp i)                                ;if the literal is a list (this means it is a negation because it has the form (not "literal"))
        (progn                                   
          (if (equal (car sentence) (cadr i)))   ;if i = (not literal)
              (return literal)                   ;return the unification
              )
          nil                                    ;do nothing
          )
        nil                                      ;do nothing
        )
    )                                            ;the dolist returns nil unless the return literal line is executed
  )

(defun newClause (c1 c2 complement)                         ;given two clauses and a shared complement pair, return the resolution of those clauses, or T if it evaluates to true
  (setf c1 (combine c2 c1))                                 ;concatenate the two clauses and store it in c1 without repeating literals
  (setf c1 (remove complement c1))                          ;remove the positive complement
  (let ((notcomplement '(not)))                             ;next 3 lines construct the negative complement so it can be removed
    (setf notcomplement (cons complement notcomplement))
    (setf notcomplement (reverse notcomplement))
    (setf c1 (remove notcomplement c1 :test #'equal))       ;remove the negative complement
    )
  (let ((flag 1))                                           ;a flag to determine if c1 evaluates to true (useless clause)
    (dolist (a c1)                                          ;both dolists compare each literal to every other literal
      (dolist (b c1)                                        
        (if (listp a)                                       ;if a is a list (not "literal")
            (if (eq (cadr a) b)                             ;if the "literal" part of a equals b
                (setf flag 0)                               ;the clause evaluates to true and the flag is set to 0
                nil                                         ;otherwise do nothing
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
  (let ((newclauses '()))                                                 ;newclauses will store the resolved clause
    (dolist (i clause1)                                                   ;for all literals in the first clause
      (let ((complement (compare i clause2)))                             ;store the complement found by compare in complement
        (if (listp complement)                                            ;if there was no complement
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
  (let ((new (resolve a (first copy)))                               ;resolve two clauses of the KB
        (temp '()))                                                  ;create temp to add new clauses to
    (if (equal new T)                                                ;if the resolved clause is true
        '(T)                                                         ;the empty set was reached so the resolution is true
        (progn
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
)

(defun combine (list1 list2)                          ;combine two lists without repeating any elements
  (dolist (r list1 list2)                             ;iterate over the first list, return the second list
    (setf list2 (remove r list2 :test #'equal))       ;remove the current element from list2 if it exists 
    (setf list2 (cons r list2))                       ;add the current element to list2
    )                                                 ;return list2
)

(defun firstClauseLoop (real copy)                                  ;first layer of recursive iteration
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
                (if (equal new2 T)                                  ;if new2 is true
                    new2                                            ;return true because the KB resolved successfully
                    (combine new2 new)                              ;return the combination of the next iteration with this iteration of recursion
                    )
                )
              )
          )
        )
    )
  )

(defun recurseResolution (clauses)                                               ;takes in full KB including ~a
  (let ((copy clauses)                                                           ;make a copy of the KB
        (resolved '()))                                                          ;stores the KB after resolutin
    (setf resolved (firstClauseLoop copy copy))                                  ;save resolved KB into resolved
    (if (eq resolved T)                                                          ;if resolved is true, 
        T                                                                        ;the KB resolved successfully so return true
        (if (equal (set-exclusive-or resolved clauses :test #'equal) nil)        ;if clauses has not changed (equal to resolved)
            nil                                                                  ;KB did not resolve so return nil
            (recurseResolution resolved)                                         ;otherwise resolve again
            )
    )
  )
)

(defun resolution (KB a)               ;KB is the knowledge base including current state, a is the query
  (if (equal 'not (first a))           ;if a is a negative literal
      (setf KB (cons (rest a) KB))     ;concatenate the positive literal onto the KB
      (let ((temp '()))                ;otherwise concatenate the negative literal onto the KB
        (setf temp (cons a temp))      ;next 3 lines create the negative literal and add it
        (setf temp (cons 'not temp))
        (setf KB (cons temp KB))
        )
  )
  (recurseResolution KB)               ;recursively resolve until the solution is found
)

(let ((CNF '(((not b11) p12 p21) ((not p12) b11) ((not p21) b11) ((not b11)))))   ;CNF form of an example KB
  (print (resolution CNF '(not p12)))
)