;got code example from http://aima.cs.berkeley.edu/lisp/logic/algorithms/unify.lisp
(setf fail nil)                  ;when unification fails it returns nil
(setf nobinding '((nil)))        ;when unifiaction succeeds with no bindings, this can be returne

(defun unify (x y &optional (bindings nobinding))                             ;form to find the unification of x and y.  bindings is optional and defaults to nobinding as defined above
  (cond ((eq bindings fail) fail)                                             ;if a further step of the recursion failed, return fail
        ((eql x y) bindings)                                                  ;if x and y are equal, unification is complete, return bindings
        ((variable1 x) (unify-var x y bindings))                               ;if x is a variable, unify y with that variable
        ((variable1 y) (unify-var y x bindings))                               ;if y is a variable, unify x with that variable
        ((and (consp x) (consp y))                                            ;if x and y are compound or lists
         (unify (rest x) (rest y) (unify (first x) (first y) bindings))       ;retrun the unification of the smaller pieces
         )
        (t fail)                                                                  ;otherwise unification failed
        )
  )

(defun unify-var (var x bindings)                               ;form for is x or y was a variable
  (cond ((get-binding var bindings)                             ;if there is already a bindings for var
         (unify (lookup var bindings) x bindings)               ;apply the binding and unify again
         )
        ((and (variable1 x) (get-binding x bindings))            ;if there is already a binding for x
         (unify var (lookup x bindings) bindings)               ;apply the binding and unify again
         )
        ((occurs-in var x bindings)                             ;occur check
         fail)
        (t (extend-bindings var x bindings))                    ;otherwise add the binding var -> x to bindings
        )
  )

(defun variable1 (x)                                        ;tests if x is a variable. all variable will start with a $ in their name.
  (and (symbolp x) (eql (char (symbol-name x) 0) #\$))     ;if the first character of the symbol name for x is $ then return true
) 

(defun get-binding (var bindings)            ;Find a (variable value) pair in a binding list
  (assoc var bindings)                       ;if the variable has a binding, return it
  )

(defun binding-var (binding)      ;Get the variable part of a single binding
  (car binding)
  )

(defun binding-val (binding)      ;Get the value part of a single binding
  (cdr binding)
  )

(defun make-binding (var val)     ;given var and (val)
  (cons var val)                  ;put var in the (val) list
  )

(defun lookup (var bindings)                   ;Get the value part of (var val) from a binding in the binding list
  (binding-val (get-binding var bindings))     
  )    

(defun extend-bindings (var val bindings)    ;Add a (var value) pair to the binding list
  (cons 
   (make-binding var val)                    ;the new binding to be added
   (if (eq bindings nobinding)           ;if the old binding list has no bindings
       nil                                   ;make the list nil so the new binding can be added correctly
       bindings                              ;if there are already bindings, let the new binding be added to the existing bindings
       )
   )
  )

(defun occurs-in (var x bindings)                                ;Does var occur anywhere inside x?
  (cond ((eq var x) t)                                           ;if var and x are equal, return true
        ((and (variable1 x) (get-binding x bindings))             ;if x is a variable and there is already a binding for it
         (occurs-in var (lookup x bindings) bindings)            ;apply the binding and do the occur check again
         )
        ((consp x) (or (occurs-in var (first x) bindings)        ;if x is a list, return true if the occur check is true for var and first x (checks first thing in list)
                       (occurs-in var (rest x) bindings))        ;or if the occur check is true for var and rest x (checks the rest of the list recursively)
         )       
        (t nil)                                                  ;if none of the obove was true, return nil
        )
  )

(defun subst-bindings (bindings x)                           ;Substitute the value of variables in bindings into x, taking recursively bound variables into account
  (cond ((eq bindings fail) fail)                            ;if the algorithm has already failed, return fail
        ((eq bindings nobinding) x)                          ;if there are no bindings yet, return x as it already is
        ((and (variable1 x) (get-binding x bindings))         ;if x is a variable and has a binding
         (subst-bindings bindings (lookup x bindings))       ;recurse and substitute bindings again after applying the binding that was found
         )
        ((atom x) x)                                         ;if x is an atom, it cannot be bound, so return x
        (t (reuse-cons (subst-bindings bindings (car x))     ;CONFUSED HERE
                       (subst-bindings bindings (cdr x))
                       x))
        )
  )

(defun unifier (x y)                 ;Return bindings that unify with both x and y
  (unify x y)
  )

(defun reuse-cons (x y x-y &key (test #'eql))    ;Not really sure what this form does
  (if (and (funcall test x (car x-y))
	   (funcall test y (cdr x-y))
           )
      x-y
      (cons x y)
      )
  )


