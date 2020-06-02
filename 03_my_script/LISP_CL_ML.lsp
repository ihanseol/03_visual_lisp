;CL : 레이어를 옆에 있는 다른 레이어로 변경하는 리습입니다.
;-------------------------------------------------------------------------------
;  MOVE ENTITIES TO LAYER OF SELECTED ENTITY : CL()
;-------------------------------------------------------------------------------
(defun C:CL ()
    (setq p (ssget))
    (setq newname (car (entsel "\nMoving layer entity:")))
    (setq nw (cdr (assoc 8 (entget newname))))
    (prompt (strcat "\n\nMoving layer name " "< " nw " >...."))
    (setq jj (getstring "\nDO YOU MOVE ? <Y> : "))
    (setq t (sslength p))
    (setq l 0 )
    (if (or					  ;condition
	 (equal jj "n")
	 (equal jj "N")
	    )
	  (princ "\nDon't move entity !!")        ;if do true
	  (progn			                 	  ;if do false
	     (while (>= t (1+ l))
		 (setq oldname (ssname p l))
		 (setq oldlist (entget oldname))
		 (setq old (assoc 8 oldlist))
		 (setq newlist (entget newname))
		 (setq new (assoc 8 newlist))
		 (setq oldlist (subst new old oldlist))
		 (entmod oldlist)
		 (setq l (+ l 1))
	      )
	    (prompt (strcat "\nMoved layer name  " "< " (cdr new) " >" ))
	  )
    ) ;if
  (prin1)
)




;ML : 레이어를 레이어테이블에 있는 다른 레이어로 변경하는 리습입니다.
;-------------------------------------------------------------------------------
;  MOVE ENTITIES TO LAYER OF SELECTED ENTITY : ML()..Move Layer 
;-------------------------------------------------------------------------------
(defun C:ML ()
    (setq p (ssget))(command "-view" "r" "ml")(terpri)
    (setq newname (car (entsel "\nMoving layer entity:")))
    (setq nw (cdr (assoc 8 (entget newname))))
    (prompt (strcat "\n\nMoving layer name " "< " nw " >...."))
    (setq jj (getstring "\nDO YOU MOVE ? <Y> : "))(command "zoom" "p")(terpri)
    (setq t (sslength p))
    (setq l 0 )
    (if (or					  ;condition
	 (equal jj "n")
	 (equal jj "N")
	    )
	  (princ "\nDon't move entity !!")        ;if do true
	  (progn			                 	  ;if do false
	     (while (>= t (1+ l))
		 (setq oldname (ssname p l))
		 (setq oldlist (entget oldname))
		 (setq old (assoc 8 oldlist))
		 (setq newlist (entget newname))
		 (setq new (assoc 8 newlist))
		 (setq oldlist (subst new old oldlist))
		 (entmod oldlist)
		 (setq l (+ l 1))
	      )
	    (prompt (strcat "\nMoved layer name  " "< " (cdr new) " >" ))
	  )
    ); if
  (prin1)
)
