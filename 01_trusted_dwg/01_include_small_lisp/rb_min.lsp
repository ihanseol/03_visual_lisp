(defun c:rb(/ ent bname rebname  elist)
	(setq ent( car (entsel "\n select block want to change ...")))
	(while 
		(cond
			((= ent nil) (eval T))
			((/= "INSERT" (cdr (assoc 0 (entget ent))))(eval T))
		)
		(setq ent (car (entsel "\n it's not block, try again ...")))
	)

	(setq bname (getstring T "\n enter new name , of seletec block ..."))
	(while
		(tblsearch "block" bname)
		(progn
			;(alert (strcat "input" bname "are already exist ..."))
			(setq rebname (strcat "input " bname " are already exist ..., try again ..."))
			(setq bname (getstring T rebname))
		)
	)
		;(setq elist (entget (cdr (assoc 330 (entget (tblobjname "block" (cdr (assoc 2 (entget ent)))))))))
		;(entmod (subst (cons 2 bame) (assoc 2 elist) elist))
		;(princ)
	(setq elist (entget (cdr (assoc 330 (entget (tblobjname "block" (cdr (assoc 2 (entget ent)))))))))
	(entmod (subst (cons 2 bname) (assoc 2 elist) elist))
	(princ)

)




