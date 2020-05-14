(defun c:rb(/ ent bname rebname elist)
	(setq ent (car (entsel "\n이름을 바꿀 블록을 선택하세요.")))
	(while
		(cond
			((= ent nil)(eval T))
			((/= "INSERT" (cdr (assoc 0 (entget ent))))(eval T))
		)
		(setq ent (car (entsel "\n블록이 아닙니다. 다시 선택하세요.")))
	)
	(setq bname (getstring T "\n선택한 블록의 새 이름을 입력하세요 : "));getstring 뒤에 T를 입력해야 Spacebar의 입력을 받을 수 있습니다.
	(while
		(tblsearch "block" bname)
		(progn
			;(alert (strcat "입력한 " bname "은 이미 사용 중입니다."));alert를 사용하면, 경고메세지가 팝업 창으로 나타납니다.
			(setq rebname (strcat "입력한 " bname "은 이미 사용 중입니다. 다시 입력하세요. : "))
			(setq bname (getstring T rebname))
		)
	)
	(setq elist (entget (cdr (assoc 330 (entget (tblobjname "block" (cdr (assoc 2 (entget ent)))))))))
	(entmod (subst (cons 2 bname) (assoc 2 elist) elist))
	(princ)
)

