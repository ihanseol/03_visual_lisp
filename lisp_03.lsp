


(defun draw_l_c (sp ep color)

	(command "line" sp ep "")
	(command "chprop" "l" "" "c" color "")
	(command "circle" "2p" sp ep)
	(command "chprop" "l" "" "c" (+ 1 color) "")
	(princ)

)


(defun  c:exo3()

	(setvar "cmdecho" 1)
	(setq p1 (getpoint "\n first point : "))
	(setq p2 (getpoint p1 "\n second point : "))
	(setq bc (getint "\n input base color : "))

	;(load "exo_common")

	(draw_l_c p1 p2 bc)

	(princ)

)