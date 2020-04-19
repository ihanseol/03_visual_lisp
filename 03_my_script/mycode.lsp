; https://youtu.be/SkHnD9xQTPE


;--------------------------------------------------
; reload, reload, reolad;
;--------------------------------------------------

(defun c:RRR() (load "mycode.lsp") (alert "Loaded!"))

;--------------------------------------------------


;--------------------------------------------------
Error handler
;--------------------------------------------------
(defun xx:Error(st)

    ;Note in addition to errors, also triggered any time user press Esc
    ; only way to distinguish, is the passed error string st
    ; on English installs: (exit) = "Function cancelled", Esc = "quit/exit"

    (if (not (member st (list "Function cancelled" "quit / exit abort")))
         ; so if it is a genuine error, code backtrace:
         (vl-bt)

    ) ;if

    (princ)
) ;Error

;--------------------------------------------------




;--------------------------------------------------
; dxf helper function
;--------------------------------------------------
; https://www.autodesk.com/techplus/autocad/acad2000/dxf/index.htm
;--------------------------------------------------

(defun dxf (i l) (cdr (assoc i l))  )

;--------------------------------------------------


;--------------------------------------------------
; test
;--------------------------------------------------

; (defun c:test(/ *error*)

; 	(setq *error* xx:Error)

; 	(alert "Hello, World! ")

; 	(if (getstring "enter string")
; 		(NewFunction)

; 		(exit)

; 	); if

; )

(defun c:test(/ *error* esel ent layer olay color newcolor)
			; esel - 
			; ent - 
			;layer -
			;olay-
			;color-
			;color-
			;newcolor-

	(setq *error* xx:Error)

	;select an object


	(if ;user selects object, get  color, get color
		(and
			(setq esel (entsel))
			(setq ent (entget (car esel)))

			;get its layer
			(setq layer (dxf 8 ent)) ; 
			;get the layers color

			(setq olay (entget (tblobjname "LAYER" layer)))
			(setq color (dxf 62 olay)) ; 

			;get a color
			(setq newcolor (acad_colordlg color))

			;update that layer
			(setq olay (subst (cons 62 newcolor) (assoc 62 olay) olay))
		);and

		;all things are true, go ahead and modify
		(progn
			(entmod olay)
			(command "REGEN")
		); progn - arbitray group
		;otherwise, not much to, jest exit
	) ;if

(princ)
) ;test
;--------------------------------------------------



(defun c:soojeon (ins str)
    (setq ins (getpoint))
    (entmakex
        (list
            '(000 . "TEXT")
            (cons 8  "수정넘버")
            (cons 6  "Continuous")
            (cons 010 ins)
            (cons 040 0.75)
            (cons 001 str)
        )
    )
)




