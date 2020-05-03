(defun c:revb (/ ds plw pt1 pt2 p1 p2 xdist ydist spcsx spcsy ent1 ent2 nxt info bulge data c_o c_l)
	(print ">> Cloud Draw Tool...")
              (setq c_o (getvar "osmode"))
	(setvar "cmdecho" 0)
	(setvar "osmode" 0)
              (setq c_l (getvar "clayer")) ;<= 현재 레이어값을 저장한다.
              (setq ly (tblsearch "layer" "Revision")) ;"레이어를 검색 없으면 레이어 생성
              (if (= ly nil) (command "layer" "n" "Revision" ""))
              (setvar "clayer" "Revision") ;<= 현재 레이어를 변경한다.
		(setq	ds (getvar "dimscale")
		plw (* 0.02 ds)                                            
		oer *error*
		bm (getvar "blipmode"))
	(setq   ds (* 30 ds))
	(print)                                            ; ------------> circle size 
	(setq str (strcat "호 지름<" (rtos ds 2) "> : "))
	(setq  buf (getint str)) 
	(if (/= buf NIL) (setq ds buf))
	(defun *error* (s)						;start error routine
		(setvar "blipmode" bm)					;reset blipmode
		(princ (strcat "\Exit..." s))				;type error message
		(if oer (setq *error* oer))
		(princ))
	(print)
	(SETQ PT1 (GETPOINT "영역의 좌측 하단점: "))	(terpri)
	(setq pt2 (getcorner pt1 "영역의 우측 상단점: "))
	(setvar "blipmode" 0)
	(setq	p1 (car pt1)	p2 (car pt2)				;find x distances
		xdist (- p2 p1))
	(setq 	p1 (cadr pt1)	p2 (cadr pt2)				;find y distances
		ydist (- p2 p1))

;******TO ADJUST SPACING OF ARCS CHANGE THE NUMBER 2 IN THE NEXT TWO LINES*****
	(setq	spcsx (/ (abs xdist) (/ ds 2))				;X spacing
		spcsy (/ (abs ydist) (/ ds 2)))				;Y spacing
			
	(if (= spcsx (fix spcsx))	(setq spcsx (fix spcsx))	(setq spcsx (+ 1 (fix spcsx))))
	(if (= spcsx 1)	(setq spcsx 2))					;min of 2 spaces
	(if (= spcsy (fix spcsy))	(setq spcsy (fix spcsy))	(setq spcsy (+ 1 (fix spcsy))))
	(if (= spcsy 1)	(setq spcsy 2))					;min of 2 spaces
	
	(setq	xdist (/ xdist spcsx)	ydist (/ ydist spcsy))		;set distances

	(setq p1 pt1)							;set polyline start point
	
	(command "PLINE" p1 "W" "0" "")					;start polyline command
	(repeat spcsx							;draw bottom line segments
		(setq p1 (polar p1 0.0 (abs xdist)))
		(command p1))
	(repeat spcsy							;draw right line segments
		(setq p1 (polar p1 (/ pi 2) (abs ydist)))
		(command p1))
	(repeat spcsx							;draw top line segments
		(setq p1 (polar p1 pi (abs xdist)))
		(command p1))
	(repeat (- spcsy 1)						;draw left line segments
		(setq p1 (polar p1 (* pi 1.5) (abs ydist)))             
		(command p1))
	(command "C")							;Close polyline

	(setq	ent1 (entlast)						;get entity
		ent2 (entget ent1)					;get entity info
;******TO ADJUST THE ARC SIZE ADJUST THE 0.5 BELOW*******			
		bulge (list (cons 42 0.5))				;build cloud arcs   0.5
		nxt (cdr (assoc -1 ent2))				;set for lookup
		nxt (entnext nxt)					;get next one
		plw (list (cons 41 plw)))				;build cloud width

	(if (= nxt nil)
		(progn
			(setq ent2 (subst (cons 42 0.5) (assoc 42 ent2) ent2))
			(entmod ent2)							;modify entity
		)
		(while nxt							;start loop
			(setq	info (entget nxt)				;get exist. info
				info (append info bulge)			;set bulge
				info (append info plw)				;set width
			)							;end of setq
			(entmod info)							;modify entity
			(setq nxt (entnext nxt))				;get next segment
		)								;end of while
	)
	(entupd ent1)							;update entity

	(setvar "blipmode" bm)						;reset blipmode
	(setvar "cmdecho" 1)						;turn command echo on
              (setvar "osmode" c_o)
              (setvar "clayer" c_l) ;<= 저장된 값으로 현재 레이어를 변경한다.
	(gc) (princ)							;print blank line
)										;End program