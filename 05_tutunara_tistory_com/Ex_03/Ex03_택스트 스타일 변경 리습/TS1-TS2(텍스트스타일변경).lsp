(setq jinbox_te_ts (load_dialog "JST"))	; Loading Dialog-Box

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun li_find (a b) (setq data1 (cdr (assoc a b))))

(defun c:ts1(/ jkey xxx tw fw rew x fw3 fw4 fw5 jts1 zzz x x7 xft1 xft2 xft)
	(if (= jinbox_te_ts nil)
		(setq jinbox_te_ts (load_dialog "jst"))
	)

	(command "undo" "group")
	(setq sset (sel_text))
	(new_dialog "jin_text_ts" jinbox_te_ts)

	(setq x (entget (ssname sset 0)))
	(setq x7 (li_find 7 x))
	(setq xft1 (cdr (assoc 3 (tblsearch "STYLE" x7))))
	(setq xft2 (cdr (assoc 4 (tblsearch "STYLE" x7))))
	(if (or (= xft2 "") (= xft2 nil))
		(setq xft (strcat "[" xft1 "]"))
		(setq xft (strcat "[" xft1 "]" "," "[" xft2 "]"))
	)
	(set_tile "jts_lic" x7)
	(set_tile "jts_fontc" xft)

	(setq	xxx  3	jkey nil  )

	(setq tw ())	;	Style name..
	(setq rew 1)
	(while (setq x (tblnext "style" rew))
		(setq rew nil)
		(if (/= (cdr (assoc 2 x)) nil)
			(setq tw (cons (strcase (cdr (assoc 2 x))) tw))
		);if
	);while
	(setq tw (append tw '("Select Style...")))
	(start_list "jts_li")	;Specify the name of the list box 
	(mapcar 'add_list (reverse tw))	;Specify the AutoLISP list 
	(end_list)	;	Style name end

	(setq fw ())	;	Font name..
	(setq rew 1)
	(while (setq x (tblnext "style" rew))
		(setq rew nil)
		(if (/= (cdr (assoc 3 x)) nil)
			(progn
				(setq fw3 (strcase (cdr (assoc 3 x))))
				(if (= fw3 "")(setq fw3 "None"))
				(setq fw4 (strcase (cdr (assoc 4 x))))
				(if (= fw4 "")(setq fw4 "None"))
				(setq fw5 (strcase (strcat "[" fw3 "],[" fw4 "]") T))
				(setq fw (cons fw5 fw))
			)
			(setq fw (cons "없 음" fw))
		);if
	);while
	(setq fw (append fw '("Select Font...")))
	(start_list "jts_font")	;Specify the name of the list box 
	(mapcar 'add_list (reverse fw))	;Specify the AutoLISP list 
	(end_list)	;	Font name end

	(mode_tile "jts_li" 2)

	(while (> xxx 2)
		(action_tile "jts_li" "(setq jts1 $value)(jts_fnt $value)")
		(action_tile "cancel" "(done_dialog)(setq jkey 999)")
		(setq xxx (start_dialog))
	)
	(action_tile "accept" "(done_dialog)")
	(action_tile "cancel" "(done_dialog)(setq jkey 999)")

	(setq tw (reverse tw))
	(setq jts1 (nth (atoi jts1) tw))

	(if (= jkey 999)
		(progn
			(prompt "\n\t Funtion's Cancel.....")
		)
		(t-sty sset jts1)
	)
	(command "undo" "end")

)

(defun jts_fnt (a / fw2)
	(setq fw2 (nth (atoi a) (reverse fw)))
	(set_tile "jts_font" fw2)
)

(defun jts_mk_sty()
  (command "style" "ghs" "simplex,ghs" "" "" "" "" "" "")
  (command "Style" "UTM" "으뜸체" "" "" "" "" "" "")
  (command "Style" "UTM2" "가는으뜸체" "" "" "" "" "" "")
)

(defun sel-sty()
   (setq ts (ssget "X" (list (cons 7 (getstring "\n Enter Style Name ? :")))))
)

(defun jts_call ( value )
	(setq a (nth value tw))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq jinbox_te_ts2 (load_dialog "JST"))	; Loading Dialog-Box

(defun jts_fnt1 (a / fw2)
	(setq fw2 (nth (atoi a) (reverse fw)))
	(set_tile "jts2_tf1" fw2)
)

(defun jts_fnt2 (a / fw2)
	(setq fw2 (nth (atoi a) (reverse fw)))
	(set_tile "jts2_tf2" fw2)
)

(defun c:ts2(/ jkey xxx tw fw rew x fw3 fw4 fw5 jts1 zzz x x7 xft1 xft2 xft)
	(if (= jinbox_te_ts2 nil)
		(setq jinbox_te_ts2 (load_dialog "jst"))
	)

	(command "undo" "group")
	(new_dialog "jin_text_ts2" jinbox_te_ts2)

	(setq	xxx  3	jkey nil  )

	(setq tw ())	;	Style name..
	(setq rew 1)
	(while (setq x (tblnext "style" rew))
		(setq rew nil)
		(if (/= (cdr (assoc 2 x)) nil)
			(setq tw (cons (strcase (cdr (assoc 2 x))) tw))
		);if
	);while
	(setq tw (append tw '("Select Style...")))
	(start_list "jts2_ts1")	;Specify the name of the list box 
	(mapcar 'add_list (reverse tw))	;Specify the AutoLISP list 
	(end_list)	;	Style name end

	(start_list "jts2_ts2")	;Specify the name of the list box 
	(mapcar 'add_list (reverse tw))	;Specify the AutoLISP list 
	(end_list)	;	Style name end


	(setq fw ())	;	Font name..
	(setq rew 1)
	(while (setq x (tblnext "style" rew))
		(setq rew nil)
		(if (/= (cdr (assoc 3 x)) nil)
			(progn
				(setq fw3 (strcase (cdr (assoc 3 x))))
				(if (= fw3 "")(setq fw3 "None"))
				(setq fw4 (strcase (cdr (assoc 4 x))))
				(if (= fw4 "")(setq fw4 "None"))
				(setq fw5 (strcase (strcat "[" fw3 "],[" fw4 "]") T))
				(setq fw (cons fw5 fw))
			)
			(setq fw (cons "없 음" fw))
		);if
	);while
	(setq fw (append fw '("Select Font...")))
	(start_list "jts2_tf1")	;Specify the name of the list box 
	(mapcar 'add_list (reverse fw))	;Specify the AutoLISP list 
	(end_list)	;	Font name end

	(start_list "jts2_tf2")	;Specify the name of the list box 
	(mapcar 'add_list (reverse fw))	;Specify the AutoLISP list 
	(end_list)	;	Font name end

	(mode_tile "jts2_ts1" 2)

	(while (> xxx 2)
		(action_tile "jts2_ts1" "(setq jts1 $value)(jts_fnt1 $value)")
		(action_tile "jts2_ts2" "(setq jts2 $value)(jts_fnt2 $value)")
		(action_tile "cancel" "(done_dialog)(setq jkey 999)")
		(setq xxx (start_dialog))
	)
	(action_tile "accept" "(done_dialog)")
	(action_tile "cancel" "(done_dialog)(setq jkey 999)")

	(setq tw (reverse tw))
	(setq jts1 (nth (atoi jts1) tw))
	(setq jts2 (nth (atoi jts2) tw))

	(if (= jkey 999)
		(progn
			(prompt "\n\t Funtion's Cancel.....")
		)
		(t-sty2 jts1 jts2)
	)
	(command "undo" "end")

)

(defun t-sty2 (a b / c d)
	(setq c (ssget "X" (list (cons 0 "TEXT")(cons 7 a))))
	(if c
		(t-sty c b)
		(prompt "Selec was Nothing...")
	)
)

(defun T-STY( a1 a2 / ts l sz i l1 t1 t2 t0 h) ; Multiple Text Style Change
	(if a1 (setq ts a1)(setq ts(ssget)))
	(if (= ts nil)(sel-sty))
   (if ts (progn (setq L(SSLENGTH ts))
   (if a2 (setq sz a2)(setq sz (getstring"\n Text Style : ")))
	(chg_text ts sz 7)))
	(setq jkey nil)
)

(defun CHG_TEXT(t_a t_b t_c / flag72)
   (setq flag72 nil)(setq i 0)(setq l1(- l 1))
   (while (<= i l1)
      (setq t1(ssname t_a i))
      (setq t2(entget t1))
      (setq t0(cdr (assoc 0 t2)))
      (setq h t_b)
      (if (= t_c 41)
         (progn
            (setq flag72 (cdr (assoc 72 t2)))
            (if (or (= flag72 3)(= flag72 5))
               (setq t2 (subst (cons 72 0)(assoc 72 t2) t2))
            )
         )
      )
      (setq t2(subst (cons t_c h)(assoc t_c t2) t2))
      ;(if (or (= flag72 3)(= flag72 5))
      ;   (setq t2(subst (cons 72 flag72)(assoc 72 t2) t2))
      ;)
      (entmod t2)
      (setq i(+ i 1))
   )
)

(defun sel_text ()	(ssget ":L" (list (cons 0 "TEXT"))))
	