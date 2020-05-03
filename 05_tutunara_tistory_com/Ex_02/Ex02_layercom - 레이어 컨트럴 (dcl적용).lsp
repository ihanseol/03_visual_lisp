(defun c:dlm (/ LayerList laynum lay_ok)
	(setq LayerList '(	"선택 레이어로 전환 (LS)"
						"동적으로 표시 (LW)"
						" "
						"대상 레이어로 변경 (LC)"
						"현재 레이어로 변경 (LSC)"
						" "
						"레이어 OFF (LF)"
						"레이어 ON (LO)"
						"모든 레이어 ON (LOA)"
						" "
						"레이어 동결 (LZ)"
						"레이어 동결해제 (LT)"
						"모든 레이어 동결해제 (LTA)"
						" "
						"레이어 잠금 (LK)"
						"레이어 잠금해제 (LU)"
						"모든 레이어 잠금해제 (LUA)"
						" "
						"대상 레이어로 병합 (LM)"
						"칼라를 유지하고 병합 (LMC)"
						"레이어 삭제 (LD)"
						" "
						"맨 위로 올림 (UP)"
						"맨 아래로 내림 (DW)"
					)
	)

(vl-load-com)
(create_dialog_dlm)

	(setq laynum (load_dialog fname))
	(new_dialog "layercom" laynum)
	(if (= layer_cnt nil)(setq layer_cnt "0"))
	(start_list "lay_num")(mapcar 'add_list LayerList)(end_list)
	(set_tile "lay_num" layer_cnt)
	(action_tile "lay_num" "(setq layer_cnt $value)")
	(setq lay_ok (start_dialog))
	(done_dialog)
	(unload_dialog laynum)
	(if (= lay_ok 1)
		(progn
			(cond
				((= layer_cnt "0")(c:LS))
				((= layer_cnt "1")(c:LW))

				((= layer_cnt "3")(c:LC))
				((= layer_cnt "4")(c:LSC))

				((= layer_cnt "6")(c:LF))
				((= layer_cnt "7")(c:LO))
				((= layer_cnt "8")(c:LOA))

				((= layer_cnt "10")(c:LZ))
				((= layer_cnt "11")(c:LT))
				((= layer_cnt "12")(c:LTA))

				((= layer_cnt "14")(c:LK))
				((= layer_cnt "15")(c:LU))
				((= layer_cnt "16")(c:LUA))

				((= layer_cnt "18")(c:LM))
				((= layer_cnt "19")(c:LMC))
				((= layer_cnt "20")(c:LD))

				((= layer_cnt "22")(c:UP))
				((= layer_cnt "23")(c:DW))
				(T (princ "\n선택된 명령이 없습니다."))
			)
		)
		(princ " *취소* ")
	)
	(princ)
)

(defun c:LS	()(command "_Laymcur")(princ))
(defun c:LW	()(command "_.laywalk")(princ))

(defun c:LC	()(command "_laymch")(princ))
(defun c:LSC()(command "_laycur")(princ))

(defun c:LF	()(command "_layoff")(princ))
;(defun c:LO)
(defun c:LOA()(command "_layon")(princ))

(defun c:LZ	()(command "_layfrz")(princ))
;(defun c:LT)
(defun c:LTA()(command "_laythw")(princ))

(defun c:LK	()(command "_laylck")(princ))
(defun c:LU	()(command "_layulk")(princ))
;(defun c:LUA)

(defun c:LM	()(command "_laymrg")(princ))
;(defun c:LMC)
(defun c:LD	()(command "_laydel")(princ))

;(defun c:UP)
;(defun c:DW)

(defun c:LO	(/ doc LayerOffList layer laynum layeroff_cnt acc ReadList cnt)

	(command "undo" "be")
  (vl-load-com)
  (create_dialog_dlm)
	(setq doc (vla-get-activedocument (vlax-get-acad-object)))
	(setq LayerOffList nil)
	(vlax-for layer (vla-get-layers doc)
		(if (= (vla-get-layeron layer) :vlax-false)
			(setq LayerOffList (append LayerOffList (list (vla-get-name layer))))
		)
	)
	(setq LayerOffList
		(vl-sort LayerOffList
			(function (lambda (e1 e2)
							(< (strcase e1) (strcase e2))
						)
			)
		)
	)
	(setq laynum (load_dialog fname))
	(new_dialog "layeroff" laynum)
	(if (= layeroff_cnt nil)(setq layeroff_cnt "0"))
	(start_list "lay_off")(mapcar 'add_list LayerOffList)(end_list)
	(set_tile "lay_off" layeroff_cnt)
	(action_tile "cancel" "(setq acc 0)(done_dialog)")
	(action_tile "accept" "(setq acc 1)(setq ReadList (ch2lst (get_tile \"lay_off\") \" \"))(done_dialog)")
	(start_dialog)
	(unload_dialog laynum)
	(if (= acc 1)
		(progn
			(setq cnt 0)
			(repeat (length ReadList)
				(setq Lname (nth (atoi (nth cnt ReadList)) LayerOffList))
				(command "-layer" "on" Lname "")
				(princ (strcat "\n\"" Lname "\" 도면층을 켰습니다."))
				(setq cnt (1+ cnt))
			)
		)
		(progn
			(princ " *취소*")
		)
	)
	(command "undo" "e")
	(princ)
)
(defun c:LT	(/ doc LayerFrzList layer laynum layerFrz_cnt acc ReadList cnt)
	(command "undo" "be")
  (vl-load-com)
  (create_dialog_dlm)
	(setq doc (vla-get-activedocument (vlax-get-acad-object)))
	(setq LayerFrzList nil)
	(vlax-for layer (vla-get-layers doc)
		(if (= (vla-get-freeze layer) :vlax-true)
			(setq LayerFrzList (append LayerFrzList (list (vla-get-name layer))))
		)
	)
	(setq LayerFrzList
		(vl-sort LayerFrzList
			(function (lambda (e1 e2)
							(< (strcase e1) (strcase e2))
						)
			)
		)
	)
	(setq laynum (load_dialog fname))
	(new_dialog "layerfreez" laynum)
	(if (= layerFrz_cnt nil)(setq layerFrz_cnt "0"))
	(start_list "lay_off")(mapcar 'add_list LayerFrzList)(end_list)
	(set_tile "lay_off" layerFrz_cnt)
	(action_tile "cancel" "(setq acc 0)(done_dialog)")
	(action_tile "accept" "(setq acc 1)(setq ReadList (ch2lst (get_tile \"lay_off\") \" \"))(done_dialog)")
	(start_dialog)
	(unload_dialog laynum)
	(if (= acc 1)
		(progn
			(setq cnt 0)
			(repeat (length ReadList)
				(setq Lname (nth (atoi (nth cnt ReadList)) LayerFrzList))
				(command "-layer" "Thaw" Lname "")
				(princ (strcat "\n\"" Lname "\" 도면층을 동결해제 시켰습니다."))
				(setq cnt (1+ cnt))
			)
			(command "regen")
		)
		(progn
			(princ " *취소*")
		)
	)
	(command "undo" "e")
	(princ)
)
(defun c:LUA (/ doc LayerLockList layer cnt)
	(command "undo" "be")
  (vl-load-com)
  (create_dialog_dlm)
	(setq doc (vla-get-activedocument (vlax-get-acad-object)))
	(setq LayerLockList nil)
	(vlax-for layer (vla-get-layers doc)
		(if (= (vla-get-lock layer) :vlax-true)
			(setq LayerLockList (append LayerLockList (list (vla-get-name layer))))
		)
	)
	(if LayerLockList
		(progn
			(setq cnt 0)
			(repeat (length LayerLockList)
				(setq Lname (nth cnt LayerLockList))
				(command "-layer" "unlock" Lname "")
				(setq cnt (1+ cnt))
			)
			(princ "\n모든 도면층을 잠금해제 시켰습니다.")
		)
		(princ "\n잠긴 도면층이 없습니다.")
	)
	(command "undo" "e")
	(princ)
)
(defun c:LMC (/ ent1 Lname1 ent2 Lname2 ynan YN ssg doc layer layercolor layerltype cnt color ltype)
	(command "undo" "be")
	(setq os (getvar "clayer"))
	(setq ent1 (entsel "\n병합할 도면층의 객체 선택: "))
	(if ent1
		(progn
			(setq Lname1 (cdr (assoc 8 (entget (car ent1)))))
			(princ (strcat "\n선택한 도면층: " Lname1 ))
			(setq ent2 (entsel "\n대상 도면층의 객체 선택: "))
			(if ent2
				(progn
					(setq Lname2 (cdr (assoc 8 (entget (car ent2)))))
					(princ "\n******** 경고 ********")
					(princ (strcat "\n\"" Lname1 "\" 도면층을 \"" Lname2 "\" 도면층에 병합합니다."))
					(setq ynan "N")
					(initget 0 "Y N _Yes No")
					(setq YN (getkword "\n계속하시겠습니까? [예(Y)/아니오(N)] <아니오(N)>: "))
					(if (member YN '("Yes" "No"))(setq ynan YN))
					(if (= YN "Yes")
						(progn
							(setq ssg (ssget "X" (list (cons 8 Lname1))))
              (vl-load-com)
              (create_dialog_dlm)
							(setq doc (vla-get-activedocument (vlax-get-acad-object)))
							(vlax-for layer (vla-get-layers doc)
								(if (= (vla-get-name layer) Lname1)
										(setq layercolor (vla-get-color layer)
											  layerltype (vla-get-linetype layer))
								)
							)
							(setq cnt 0)
							(repeat (sslength ssg)
								(setq ent (entget (ssname ssg cnt)))
								(setq color (cdr (assoc 62 ent)))
								(setq ltype (cdr (assoc 6 ent)))
								(if (= color nil)
									(entmod (append ent (list (cons 62 layercolor))))
								)
								(if (= ltype nil)
									(entmod (append ent (list (cons 6 layerltype))))
								)
								(entmod (subst (cons 8 Lname2) (cons 8 Lname1) ent))
								(setq cnt (1+ cnt))
							)
							(if (= os Lname1)(setvar "clayer" "0"))
								(if (= Lname1 "0")
									(princ "\n\"0\"번 도면층은 삭제를 못합니다.")
									(progn
										(command "purge" "la" lname1 "y" "y")
										(princ (strcat "\n\"" Lname1 "\" 도면층을 삭제했습니다."))
									)
								)
						)
						(princ " *취소*")
					)
				)
				(princ "\n선택한 객체가 없어서 취소합니다.")
			)
		)
		(princ "\n선택한 객체가 없어서 취소합니다.")
	)
	(command "undo" "e")
	(prin1)
)
(defun c:UP (/ ent ssg)
	(command "undo" "be")
	(princ "\n레이어 위로 보내기")
	(setq ent (entsel "\n객체 선택: "))
	(if ent
		(progn
			(setq ent (cdr (assoc 8 (entget (car ent)))))
			(setq ssg (ssget "X" (list (cons 8 ent))))
			(command "draworder" ssg "" "F")
			(princ (strcat "\n\"" ent "\" 도면층을 맨 위로 보내기 하였습니다."))
		)
	)
	(command "undo" "e")
	(prin1)
)
(defun c:DW (/ ent ssg)
	(command "undo" "be")
	(princ "\n레이어 아래로 보내기")
	(setq ent (entsel "\n객체 선택: "))
	(if ent
		(progn
			(setq ent (cdr (assoc 8 (entget (car ent)))))
			(setq ssg (ssget "X" (list (cons 8 ent))))
			(command "draworder" ssg "" "B")
			(princ (strcat "\n\"" ent "\" 도면층을 맨 아래로 보내기 하였습니다."))
		)
	)
	(command "undo" "e")
	(prin1)
)
(defun ch2lst (txt1 txt2 / ps rt tt lst ll)
	(setq ps 1 rt "" lst '())
	(if (= 'str (type txt1))
		(progn
			(setq ll (strlen txt1))
			(repeat ll
				(setq tt (substr txt1 ps 1))
				(if (/= txt2 tt)
				(setq rt (strcat rt tt))
					(progn
						(if (/= "" rt) (setq lst (append lst (list rt))))
						(setq rt "")
					)
				)
				(setq ps (1+ ps))
			)
			(setq lst (append lst (list rt)))
		)
		(princ "\nBad Argument list!!!")
	)
)
(prin1)


;; ------------------------------------------------------ dcl start -----------------------------
(defun create_dialog_dlm ()
(setq fname (vl-filename-mktemp "layer_merge_1.dcl"))
(setq fn (open fname "w"))
(write-line "

layercom : dialog {label = \"레이어관리\" ;
spacer_1 ;
	:list_box {
				key = \"lay_num\";
				height = 25 ; width = 30 ;
//				fixed_width_font = true ;
				allow_accept = true ;
	}
spacer_1 ;
ok_cancel;
}
layeroff : dialog {label = \"LAYER OFF LIST\" ;
spacer_1 ;
	: column {
		: row {
			:list_box {
				key = \"lay_off\";
				height = 15 ; width = 15 ;
				multiple_select = true ;
				allow_accept = true ;
			}
		}
		: row {
			: button {
				label = \"선택(S)\" ;
				key = \"accept\" ;
				is_default = true ;
			}
			: button {
				label = \"취소(C)\" ;
				key = \"cancel\" ;
				is_default = false ;
				is_cancel = true ;
			}
		}
	}
}
layerfreez : dialog {label = \"LAYER FREEZ LIST\" ;
spacer_1 ;
	: column {
		: row {
			:list_box {
				key = \"lay_off\";
				height = 15 ; width = 15 ;
				multiple_select = true ;
				allow_accept = true ;
			}
		}
		: row {
			: button {
				label = \"선택(S)\" ;
				key = \"accept\" ;
				is_default = true ;
			}
			: button {
				label = \"취소(C)\" ;
				key = \"cancel\" ;
				is_default = false ;
				is_cancel = true ;
			}
		}
	}
}


" fn)

(close fn)

);defun

;; ------------------------------------------------------ dcl end -----------------------------
