(defun C:FSS()(c:fs)(c:off)) ; 선택레이어를 현재레이어로 바꾸고 나머지 모두 off

(defun C:FS(/ la name) ; 현재레이어를 선택한개체의 레이어로 설정
   (prompt " = Set Layer (Pick)")
   ;(setq olderr  *error* *error* myerror chm 0)
   (setq la(car (entsel "\n\t Pick an object on the SETTING LAYER :")))
   (if la
      (progn
         (setq name (cdr (assoc 8 (entget la))))
         (command "layer" "set" name "")
         (command "layer" "Unlock" name "")
      )
      (progn
         (setq name (getstring "\n\t Enter the SET-LAYER to NAME ? : "))
         (command "layer" "set" name "")
         (command "layer" "Unlock" name "")
      )
   )
   (prin1 name)(prin1)
)
;;;-------------------------------------------------------------------------

(defun C:FF (/ la name) ; 선택한개체의 레이어를 Off함
  (prompt " = Off Layer (Pick)")
  ;(setq	olderr	*error*	*error*	myerror	chm	0)
  (setq la (car (entsel "\n\t Pick an object to be --OFF-- layer :")))
  (if la
    (progn
      (setq name (cdr (assoc 8 (entget la))))
      (if (= (getvar "clayer") name)
			(progn
				(alert	"*주의!!				
						현재 레이어가 Off 됩니다. 주의바람"	
				)
				(command "layer" "off" name "Y" "")
			)
			(command "layer" "off" name "")
		)
    )
    (progn
			(initdia 1)
			(command "layer")
    )
  )
  (prin1 name)
  (prin1)
)
;;;-------------------------------------------------------------------------

(defun C:FD (/ la name) ; 선택한개체의 레이어를 Lock함
  (prompt " = Lock Layer (Pick)")
  ;(setq	olderr	*error*	*error*	myerror	chm	0)
  (setq la (car (entsel "\n\t Pick an object to be --OFF-- layer :")))
  (if la
    (progn
      (setq name (cdr (assoc 8 (entget la))))
      (if (= (getvar "clayer") name)
			(progn
				(alert	"*주의!!				
						현재 레이어가 Lock 됩니다. 주의바람"	
				)
				(command "layer" "lock" name "")
			)
			(command "layer" "lock" name "")
		)
    )
    (progn
			(initdia 1)
			(command "layer")
    )
  )
  (prin1 name)
  (prin1)
)

;;;-------------------------------------------------------------------------
(defun C:FUU(/ la name) ; 선택한개체의 레이어를 Unlock
   (prompt " = Un-Lock Layer (Pick)")
   ;(setq olderr  *error* *error* myerror chm 0)
   (setq la (car (entsel "\n\t Pick an object to be --UNLOCK-- layer :")))
   (if la
      (progn
         (setq name (cdr (assoc 8 (entget la))))
         (command "layer" "unlock" name "")
      )
      (progn
         (setq name (getstring "\n\t Enter the Un-LOCK-LAYER's to NAME ? : "))
         (command "layer" "unlock" name "")
      )
   )
   (prin1 name)(prin1)
)
;;;-------------------------------------------------------------------------

(defun C:FDD(/ la) ; 현재레이어와 입력한레이어를 제외한 나머지 레이어를 Lock
   (prompt " = Lock-Layer (*)")
   (prompt "\n\t 락에서 제외할 레이어이름들 :")
   (setq la (getstring " 없으면 현재레이어만 락에서 제외함 : "))
   (if (or (= la nil)(= la ""))(setq la nil))
   (command "layer" "lock" "*" "u" (getvar "CLAYER") "")
   (if la (command "layer" "u" la ""))
)
;;;-------------------------------------------------------------------------

(defun C:FU() ; 전체레이어를 Unlock
   (prompt " = Un-Lock Layer (*)")
   (command "Layer" "U" "*" "")(prin1))
;;;-------------------------------------------------------------------------

(defun C:FT() ; 전체레이어를 Unfreeze
   (prompt " = Un-Freeze Layer (*)")
   (command "Layer" "T" "*" "")(prin1))

;;;-------------------------------------------------------------------------

(defun C:FO(/ lam) ; 입력한 레이어를 On
  (prompt " = On-Layer (Name)")
  (setq lam (getstring "\n\t Typing the -On- Layer Name ? : "))
  (if lam (command "layer" "on" lam ""))
  (prin1 lam)(prin1)
)

;;;-------------------------------------------------------------------------

(defun C:oof (/ la name nl n i e2 t2 ed laname ) ; 선택한개체의 레이어만 빼고 나머지레이어 Off함
  (command "undo" "group")
  (setq        la (ssget)
        laname (getvar "clayer")
        i 0
  )
  (if la
    (progn
      (setq nl (sslength la))
      (setq n (- nl 1))
      (while (<= i n)
                        (setq ed (entget (setq e2 (ssname la i))))
                        (setq t2 (cdr (assoc 8 ed)))
                        (setq laname (strcat laname "," t2))
                        (setq i (1+ i))
      )
      (command "Layer" "off" "all" "on" laname "")
    )
  )
  (command "undo" "end")
)

;;;-------------------------------------------------------------------------

(defun C:ON() (prompt " = On-Layer (*)")(command "layer" "on" "*" ""))
(defun C:OFF()(prompt " = Off-Layer (*)")(command "LAYER" "OFF" "*" "N" ""))

