(defun C:FSS()(c:fs)(c:off)) ; ���÷��̾ ���緹�̾�� �ٲٰ� ������ ��� off

(defun C:FS(/ la name) ; ���緹�̾ �����Ѱ�ü�� ���̾�� ����
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

(defun C:FF (/ la name) ; �����Ѱ�ü�� ���̾ Off��
  (prompt " = Off Layer (Pick)")
  ;(setq	olderr	*error*	*error*	myerror	chm	0)
  (setq la (car (entsel "\n\t Pick an object to be --OFF-- layer :")))
  (if la
    (progn
      (setq name (cdr (assoc 8 (entget la))))
      (if (= (getvar "clayer") name)
			(progn
				(alert	"*����!!				
						���� ���̾ Off �˴ϴ�. ���ǹٶ�"	
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

(defun C:FD (/ la name) ; �����Ѱ�ü�� ���̾ Lock��
  (prompt " = Lock Layer (Pick)")
  ;(setq	olderr	*error*	*error*	myerror	chm	0)
  (setq la (car (entsel "\n\t Pick an object to be --OFF-- layer :")))
  (if la
    (progn
      (setq name (cdr (assoc 8 (entget la))))
      (if (= (getvar "clayer") name)
			(progn
				(alert	"*����!!				
						���� ���̾ Lock �˴ϴ�. ���ǹٶ�"	
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
(defun C:FUU(/ la name) ; �����Ѱ�ü�� ���̾ Unlock
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

(defun C:FDD(/ la) ; ���緹�̾�� �Է��ѷ��̾ ������ ������ ���̾ Lock
   (prompt " = Lock-Layer (*)")
   (prompt "\n\t ������ ������ ���̾��̸��� :")
   (setq la (getstring " ������ ���緹�̾ ������ ������ : "))
   (if (or (= la nil)(= la ""))(setq la nil))
   (command "layer" "lock" "*" "u" (getvar "CLAYER") "")
   (if la (command "layer" "u" la ""))
)
;;;-------------------------------------------------------------------------

(defun C:FU() ; ��ü���̾ Unlock
   (prompt " = Un-Lock Layer (*)")
   (command "Layer" "U" "*" "")(prin1))
;;;-------------------------------------------------------------------------

(defun C:FT() ; ��ü���̾ Unfreeze
   (prompt " = Un-Freeze Layer (*)")
   (command "Layer" "T" "*" "")(prin1))

;;;-------------------------------------------------------------------------

(defun C:FO(/ lam) ; �Է��� ���̾ On
  (prompt " = On-Layer (Name)")
  (setq lam (getstring "\n\t Typing the -On- Layer Name ? : "))
  (if lam (command "layer" "on" lam ""))
  (prin1 lam)(prin1)
)

;;;-------------------------------------------------------------------------

(defun C:oof (/ la name nl n i e2 t2 ed laname ) ; �����Ѱ�ü�� ���̾ ���� ���������̾� Off��
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

