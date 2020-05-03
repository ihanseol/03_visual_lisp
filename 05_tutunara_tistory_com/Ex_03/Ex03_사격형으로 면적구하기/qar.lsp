;;====================================================================
;  사각형으로 면적구하기 (2006.11 주말농부)
;  ->사각형 면적을 대각의 두점을 선택하여 가로,세로,면적을
;    표기하는 명령어(자리수 지정)
;  ->복수선택/dcl적용(2007.7.4)
;  ->자리수 표기 오류수정(2007.8.13)
;------- quick area --------------------------------------------------
(defun c:qar(/ os ts cm bl a yn ht tem dot pt1 pt2 pt3 rlt txy dx dy
               dix diy dxy txy1 k eno en ed dz)
;->*error* start
 (defun *error* (msg)(princ "error: ")(princ msg)
 (setvar "osmode" os)(setvar "blipmode" bl)(setvar "dimzin" dz)
 (princ))
;-<*error* end
   (princ " 사각형으로 면적구하기...")
   (setq os (getvar "osmode") ts (getvar "textsize")
         cm (getvar "cmdecho") bl (getvar "blipmode")
         dz (getvar "dimzin") )
   (setvar "cmdecho" 0) (setvar "blipmode" 1)(setvar "dimzin" 0)
;-->설정값 변경
   (if (= dwqar nil) (setq dwqar 2))
   (setq a (strcat "\n현재 설정값(문자높이=" (rtos ts 2 1)
            ",자리수=" (rtos dwqar 2 0) ")을 변경할까요? [Y/N] <N> : "))
   (initget "Yes No")
   (setq yn (getkword a))
   (if (= yn "Yes")
     (progn
       (setq a (strcat "\n문자높이<" (rtos ts 2 1) ">:"))
       (setq ht (getdist a))
       (if (= ht nil) (setq ht ts))
       (setq a (strcat "\n소수점 자리수을 지정<" (rtos dwqar 2 0) ">:"))
       (setq tem (getint a))
       (if (= tem nil) (setq dot dwqar) (setq dot (abs tem)) )
       (setq dwqar dot)
     )
     (setq ht ts dot dwqar)
   )
;--<
   (setq pt1 (getpoint "\n사각형의 한점 선택-> "))
   (setq rlt "0" k 1)
(while pt1
   (setq pt2 (getcorner pt1 "\대각점 선택-> "))
   (if pt2 (progn
      (setq dx (abs (- (car pt2) (car pt1))))
      (setq dy (abs (- (cadr pt2) (cadr pt1))) ) (terpri)
      (setq dx (/ dx 1000) dy (/ dy 1000) )
      (setq dix (rtos dx 2 dot));->x값
      (setq diy (rtos dy 2 dot));->y값
      (setq dxy (* (atof dix) (atof diy)))
      (setq dixy (rtos dxy 2 dot));->x*y값
      (setq txy1 (strcat "( " dix " x " diy " )" ))
      (setq rlt (rtos (+ (atof rlt) (atof dixy)) 2 dot))
      (if (> k 1) (setq txy (strcat txy " + " txy1))
                  (setq txy txy1))
      (setq k (+ k 1))
      (princ "\n결과: ")(princ txy)(princ " = ")(princ rlt)  ))
   (setq pt1 (getpoint "\n사각형의 한점 선택<dcl보기>-> "))
)
;->dcl load
   (setq eno nil)
   (setq dcl_id (load_dialog "c:/ezqcad/ezqdcl/qar.dcl"))
   (new_dialog "qar_dcl" dcl_id)
   (set_tile "tx-exp" txy)
   (set_tile "tx-rlt" rlt)
   (set_tile "rad1" "1")
   (action_tile "butt1" "(@qar 1)")
   (action_tile "butt2" "(@qar 2)")
   (action_tile "tx-exp" "(setq txy $value)")
   (action_tile "tx-rlt" "(setq rlt $value)")
   (action_tile "rad1" "(setq eno 1)")
   (action_tile "rad2" "(setq eno 2)")
   (action_tile "accept" "(done_dialog)")
   (action_tile "cancel" "(setq eno 0)(done_dialog)")
   (start_dialog)
   (unload_dialog dcl_id)
   (if (= eno nil) (setq eno 1))
;<-dcl end
   (cond
      ((= eno 1)
         (setq en (car (entsel"\n산식을 넣을 문자선택->")))
         (setq ed (entget en))
         (setq ed (subst (cons 1 txy) (assoc 1 ed) ed))
         (entmod ed)
         (setq en (car (entsel"\n결과값을 넣을 문자선택->")))
         (setq ed (entget en))
         (setq ed (subst (cons 1 rlt) (assoc 1 ed) ed))
         (entmod ed)
      )
      ((= eno 2)
         (setq pt3 (getpoint "\n표기될 문자 위치선택->"))
         (setvar "osmode" 0)
         (command "text" "j" "r" pt3 ht "0" txy)
         (setq pt3 (polar pt3 (dtr 270) (* 1.5 ht)))
         (command "text" "j" "r" pt3 ht "0" rlt)
      )
   );cond end
   (setvar "osmode" os)(setvar "blipmode" bl)
   (setvar "cmdecho" cm)(setvar "dimzin" dz)
(prin1))

;subroutine
(defun @qar(k)
   (cond
      ((= k 1)(mode_tile "tx-exp" 2))
      ((= k 2)(mode_tile "tx-rlt" 2))
))
