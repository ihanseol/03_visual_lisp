;;====================================================================
;  �簢������ �������ϱ� (2006.11 �ָ����)
;  ->�簢�� ������ �밢�� ������ �����Ͽ� ����,����,������
;    ǥ���ϴ� ��ɾ�(�ڸ��� ����)
;  ->��������/dcl����(2007.7.4)
;  ->�ڸ��� ǥ�� ��������(2007.8.13)
;------- quick area --------------------------------------------------
(defun c:qar(/ os ts cm bl a yn ht tem dot pt1 pt2 pt3 rlt txy dx dy
               dix diy dxy txy1 k eno en ed dz)
;->*error* start
 (defun *error* (msg)(princ "error: ")(princ msg)
 (setvar "osmode" os)(setvar "blipmode" bl)(setvar "dimzin" dz)
 (princ))
;-<*error* end
   (princ " �簢������ �������ϱ�...")
   (setq os (getvar "osmode") ts (getvar "textsize")
         cm (getvar "cmdecho") bl (getvar "blipmode")
         dz (getvar "dimzin") )
   (setvar "cmdecho" 0) (setvar "blipmode" 1)(setvar "dimzin" 0)
;-->������ ����
   (if (= dwqar nil) (setq dwqar 2))
   (setq a (strcat "\n���� ������(���ڳ���=" (rtos ts 2 1)
            ",�ڸ���=" (rtos dwqar 2 0) ")�� �����ұ��? [Y/N] <N> : "))
   (initget "Yes No")
   (setq yn (getkword a))
   (if (= yn "Yes")
     (progn
       (setq a (strcat "\n���ڳ���<" (rtos ts 2 1) ">:"))
       (setq ht (getdist a))
       (if (= ht nil) (setq ht ts))
       (setq a (strcat "\n�Ҽ��� �ڸ����� ����<" (rtos dwqar 2 0) ">:"))
       (setq tem (getint a))
       (if (= tem nil) (setq dot dwqar) (setq dot (abs tem)) )
       (setq dwqar dot)
     )
     (setq ht ts dot dwqar)
   )
;--<
   (setq pt1 (getpoint "\n�簢���� ���� ����-> "))
   (setq rlt "0" k 1)
(while pt1
   (setq pt2 (getcorner pt1 "\�밢�� ����-> "))
   (if pt2 (progn
      (setq dx (abs (- (car pt2) (car pt1))))
      (setq dy (abs (- (cadr pt2) (cadr pt1))) ) (terpri)
      (setq dx (/ dx 1000) dy (/ dy 1000) )
      (setq dix (rtos dx 2 dot));->x��
      (setq diy (rtos dy 2 dot));->y��
      (setq dxy (* (atof dix) (atof diy)))
      (setq dixy (rtos dxy 2 dot));->x*y��
      (setq txy1 (strcat "( " dix " x " diy " )" ))
      (setq rlt (rtos (+ (atof rlt) (atof dixy)) 2 dot))
      (if (> k 1) (setq txy (strcat txy " + " txy1))
                  (setq txy txy1))
      (setq k (+ k 1))
      (princ "\n���: ")(princ txy)(princ " = ")(princ rlt)  ))
   (setq pt1 (getpoint "\n�簢���� ���� ����<dcl����>-> "))
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
         (setq en (car (entsel"\n����� ���� ���ڼ���->")))
         (setq ed (entget en))
         (setq ed (subst (cons 1 txy) (assoc 1 ed) ed))
         (entmod ed)
         (setq en (car (entsel"\n������� ���� ���ڼ���->")))
         (setq ed (entget en))
         (setq ed (subst (cons 1 rlt) (assoc 1 ed) ed))
         (entmod ed)
      )
      ((= eno 2)
         (setq pt3 (getpoint "\nǥ��� ���� ��ġ����->"))
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
