;;=====================================================================
;  �������ϱ�(98�ָ����)
;  ->pline�̳� �������� �����Ͽ� ȭ�鿡 ������ �ڵ����� ǥ��(������)
;  ->���������� �ɼ��߰�/�ڸ��� ���ð���(2007.5.11)
;;------ quick area ---------------------------------------------------
(defun c:qa(/ os ot ts ht a yn tem ent1 en1 grpt pt1 ar1 ar2 ar3)
;->*error* start
 (defun *error* (msg)(princ "error: ")(princ msg)
 (setvar "osmode" os)(setvar "orthomode" ot)
 (princ))
;-<*error* end
   (prompt "...�������ϱ�")
   (setq os (getvar "osmode") ot (getvar "orthomode")
         ts (getvar "textsize"))
   (setvar "orthomode" 0) (setvar "osmode" 0)
   (if (= dw nil) (setq dw 2))
;������ ����
   (setq a (strcat "\n���� ������(���ڳ���=" (rtos ts 2 1)
            ",�ڸ���=" (rtos dw 2 0) ")�� �����ұ��? [Y/N] <N> : "))
   (initget "Yes No")
   (setq yn (getkword a))
   (if (= yn "Yes")
     (progn
       (setq a (strcat "\n���ڳ���<" (rtos ts 2 1) ">:"))
       (setq ht (getdist a))
       (if (= ht nil) (setq ht ts))
       (setq a (strcat "\n�Ҽ��� �ڸ����� ����<" (rtos dw 2 0) ">:"))
       (setq tem (abs (getint a)))
       (if (= tem nil) (setq dot dw) (setq dot tem) )
       (setq dw dot)
     )
     (setq ht ts dot dw)
   )
;���ù��
   (setq ent1 (entsel  "\nPline���� <������ ����>->"))
   (if ent1 
     (progn
       (setq en1 (car ent1)) (redraw en1 3)
       (setq grpt (grread T))
       (setq pt1 (cadr grpt))
       (command "area" "e" ent1)  )
     (progn
       (prompt "\nSelect internal point->")
       (command "boundary" pause "")
       (setq en1 (entlast))(redraw en1 3)
       (command "area" "e" "l")
       (setq pt1 (getvar "lastpoint"))  )
   )
;
   (setq ar1 (/ (getvar "area") 1000000));�������ͷ� ����
   (setq ar2 (rtos ar1 2 dot));�ڸ���=dot
   (if (>= dot 1)
       (setq ar3 (@tx_dot ar2 dot))
       (setq ar3 ar2)
   )
   (command "text" "j" "r" pt1 ht "0" ar3)
   (prompt "\nEnter text point->")
   (command "move" "l" "" pt1 pause)
   (redraw en1 4)
   (setvar "osmode" os)(setvar "orthomode" ot)
   (prompt "\n����: ")(prin1 ar1)(prompt "��")
(prin1))
;���ڷε� ������ �ڸ����� ���߰� ���ڷ� �ǵ�����
(defun @tx_dot(tx dot / k k1 tx tx1 tx2 tx3)
   (setq k1 (vl-string-position (ascii ".") tx))
   (if (= k1 nil) (setq tx1 (strcat tx ".0")) (setq tx1 tx))
   (setq k (strlen tx1))
   (setq k1 (vl-string-position (ascii ".") tx1))
   (setq tx2 (substr tx1 1 (+ k1 1)));�����κ�
   (setq tx3 (substr tx1 (+ k1 2)));�Ҽ��κ�
   (setq k (strlen tx3))
   (while (< k dot)
      (setq tx3 (strcat tx3 "0"))
      (setq k (strlen tx3))
   )
   (strcat tx2 tx3)
)