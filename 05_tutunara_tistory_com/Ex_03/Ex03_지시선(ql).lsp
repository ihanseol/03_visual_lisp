;;=======================================================
;  ���ü��׸���(2007.5.3 �ָ����)
;  ->ġ����Ÿ�� ������� �����԰��� ���ü��� �׷���.
;  ->ȭ���˼��� �� ���ü��� ������ ����.
;;------ quick leader -----------------------------------
(defun c:ql(/ a kw os ldr asz clrd pt1 pt2 pt3 dw)
;->*error* start
 (defun *error* (msg)(princ "error: ")(princ msg)
 (setvar "osmode" os)(setvar "dimldrblk" ldr)
 (setvar "dimasz" asz)(setvar "dimclrd" clrd)
 (princ))
;-<*error* end
   (setq os (getvar "osmode"))
   (setq ldr (getvar "dimldrblk"))
   (setq asz (getvar "dimasz"))
   (setq clrd (getvar "dimclrd"))
   (setvar "osmode" 512)
   (setvar "dimclrd" 256) ;ġ���� ����
   (prompt " ���ü��׸���...")
;->ȭ���� ����
   (setq a "\nȭ���� ����[������(1)/�ݰ�ä��(2)/���ഫ��(3)/�����ʰ���(4)] <������(1)>: ")
   (initget "1 2 3 4")
   (setq kw (getkword a))
   (if (= kw nil) (setq kw "1"))
   (cond ((= kw "1")
          (progn (setvar "dimldrblk" "_DOTSMALL") (setvar "dimasz" 4))) ;������
         ((= kw "2")
          (progn (setvar "dimldrblk" ".")(setvar "dimasz" 3))) ;�ݰ�ä��
         ((= kw "3")
          (progn (setvar "dimldrblk" "_ARCHTICK") (setvar "dimasz" 2))) ;���ഫ��
         ((= kw "4")
          (progn (setvar "dimldrblk" "_OPEN90")(setvar "dimasz" 3))) ;�����ʰ���
   )
;-<
   (setq pt1 (getpoint "\n���ü� ������ ����->"))
   (setvar "osmode" 0)
   (setq pt2 (getpoint pt1 "\n������ ����->"))
   (setq pt3 (getpoint pt2 "\n������ ����->"))
   (setq dw (getstring "\n���ü� ����[����(St)/���ö���(S)] <����(ST)>: "))
   (if (= dw nil) (setq dw "st"))
   (if pt3
     (command "leader" pt1 pt2 pt3 "f" dw "" "" "n")
     (command "leader" pt1 pt2  "f" dw "" "" "n")
   )
   (setvar "osmode" os)
   (setvar "dimldrblk" ldr)
   (setvar "dimasz" asz)
   (setvar "dimclrd" clrd)
(prin1))
