;[������� ���ڸ����̱�]
;BLOCk�����-���̸� ������,�������� �ʰ� ���ڿͽð��� �̸�����...
;���ڿͽð��� 10������ �����Ͽ� �Ǽ��� ǥ����
;��) �����̸� + 2010�� 02�� 09�� ���� 7�� 05�� 17�� -> [ �����̸�-20100209.190517 ] �� �ۼ���
;by ysJeong 2010.02.19

(defun c:bb (/ os ent bp blk_nme obn ins rnam byn byn2 date fn_date)

  (defun *error* (msg)(princ "error: ")(princ msg)

  (setvar "osmode" os) (princ))

   (graphscr)(terpri) (setvar "CMDECHO" 0)
   (setq os (getvar "osmode"))
   (setq date (getvar "cdate"))
   (setq fn_date (rtos date 2 6))
   (prompt "�� ������ ���� ��ü�� ������ �ϼ���... ")
   (setq ent (ssget ))
   (setq bp (getpoint "\n�� ������ Ŭ���ϼ��� : "))(terpri)
   (command "_copybase" bp ent "" "_pasteblock" bp "erase" ent "")  
      (setq ent (entlast))
        (setq elist (entget ent))
        (setq obn (cdr (assoc 2 elist)))
        (setq ins (cdr (assoc 10 elist)))
            (command "explode" ent)
            (setvar "osmode" 0)
            (setq b_name (strcat "ABC-" fn_date)) ; ���⼭ ABC- �� �ڽ��� ����- �� �ٲٽø� �˴ϴ�.
            (command "_.block" b_name ins "P" "")
            (command "_.insert" b_name ins "" "" "")
            (princ "�� ����̸� : ") (princ "[ ")(princ b_name)(princ " ]")(princ "(��)�� �ۼ��Ǿ����ϴ�.") 
   (setvar "osmode" os)
   (princ)
   );defun 




;; ���ڸ� ����� (�� �̸� ����)
(defun c:bb1 (/ os ent bp blk_nme obn ins rnam byn byn2)

 (defun *error* (msg)
   (princ "error: ")
   (princ msg)
   (setvar "osmode" os)
 (princ))

   (graphscr)(terpri) (setvar "CMDECHO" 0)
   (setq os (getvar "osmode"))
   (prompt "�� ������ ���� ��ü�� ������ �ϼ���... ")
   (setq ent (ssget ))
   (setq bp (getpoint "\n�� ������ Ŭ���ϼ��� : "))(terpri)
   (command "_copybase" bp ent "" "_pasteblock" bp "erase" ent "")  
   (setq blk_nme (getstring "\n�� ���̸��� �����Ͻðڽ��ϱ�? [ Yes/No(Esc) ] [Y]  : "))   
   (if (or (= blk_nme "")(= blk_nme "Y") (= blk_nme "y"))
   ;;�� �̸� ����
      (if (setq ent (entlast))
   (progn (setq elist (entget ent))
        (setq obn (cdr (assoc 2 elist)))
        (setq ins (cdr (assoc 10 elist)))
        (setq rname nil)
        (while (= rname nil)
          (setq rname (getstring t "\n�� ���̸��� �Է��ϼ��� : "))
          (setq byn (assoc 2 (tblsearch "block" rname)))
          (setq byn2 (cdr byn)) ; byn2 ���̸�
          (if (= rname byn2) 
            (progn (setq rname nil)
               (prompt "�� �ߺ��� �̸��Դϴ� -> �ٽ� �Է��� �ּ���")
            ) ;progn
          );if 
        ) ;while
        (if (/= rname "") 
          (progn (command "explode" ent)
            (setvar "osmode" 0)
            (command "_.block" rname ins "P" "")
            (command "_.insert" rname ins "" "" "")
            (setvar "osmode" os)
          (princ "�� ����̸� : ")(princ "[ ")(princ obn)(princ " ]")(princ " ���� ")
    (princ "[ ")(princ rname)(princ " ]")(princ "(��)�� �����Ǿ����ϴ�.") 
          );progn
        );if 
      );progn
     );if 
   ;;�� �̸� ���� ��
   );if
   (setvar "osmode" os)
   (princ)
   );defun