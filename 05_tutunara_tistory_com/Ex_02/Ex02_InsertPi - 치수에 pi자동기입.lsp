(defun c:pi ()   

   (princ "=pi()")  
   (command "dimoverride" "dimpost" "%%c<>" "") 
   (prompt "\nǥ���� ġ���� �����ϼ���.")

   (princ)
)

; pi ����
(defun c:pp ()   

   (princ "=pi()")  
   (command "dimoverride" "dimpost" "" "") 
   (prompt "\nǥ���� ġ���� �����ϼ���.")

   (princ)
)

 
;-PCD : (COMMAND "dimoverride" "dimpost" "PCD<>" "")
;-�߽ɿ� ���� : (COMMAND "dimoverride" "dimpost" "<> CENTRAL" "")
;-��ȣ : (COMMAND "dimoverride" "dimpost" "(<>)" "")
;-���� : (COMMAND "dimoverride" "dimpost" "<> REF" "")
;-�簢 : (COMMAND "dimoverride" "dimpost" "�� <>" "")
;-��� : (COMMAND "dimoverride" "dimpost" "<> EQ. SPACED" "")
;-���� : (COMMAND "dimoverride" "dimpost" "<> DEPTH" "")
;-�� : (COMMAND "dimoverride" "dimpost" "��<>" "")
;-ö : (COMMAND "dimoverride" "dimpost" "��<>" "")
;-�ٱ��� : (COMMAND "dimoverride" "dimpost" "<> OUT SIDE" "")
;-���� : (COMMAND "dimoverride" "dimpost" "<> IN SIDE" "")


;;; ������ ġ�� or ���ڿ� ����(��) �ֱ�
;(defun c:pi(/ os ss sslen ass1 otxt ntxt elist k)
;   (setvar "cmdecho" 0)
;   (setq os (getvar "osmode"))
;   (setvar "osmode" 0)
;   (prompt "\n����(��) �ֱ��� ġ���� ���ڼ��� ")
;   (setq ss (ssget))
;   (setq sslen (sslength ss) k 0)
;   (while (< k sslen)
;       (setq elist (entget (ssname ss k)) )
;       (setq ass1 (assoc 1 elist) otxt (cdr ass1) )
;       (if (= otxt "") (setq otxt "<>"))
;       (if (and (= (wcmatch otxt "%%c*") nil) (= (wcmatch otxt "%%C*") nil))
;           (progn
;              (setq ntxt  (strcat "%%C" otxt ) elist (subst (cons 1 ntxt) ass1 elist))
;              (entmod elist) 
;           )
;       )
;       (setq k (+ K 1))
;   )
;  (setvar "osmode" os)
;   (prin1) 
;)