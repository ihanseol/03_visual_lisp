(defun c:pi ()   

   (princ "=pi()")  
   (command "dimoverride" "dimpost" "%%c<>" "") 
   (prompt "\n표시할 치수를 선택하세요.")

   (princ)
)

; pi 삭제
(defun c:pp ()   

   (princ "=pi()")  
   (command "dimoverride" "dimpost" "" "") 
   (prompt "\n표시할 치수를 선택하세요.")

   (princ)
)

 
;-PCD : (COMMAND "dimoverride" "dimpost" "PCD<>" "")
;-중심에 대한 : (COMMAND "dimoverride" "dimpost" "<> CENTRAL" "")
;-괄호 : (COMMAND "dimoverride" "dimpost" "(<>)" "")
;-참고 : (COMMAND "dimoverride" "dimpost" "<> REF" "")
;-사각 : (COMMAND "dimoverride" "dimpost" "□ <>" "")
;-등간격 : (COMMAND "dimoverride" "dimpost" "<> EQ. SPACED" "")
;-깊이 : (COMMAND "dimoverride" "dimpost" "<> DEPTH" "")
;-요 : (COMMAND "dimoverride" "dimpost" "凹<>" "")
;-철 : (COMMAND "dimoverride" "dimpost" "凸<>" "")
;-바깥쪽 : (COMMAND "dimoverride" "dimpost" "<> OUT SIDE" "")
;-안쪽 : (COMMAND "dimoverride" "dimpost" "<> IN SIDE" "")


;;; 선택한 치수 or 문자에 파이(Ø) 넣기
;(defun c:pi(/ os ss sslen ass1 otxt ntxt elist k)
;   (setvar "cmdecho" 0)
;   (setq os (getvar "osmode"))
;   (setvar "osmode" 0)
;   (prompt "\n파이(Ø) 넣기할 치수나 문자선택 ")
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