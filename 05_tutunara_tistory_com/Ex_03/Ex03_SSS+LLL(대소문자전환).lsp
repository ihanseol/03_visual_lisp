(defun c:sss ()
(tt 1)
(princ))

(defun c:LLL ()
(tt nil)
(princ))



(defun tt (mmgt / temp text edata)
(if (setq edata (entsel "\n 대(A), 소(a) 문자로 바꾸고 싶은 문자를 선택해 주세요 :  "))
    (progn
       (setq edata (entget (car edata))
             temp (assoc 1 edata)
             text (strcase (cdr temp) mmgt )
             edata (subst (cons 1 text) temp edata)
       )
       (entmod edata)
))
(princ))

(princ "\n▶대소문자전환.lsp 올려짐, 대문자로 바꿀땐 LLL, 소문자로 바꿀땐 SSS")
(princ)

