(defun c:sss ()
(tt 1)
(princ))

(defun c:LLL ()
(tt nil)
(princ))



(defun tt (mmgt / temp text edata)
(if (setq edata (entsel "\n ��(A), ��(a) ���ڷ� �ٲٰ� ���� ���ڸ� ������ �ּ��� :  "))
    (progn
       (setq edata (entget (car edata))
             temp (assoc 1 edata)
             text (strcase (cdr temp) mmgt )
             edata (subst (cons 1 text) temp edata)
       )
       (entmod edata)
))
(princ))

(princ "\n����ҹ�����ȯ.lsp �÷���, �빮�ڷ� �ٲܶ� LLL, �ҹ��ڷ� �ٲܶ� SSS")
(princ)

