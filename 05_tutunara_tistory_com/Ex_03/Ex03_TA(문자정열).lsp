;;����������������������������������������������������������������
;;��program name : ���ڸ� ���� ������ ���߾� �����ϴ� ���α׷�  ��
;;��program by   : kang min soo                                 ��
;;��date         : 2003.8.7                                     ��
;;��e-mail       : kang0669@chol.com                            ��
;;����������������������������������������������������������������

(defun c:tA(/ tex lin t1 t2 ta l1 l2 lsp lep laa)
   (setvar "cmdecho" 0)
       (setq tex (entsel "\n>>���ڸ� �����Ͻʽÿ�! : "))
       (setq lin (entsel "\n>>������ �����Ͻʽÿ�! : "))
       (setq t1 (car tex)
             t2 (entget t1)
             ta (assoc 50 t2)
             l1 (car lin)
             l2 (entget l1)
       )
       (setq lsp (cdr (assoc 10 l2)))
       (setq lep (cdr (assoc 11 l2)))
       (setq laa (angle lsp lep))
         (if (and (> laa 1.5708) (<= laa 4.71239))
            (setq laa (angle lep lsp))
         ) 
         (if (and (<= laa 1.5708) (> laa 4.71239))
            (setq laa (angle lep lsp))
         )
       (setq t2 (subst (cons 50 laa) ta t2))
       (entmod t2)
  (princ)
)

        