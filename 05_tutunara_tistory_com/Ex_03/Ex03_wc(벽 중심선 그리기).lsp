;;====================================================================
;  ��ü �߽ɼ� �׸��� (2007.6.�ָ����)
;  ->�μ��� �߰��� �߽ɼ��� �׸��� ��ɾ�(�������� �������)
;    �����϶��� �νĵ� / ���Ӽ��� ���
;  ->�߽ɼ� ���̾� ���ñ�� �߰�(2007.7.1)
;;------ wall center draw --------------------------------------------
(defun c:wc(/ os ln en1 en2 en3 ed1 ed2 p1s p1e p2s p2e d1 d2 cps cpe)
;->*error* start
 (defun *error* (msg)(princ "error: ")(princ msg)
 (setvar "osmode" os)(setvar "clayer" cl)
 (princ))
;-<*error* end
   (setq os (getvar "osmode") cl (getvar "clayer"))
   (setq en1 nil en2 nil)
   (setvar "osmode" 0)
   (prompt " ��ü �߽ɼ� �׸���...")
   (setq en3 (car (entsel "\n�߽ɼ����̾� ����<���緹�̾�>: ")))
   (if (= en3 nil) (setq ln cl)
       (setq ln (cdr (assoc 8 (entget en3)))) )
   (setvar "clayer" ln)
   (setq en1 (car (entsel "\nù��°��->")))
   (if en1 (setq en2 (car (entsel " �ι�°��->"))))
(while en2
   (setq ed1 (entget en1))
   (setq ed2 (entget en2))
   (if (and (= (cdr (assoc 0 ed1)) "LINE")
            (= (cdr (assoc 0 ed2)) "LINE"))
      (progn
         (setq p1s (cdr (assoc 10 ed1)) p1e (cdr (assoc 11 ed1)))
         (setq p2s (cdr (assoc 10 ed2)) p2e (cdr (assoc 11 ed2)))
         (if (< (distance p1s p2e) (distance p1s p2s))
             (setq tem p2s p2s p2e p2e tem) )
         (setq d1 (/ (distance p1s p2s) 2.0))
         (setq d2 (/ (distance p1e p2e) 2.0))
         (setq cps (polar p1s (angle p1s p2s) d1))
         (setq cpe (polar p1e (angle p1e p2e) d2))
         (command "line" cps cpe "") )
      (prompt "..������ �����Ұ�")
   );if end
   (setq en1 nil en2 nil)
   (setq en1 (car (entsel "\nù��°��->")))
   (if en1 (setq en2 (car (entsel " �ι�°��->"))))
);while end
   (setvar "osmode" os) (setvar "clayer" cl)
(prin1))