(defun sel_2p ()
  ;(setvar "osmode" 1)
  (setq p1 (getpoint "Strat Point ? :"))
  (command "_select" "si" "c" p1 pause )
  (setq p2 (car (cdr (grread t))))
  ;(setq p2 (osnap px "_end,_int"))
  ;(command "line" p1 p2 "")
)

(defun c:llc (/ p1 p2 pts pte pts_l1 pts_l2 pte_l1 pte_l2)
  (setvar "osmode" 0)
  (while (or (= p1 nil)(= p2 nil))
    (sel_2p)
  )
  (setq ents (ssget "P" (list (cons 0 "LINE"))))
  (setq pts_l1 (cdr (assoc 10 (entget (ssname ents 0)))))
  (setq pte_l1 (cdr (assoc 11 (entget (ssname ents 0)))))
  (setq pts_l2 (cdr (assoc 10 (entget (ssname ents 1)))))
  (setq pte_l2 (cdr (assoc 11 (entget (ssname ents 1)))))

  (if (< (distance p2 pts_l1) (distance p2 pte_l1))
    (setq pts pts_l1)
    (setq pts pte_l1)
  )
  (if (< (distance p2 pts_l2) (distance p2 pte_l2))
    (setq pte pts_l2)
    (setq pte pte_l2)
  )

  (command "line" pts pte "")
)


; �ι�°���� �׻� �����Ϸ������� ��������� �����ؾ� �մϴ�.
; ������ ������ �޾Ƶ��̴� ����� 2006�ѱ��� �̿��� ������ ���� �ƴ��մϴ�. 