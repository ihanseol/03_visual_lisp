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


; 두번째점은 항상 연결하려는쪽이 가까운쪽을 지정해야 합니다.
; 두점을 별나게 받아들이는 관계로 2006한글판 이외의 버젼은 보장 아니합니다. 