;;���� Ÿ��Ʋ�� ������ ������ Ÿ��Ʋ �۾����� (������ ��+�۾�����*2) ��ŭ ������ �Ÿ��� ����
;; 2003-10-06 ����

(defun c:titS()
	(prompt "\n\t Select of Title Line & Text (Only 3EA) ? :")
	(while 	(/= (sslength (setq a (ssget))) 3)
		(prompt "\n\t Select is Only 3EA !! ")
	)
	(setq i 0 n (sslength a))
	(setq a11_0 (cdr (assoc 0 (setq a11_1 (entget (setq a11 (ssname a 0)))))))
	(setq a12_0 (cdr (assoc 0 (setq a12_1 (entget (setq a12 (ssname a 1)))))))
	(setq a13_0 (cdr (assoc 0 (setq a13_1 (entget (setq a13 (ssname a 2)))))))
	(cond 
		((= a11_0 "LINE")(setq l1 a11_1))
		((= a12_0 "LINE")(setq l1 a12_1))
		((= a13_0 "LINE")(setq l1 a13_1))
	)
	(if (= a11_1 l1)
		(tit_text23 a12_1 a13_1)
		(if (= a12_1 l1)
			(tit_text23 a11_1 a13_1)
			(tit_text23 a11_1 a12_1)
		)
	)
	(setq l1_pe (cdr (assoc 11 l1)))	; ������ ��
	(tp_get t1)
;	(setq t1_ps (cdr (assoc 10 t1)))	; Ÿ��Ʋ ������
;	(setq t1_ptb (textbox t1))			; Ÿ��Ʋ textbox point
;	(setq ptb_p1 (car t1_ptb) ptb_p2 (cadr t1_ptb))	; �����ϴ�, �����ʻ��
;	(setq ptb_p3 (list (car ptb_p1) (cadr ptb_p2)))	; ���ʻ��
;	(setq ptb_p4 (list (car ptb_p2) (cadr ptb_p1)))	; �������ϴ�

;	(setq t1_pe (polar t1_ps 0 (distance ptb_p1 ptb_p4)))	; ������ ����
	(setq t1_pe2 (polar t1_pe 0 (* 2 (cdr (assoc 40 t1)))))	; 

;	(command "line" ptb_p1 ptb_p3 ptb_p2 ptb_p4 "")	; test

	(setq l1_pe2 (list (car t1_pe2)(cadr l1_pe)))		; ���γ�������
    (setq l1 (subst (cons 11 l1_pe2) (assoc 11 l1) l1))	;
    (entmod l1)											;
	(setq tp_t1e t1_pe)
;	(setq t2_ps (cdr (assoc 10 t2)))
	(tp_get t2)
;	(setq tp_t1e t1_pe)
	(setq tp_pe2 (list (car tp_t1e) (cadr t1_pe)))		; scale ���� �̵�
	(setq tp_ps2 (polar t1_ps (angle t1_pe tp_pe2) (distance t1_pe tp_pe2)))
    (setq t2 (subst (cons 10 tp_ps2) (assoc 10 t2) t2))	;
    (entmod t2)											;

;	(command "move" t2 "" t1_pe tp_pe2)
)


(defun tp_get (t1)
	(setq t1_ps (cdr (assoc 10 t1)))	; Ÿ��Ʋ ������
	(setq t1_ptb (textbox t1))			; Ÿ��Ʋ textbox point
	(setq ptb_p1 (car t1_ptb) ptb_p2 (cadr t1_ptb))	; �����ϴ�, �����ʻ��
	(setq ptb_p3 (list (car ptb_p1) (cadr ptb_p2)))	; ���ʻ��
	(setq ptb_p4 (list (car ptb_p2) (cadr ptb_p1)))	; �������ϴ�
	(setq t1_pe (polar t1_ps 0 (distance ptb_p1 ptb_p4)))	; ������ ����
;	(setq t1_pe2 (polar t1_pe 0 (* 2 (cdr (assoc 40 t1)))))	; 
)

(defun tit_text23 (a b)
	(setq a_40 (cdr (assoc 40 a)))
	(setq b_40 (cdr (assoc 40 b)))
	(if (> a_40 b_40)
		(progn
			(setq t1 a)
			(setq t2 b)
		)
		(progn
			(setq t1 b)
			(setq t2 a)
		)
	)
)
