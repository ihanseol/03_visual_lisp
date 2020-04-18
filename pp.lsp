(defun findplotarea (blocks / en lst ipt ipt&en data xsort xysort)
	(while
		(setq en (ssname blocks 0))
		(setq lst (entget en))
		(setq ipt (cdr (assoc 10 lst)))
		(setq ipt&en (cons ipt en))
		(setq data (cons ipt&en data))
		(ssdel en blocks)
	)
	(setq xsort (vl-sort data (function (lambda (a b) (< (caar a) (caar b))))))
	(setq xysort (vl-sort xsort (function (lambda (a b) (> (cadar a) (cadar b))))))
)

(defun c:pp(/ ss plotlist num vlename min max llp urp width height landscape)
	(setvar "cmdecho" 0)
	(setq ss (ssget '((0 . "insert"))))
	(setq plotlist (findplotarea ss))
	;(setq num 0)
	(repeat (length plotlist)
		(setq vlename (vlax-ename->vla-object (cdar plotlist)))
		;(setq vlename (vlax-ename->vla-object (cdr (nth num plotlist))))
		(vla-getboundingbox vlename 'min 'max)
		(setq llp (vlax-safearray->list min))
		(setq urp (vlax-safearray->list max))
		(setq width (- (car urp) (car llp)))
		(setq height (- (cadr urp) (cadr llp)))
		(if
			(> width height)
			(setq landscape "l")
			(setq landscape "p")
		)
		(command	"._plot"
					"Y"								;�� ����
					""								;����
					"DocuCentre-IV C2263"						;�÷��� �̸�
					"ISO ��ü ������ A4(297.00 x 210.00 mm)"			;���� ũ��
					"m"								;���� ���� �и�����
					landscape							;���� ���� �Ǵ� landscape, portrait
					"N"								;�� �Ʒ� ����
					"W"								;�μ� ���� - ������ �Է�
					llp								;���� �Ʒ� �� lowleftpoint
					urp								;������ �� �� uprightpoint
					"f"								;
					"c"								;�÷� ���� ���� - �߽�(c)
					"Y"								;�÷� ��Ÿ�� ��� ����
					"monochrome.ctb"						;�÷� ��Ÿ��
					"y"								;�� ����ġ ���� ����
					"a"								;���� ���� - ǥ�õǴ� ���(A)/���̾�������(W)/����(H)/�� ��Ÿ��(V)/����(R)
					"n"								;���Ϸ� ���� ����
					"n"								;������ ���� ���� ����
					"y"								;�÷� ���� ����
		)
		;(setq num (1+ num))
		(setq plotlist (cdr plotlist))
	)
)

(defun c:ppdf(/ ss plotlist filename pdfnameprefix pdfpath pdfname vlename min max llp urp width height landscape)
	(setvar "cmdecho" 0)
	(setq ss (ssget '((0 . "insert"))))
	(setq plotlist (findplotarea ss))
	(setq filename (getvar "dwgname"))
	(setq pdfnameprefix (substr filename 1 (- (strlen filename) 4)))
	(setq pdfpath (strcat (getvar "dwgprefix") pdfnameprefix))
	;(setq pdfpath (acet-ui-pickdir "�� ������ ����� �����" (getvar "dwgprefix") "pdf�� ������ ������ �����ϼ���"));expresstools ��ġ�ؾ� ��밡��
	(repeat (length plotlist)
		(setq pdfname (strcat pdfpath "-" (rtos (getvar "cdate") 2 6) ".pdf"))
		(setq vlename (vlax-ename->vla-object (cdar plotlist)))
		(vla-getboundingbox vlename 'min 'max)
		(setq llp (vlax-safearray->list min))
		(setq urp (vlax-safearray->list max))
		(setq width (- (car urp) (car llp)))
		(setq height (- (cadr urp) (cadr llp)))
		(if
			(> width height)
			(setq landscape "l")
			(setq landscape "p")
		)
		(command	"._plot"
					"Y"											;�� ����
					""											;����
					"DWG To PDF.pc3"						;�÷��� �̸�
					"ISO ��ü ������ A4(297.00 x 210.00 mm)"			;���� ũ��
					"m"								;���� ���� �и�����
					landscape							;���� ���� �Ǵ� landscape, portrait
					"N"								;�� �Ʒ� ����
					"W"								;�μ� ���� - ������ �Է�
					llp								;���� �Ʒ� �� lowleftpoint
					urp								;������ �� �� uprightpoint
					"f"								;
					"c"								;�÷� ���� ���� - �߽�(c)
					"Y"								;�÷� ��Ÿ�� ��� ����
					"monochrome.ctb"						;�÷� ��Ÿ��
					"y"								;�� ����ġ ���� ����
					"a"								;���� ���� - ǥ�õǴ� ���(A)/���̾�������(W)/����(H)/�� ��Ÿ��(V)/����(R)
					pdfname								;pdf���� ����
					"n"								;������ ���� ���� ����
					"y"								;�÷� ���� ����
		)
		(command "delay" 1000)
		(setq plotlist (cdr plotlist))
	)
)



	   
	   