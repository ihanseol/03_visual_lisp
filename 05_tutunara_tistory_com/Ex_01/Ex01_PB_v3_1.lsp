;;;
;;;
;;; ��Ű���.2013.08.24.
;;;
;;;
;;; CTB, PRINTER, PAPER ����
;;;
;;; �� �κ��� ���� ctb_file, prt_name, paper_size �� ctb�����̸�,�����ͼ������̺������̸�,���������̸����� 
;;; ��ü���� AutoCAD���� �ε��Ͽ��� �Ѵ�.
;;; 
;;;

;Plot Device ����
(setq prt_name "Default Windows System Printer.pc3")		;; ������ �̸� ����
(setq ctb_file "acad.ctb")				        ;; �� ����
(setq paper_size "A3")						;; ���� ����

;PDF Device ����: ACAD2012
(setq pdf_prt_name "DWG To PDF.pc3")				;; ������ �̸� ����
(setq pdf_ctb_file " acad.ctb")				;; �� ����
(setq pdf_paper_size "ISO_expand_A3_(297.00_x_420.00_MM)")	;; ���� ����

;��¿� Layer�� ����(PLOTBOX)
(setq #PlotClayer "0")		;;�÷Կ� �ڽ� �⺻ ���̾� ���� (Default: "0")
(setq #PlotColor 2)		;;�÷Կ� �ڽ� ���� ���� (Default: nil)

;;;
;;; PLOT ������ Ȯ��
;;;
;;; ��� ����ϴ� ȯ������ PLOT������ ��ġ�� ������ ��,
;;; PLOTNAME ������� ���� ���� Ȯ���� �� �ִ�.
;;;

(defun c:PLOTNAME ()
(princ "\n�÷��� �̸�: ") (prin1 (vla-get-ConfigName LayObj))
(princ "\n���� �̸�: ") (prin1 (vla-get-CanonicalMediaName LayObj))
(princ "\n�� �̸�: ") (prin1 (vla-get-StyleSheet LayObj))
(princ "\n")(princ)
)

;;;
;;;
;;; ��� ��ɾ� ����
;;; C:OOO �� ���ϴ� ������� �ٲ۴�.
;;;
;;; -�⺻ ��ɾ�-
;;; PX: �ڽ��� �� ����ܰ��� �����Ͽ� ����Ѵ�.
;;; PXX: ���� ��ü�� ����Ѵ�.
;;; RV: ���� ��°� �����ϰ� �̸����⸦ �Ѵ�.
;;; PI: ������� �� ����ܰ��� ����Ѵ�.
;;; PB: PLOT ���� �簢�ڽ��� �����Ѵ�.
;;; PDF: �ڽ��� �� ����ܰ��� PDF���Ϸ� ����Ѵ�.
;;; PDI: ������� �� ����ܰ��� PDF���Ϸ� ����Ѵ�.
;;;
;;;

(defun C:PX () (prompt (strcat "PLOTEXPRESS\n����� ���� ����...")) (#PLOT_EXPRESS "Plot_Mode"))
(defun C:PXX () (prompt (strcat "PLOTEXPRESSALL\nPLOT���� LAYER ��ü ���...")) (#PLOT_EXPRESS "AP_Mode"))
(defun C:RV () (prompt (strcat "DREVIEW\n�̸����� �� ���� ����...")) (#PLOT_EXPRESS "RV_Mode"))
(defun C:PI () (prompt (strcat "PLOTINSERTBLOCK\n����� ��� ����...")) (#PLOT_EXPRESS "Block_Mode"))
(defun C:PB () (prompt (strcat "PLOTBOX\nPLOT���� BOX ����...")) (c:PLOTBOX))
(defun C:PDF () (prompt (strcat "PDFOUT\nBOX�� PDF�� ��������...")) (#PLOT_EXPRESS "PDF_Mode"))
(defun C:PDI () (prompt (strcat "PDFBLOCKOUT\n����� PDF�� ��������...")) (#PLOT_EXPRESS "PDFi_Mode"))

;;;
;;;
;;; ���� Open �� �ڵ����� �÷��� Ȯ��
;;;
;;;

(vl-load-com)
(setq LayObj (vla-get-ActiveLayout (vla-get-ActiveDocument (vlax-get-acad-object))))

(and
  (or
    (and
      (member prt_name (vlax-safearray->list (vlax-variant-value (vla-GetPlotDeviceNames LayObj))))
      (member ctb_file (vlax-safearray->list (vlax-variant-value (vla-GetPlotStyleTableNames LayObj))))
      ;;(member paper_size (vlax-safearray->list (vlax-variant-value (vla-GetCanonicalMediaNames LayObj))))
    )
    (prompt "\nLISP: �÷� �������� �ùٸ��� ����. PLOTNAME ������� ���� �������� Ȯ���Ͻʽÿ�.")
  )
  (or
    (/= (vla-get-ConfigName LayObj) prt_name)
    (/= (vla-get-StyleSheet LayObj) ctb_file)
    (/= (vla-get-CanonicalMediaName LayObj) paper_size)
  )
  (progn
    (vla-put-ConfigName LayObj prt_name)
    (vla-put-StyleSheet LayObj ctb_file)
    (vla-put-CanonicalMediaName LayObj paper_size)
  )
)

;;;
;;;
;;; ���� Open �� �ڵ����� UCS �ʱ�ȭ
;;;
;;;

(if (= (getvar "worlducs") 0) (and (setvar "cmdecho" 0) (vl-cmdf "UCS" "") (setvar "cmdecho" 1) (prompt (strcat "\n **UCS Initialized** "))))

;;;
;;;
;;;    �÷Թڽ� �����
;;;
;;; CLA�� �ش��ϴ� layer�� 4�� �������� �ۼ��Ѵ�.
;;; �ش� layer�� ���� ��� �ڵ����� �ۼ��Ѵ�.
;;; 4�� �������� ������ �ܰ��� ����� ���Ŀ� �ϰ���¿� Ȱ�밡���ϴ�.
;;;
;;;

(defun c:PLOTBOX ( / COL CLA pt)
(if (= (getvar "worlducs") 0) (and (setvar "cmdecho" 0) (vl-cmdf "UCS" "") (setvar "cmdecho" 1)))
(setq CLA #PlotClayer)
(setq COL #PlotColor)
(if (= nil (tblsearch "layer" CLA))
  (entmake (list (cons 0 "LAYER") (cons 100 "AcDbSymbolTableRecord") (cons 100 "AcDbLayerTableRecord")
		 (cons 2 CLA) (cons 6 "CONTINUOUS") (cons 62 2) (cons 70 0)
	   )
  )
)
(and (setq pt (getpoint "ù ��° ������ ����: "))
     (cadr (setq pt (list pt (getcorner pt "�ٸ� ������ ����: "))))
     (setq pt (list (list (caar pt) (caadr pt)) (list (cadar pt) (cadadr pt))))
)
(if (= (length pt) 2)
  (entmake 
    (vl-remove-if 'not
	(list (cons 0 "LWPOLYLINE") (cons 100 "AcDbEntity") (cons 8 CLA)
	      (if COL (cons 62 COL)) (cons 100 "AcDbPolyline") (cons 90 4) (cons 70 1)
	      (cons 10 (list (apply 'min (car pt)) (apply 'min (cadr pt))))
	      (cons 10 (list (apply 'max (car pt)) (apply 'min (cadr pt))))
	      (cons 10 (list (apply 'max (car pt)) (apply 'max (cadr pt))))
	      (cons 10 (list (apply 'min (car pt)) (apply 'max (cadr pt))))
	)
    )
  )
)(princ)
)

;;;
;;;
;;;    �ڽ� ����
;;;
;;; �ڽ��� ������ ������ �������� x�� �������� �������Ѵ�.
;;;
;;;

(defun PTE:highlight (ss x) (mapcar '(lambda (ent)(redraw ent x)) (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))))

(defun #SS_Sort_Sub (ss / n buffer ss_lst)
(setq n (cadddr (car (ssnamex ss))))
(foreach x (ssnamex ss)
  (if (equal n (cadddr x))
    (setq buffer (append buffer (list (cadr x))))
    (setq ss_lst (append ss_lst (list (list n buffer))) n (cadddr x) buffer (list (cadr x)))
  )
)
(setq ss_lst (vl-remove-if '(lambda (x) (listp (car (cadr x)))) (append ss_lst (list (list n buffer)))))
ss_lst
)


(defun #SS_Sort_Main (ss / SortSS MinPt MaxPt MinPtB MaxPtB)
(setq SortSS (ssadd))
(foreach x (#SS_Sort_Sub ss)
  (if (= (length (cadr x)) 1)
    (ssadd (car (cadr x)) SortSS)
    (mapcar '(lambda (f) (ssadd f SortSS))
      (vl-sort (cadr x)
	'(lambda (a b)
	 (if
	  (equal 
	    (cadr (progn (vla-GetBoundingBox (vlax-ename->vla-object a) 'MinPt 'MaxPt) (vlax-safearray->list MinPt)))
	    (cadr (progn (vla-GetBoundingBox (vlax-ename->vla-object b) 'MinPtB 'MaxPtB) (vlax-safearray->list MinPtB)))
	    40.0
	  )
	  (< (car (vlax-safearray->list MinPt)) (car (vlax-safearray->list MinPtB)))
	  (> (cadr (vlax-safearray->list MinPt)) (cadr (vlax-safearray->list MinPtB)))
	 )
	)
      )
    )
  )
)SortSS
)

(defun List->Safearray (lst) (vlax-safearray-fill (vlax-make-safearray vlax-vbdouble (cons 0 (1- (length lst)))) lst))

(defun #BoxReaction (ss SF_List / en n MinPt MaxPt ns nb)
(repeat (setq n (sslength ss))
  (and
    (setq en (ssname ss (setq n (1- n))))
    (progn
      (command "zoom" "object" en "")
      (vla-GetBoundingBox (vlax-ename->vla-object en) 'MinPt 'MaxPt)
      (setq MinPt (vlax-safearray->list MinPt) MaxPt (vlax-safearray->list MaxPt))
      (if (> 70 (distance MinPt MaxPt))
	(ssdel en ss)
	(if (setq nb 0 ns (ssget "w" MinPt MaxPt SF_List))
	  (progn (ssdel en ns) (repeat (sslength ns) (ssdel (ssname ns nb) ss) (setq nb (1+ nb))))
	)
      )
    )
  )
)ss
)

;;;
;;;   PDF �ѹ���
;;;
;;; PDF������ �ѹ��� �����ϰų� �����Ѵ�.
;;;
;;;

(defun $PDF_NUM_ADD ( / n TXT1 TXT2 FN F1 F2)
(setq n 0
      TXT1
        (vl-list->string
	  (reverse
	    (vl-remove-if '(lambda (f) (= 0 (if (= f 92) (setq n nil) n))) (reverse (vl-string->list #PDF_Filename)))
	  )
        )
      TXT2 (substr #PDF_Filename (1+ (strlen TXT1)))
      FN (substr TXT2 1 (- (setq n (strlen TXT2)) 4))
)
(if (< 4 n)
  (progn
    (setq n 0
	  F2 (reverse (vl-remove-if '(lambda (f) (= nil (if (or (< f 48) (> f 57)) (setq n nil) n))) (reverse (vl-string->list FN))))
	  F1 (substr FN 1 (- (strlen FN) (length F2)))
    )
    (setq F2 (vl-string->list (itoa (fix (1+ (atoi (vl-list->string F2)))))))
    (if (> 3 (length F2)) (while (> 3 (length F2)) (setq F2 (append (list 48) F2))))
    (setq F2 (vl-list->string F2) FN (strcat F1 F2))
  )
  (setq FN (strcat FN "001"))
)(setq #PDF_Filename (strcat TXT1 FN ".pdf"))#PDF_Filename
)


(defun $PDF_NUM_MINUS ( / n TXT1 TXT2 FN F1 F2)
(setq n 0
      TXT1
        (vl-list->string
	  (reverse
	    (vl-remove-if '(lambda (f) (= 0 (if (= f 92) (setq n nil) n))) (reverse (vl-string->list #PDF_Filename)))
	  )
        )
      TXT2 (substr #PDF_Filename (1+ (strlen TXT1)))
      FN (substr TXT2 1 (- (setq n (strlen TXT2)) 4))
)
(if (< 4 n)
  (progn
    (setq n 0
	  F2 (reverse (vl-remove-if '(lambda (f) (= nil (if (or (< f 48) (> f 57)) (setq n nil) n))) (reverse (vl-string->list FN))))
	  F1 (substr FN 1 (- (strlen FN) (length F2)))
    )
    (setq F2 (vl-string->list (itoa (fix (1- (atoi (vl-list->string F2)))))))
    (if (> 3 (length F2)) (while (> 3 (length F2)) (setq F2 (append (list 48) F2))))
    (setq F2 (vl-list->string F2) FN (strcat F1 F2))
  )
  (setq FN (strcat FN "001"))
)(setq #PDF_Filename (strcat TXT1 FN ".pdf"))#PDF_Filename
)

;;;
;;;
;;;   �ڽ� ���
;;;
;;; ������ 4�� �������� ������ ����Ѵ�.
;;; ���߼��õ� �����ϴ�.
;;;
;;;

(defun #PLOT_EXPRESS ($Mode / AcDoc AcLay AcPlot sa ss n MinPt MaxPt LP SF_List CLA)
(if (= (getvar "worlducs") 0) (and (setvar "cmdecho" 0) (vl-cmdf "UCS" "") (setvar "cmdecho" 1)))
(setq CLA #PlotClayer);;��ü �ڵ���� �� Plot���� Layer ������
(if (or (= $Mode "Block_Mode") (= $Mode "PDFi_Mode"))
  (setq SF_List (list (cons 0 "INSERT")))
  (setq SF_List (list (cons 0 "LWPOLYLINE") (cons 40 0) (cons 41 0) (cons 70 1) (cons 90 4)))
)

(setq AcDoc (vla-get-ActiveDocument (vlax-get-acad-object))
      AcLay (vla-get-activelayout AcDoc)
      AcPlot (vla-get-plot AcDoc)
      n 0
)


(if (or (= $Mode "PDF_Mode") (= $Mode "PDFi_Mode"))
  (and
    (if #PDF_Filename ($PDF_NUM_ADD) (setq #PDF_Filename (strcat (getvar "dwgprefix") "A001.pdf")))
    (or
      (/= (vla-get-ConfigName AcLay) pdf_prt_name)
      (/= (vla-get-StyleSheet AcLay) pdf_ctb_file)
      (/= (vla-get-CanonicalMediaName AcLay) pdf_paper_size)
    )
    (progn
      (vla-put-ConfigName AcLay pdf_prt_name)
      (vla-put-StyleSheet AcLay pdf_ctb_file)
      (vla-put-CanonicalMediaName AcLay pdf_paper_size)
    )
  )
  (and
    (or
      (/= (vla-get-ConfigName AcLay) prt_name)
      (/= (vla-get-StyleSheet AcLay) ctb_file)
      (/= (vla-get-CanonicalMediaName AcLay) paper_size)
    )
    (progn
      (vla-put-ConfigName AcLay prt_name)
      (vla-put-StyleSheet AcLay ctb_file)
      (vla-put-CanonicalMediaName AcLay paper_size)
    )
  )
)

(and 
  (if (or (= $Mode "PDF_Mode") (= $Mode "PDFi_Mode"))
    (and
      (if (setq Buffer (getfiled "PDF ���� �ۼ�" #PDF_Filename "pdf" 1))
	(and
	  (setq #PDF_Filename Buffer)
	  ($PDF_NUM_ADD)
	  ($PDF_NUM_MINUS)
	)
      )
    ) T
  )
  (if (= $Mode "AP_Mode")
    (progn
      (initget "Yes No")
      (if (/= (getkword "\n��ü ������ ����Ͻðڽ��ϱ�? [��(Y)/�ƴϿ�(N)] <Y>: ") "No") (setq sa (ssget "X" SF_List)))
    )
    (setq sa (ssget SF_List))
  )
  (progn (vla-startundomark AcDoc) (setvar "cmdecho" 0) (command "zoom" "object" ss)(graphscr) (setq ss (#SS_Sort_Main sa)))
  (#BoxReaction ss SF_List)
  (/= (sslength ss) 0)
  (progn
    (vla-startundomark acDoc)
    (setvar "backgroundplot" 0)
    (vla-put-paperunits AcLay acmillimeters)
    (vla-put-standardscale AcLay acvpscaletofit)
    (vla-put-centerplot AcLay :vlax-true)
    (command "zoom" "object" ss "")(graphscr)
    (setvar "cmdecho" 1)
    (repeat (sslength ss)
      (vla-GetBoundingBox (vlax-ename->vla-object (ssname ss n)) 'MinPt 'MaxPt)
      (setq MinPt (vlax-safearray->list MinPt)
	    MaxPt (vlax-safearray->list MaxPt)
	    n (1+ n)
      )
      (ssget "W" MinPt MaxPt)
      (if (< (- (car MinPt) (car MaxPt)) (- (cadr MinPt) (cadr MaxPt)))	(setq LP 1) (setq LP 0))
      (setq DCSFix (trans '(0 0) 2 0)
	    MinPt (List->Safearray (list (- (car MinPt) (car DCSFix)) (- (cadr MinPt) (cadr DCSFix))))
	    MaxPt (List->Safearray (list (- (car MaxPt) (car DCSFix)) (- (cadr MaxPt) (cadr DCSFix))))
      )
      (vla-put-PlotRotation AcLay LP)
      (vla-SetWindowToPlot AcLay MinPt MaxPt)
      (vla-put-PlotType AcLay AcWindow)
      (if (= $Mode "RV_Mode")
	(vla-DisplayPlotPreview AcPlot acPartialPreview)
	(if (or (= $Mode "PDF_Mode") (= $Mode "PDFi_Mode"))
	  (progn (vla-PlotToFile AcPlot #PDF_Filename) ($PDF_NUM_ADD))
	  (progn (vla-PlotToDevice AcPlot) (PTE:highlight (ssget "P") 2)) ;;��°�ü ���� �ɼ�
	)
      )
    )
    (sssetfirst nil ss) (ssget)
    (setvar "cmdecho" 1)
    (vla-endundomark acDoc)
    (if (/= $Mode "RV_Mode")
      (prompt 
	(strcat "\n" (getvar "dwgprefix")(getvar "dwgname")
	  (if (or (= $Mode "PDF_Mode") (= $Mode "PDFi_Mode")) "\n" "\n\n��µ� ��ü�� ǥ���Ϸ��� Regen������� �����...\n")
	  (rtos n) " ���� ��µǾ����ϴ�."
	)
      )
      (prompt (strcat "\n" (rtos n) "���� �̸����� �Ϸ��Ͽ����ϴ�."))
    )
    (if (and (/= $Mode "RV_Mode") (> n 1))
      (alert 
	(strcat "\n\t" (getvar "dwgname") "\n\t" (rtos n) " ���� ��µǾ����ϴ�."
	  (if (or (= $Mode "PDF_Mode") (= $Mode "PDFi_Mode")) "" "\n\n\t��µ� ��ü�� ǥ���Ϸ��� Regen������� ��ü ������� �ʿ��մϴ�.")
	)
      )
    )(vlr-beep-reaction) ;;��¿Ϸ� ��ȣ��
  )
)
(if (or (= $Mode "PDF_Mode") (= $Mode "PDFi_Mode")) ($PDF_NUM_MINUS))
(vla-endundomark AcDoc)
(princ)
)

(defun C:PLOTEXPRESS () (prompt (strcat "PLOTEXPRESS\n����� ���� ����...")) (#PLOT_EXPRESS "Plot_Mode"))
(defun C:PLOTEXPRESSALL () (prompt (strcat "PLOTEXPRESSALL\nPLOT���� LAYER ��ü ���...")) (#PLOT_EXPRESS "AP_Mode"))
(defun C:DREVIEW () (prompt (strcat "DREVIEW\n�̸����� �� ���� ����...")) (#PLOT_EXPRESS "RV_Mode"))
(defun C:PLOTBLOCK () (prompt (strcat "PLOTINSERTBLOCK\n����� ��� ����...")) (#PLOT_EXPRESS "Block_Mode"))
(defun C:PDFOUT () (prompt (strcat "PDFOUT\nPDF�� ��������...")) (#PLOT_EXPRESS "PDF_Mode"))
(defun C:PDFBLOCKOUT () (prompt (strcat "PDFBLOCKOUT\nPDF�� ��������...")) (#PLOT_EXPRESS "PDFi_Mode"))