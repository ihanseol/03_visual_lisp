;û��� ����------------------------------------------------------------
;���������� ó������ ���ڸ� ���� ���̶�� ���� �۾��ε� ���� �ø��� ���� ������ �̹� ������ ���� �ڵ�� �������

;������ ��ǥ���� �������� ���� �Ǵ� �������� �ڵ����� ���ڰ� �ٲ�� �� ���� ���� �����߽��ϴ�.

 

;�ϴ� ������ ����ĳ�� ���� ����߿� �� ���� ���縦 �ϰ� (load "TNA")�� ȣ���� �� ����ϸ� �˴ϴ�.

 

;���ʴ�� �� ������ �������ڸ�, ���� "TNA"��� ��ɾ �Է��մϴ�.

 

 

 

;�� �׸��� ���� ������ǥ�� �ִٰ� ������ ��,


 

 
;�׸�ó�� �����ϰ��� �ϴ� ���ڸ� �����մϴ�.
 
;������ ������ �Ʒ� ������ ���� �޽����� ��µ˴ϴ�.
 
;Text Read Direction [X/<Y>]  ==> ������ ������ �����ϴ�. ���� �����̹Ƿ�  "y" �Ǵ� �⺻ �����̹Ƿ� ���͸� Ĩ�ϴ�.
 
;���� �տ� �ٴ� ����<none>: ==> ���� �տ� �ٴ� ���ڸ� �Է��մϴ�. ������ �׳� ���͸� ġ�� �˴ϴ�.
 
;���� �ڿ� �ٴ� ����<none>: ==> ���� �ڿ� �ٴ� ���ڸ� �Է��մϴ�. ���� ������ ���͸� Ĩ�ϴ�.
 
;���� ����<1>: ==> ������ ���۰��� �Է��մϴ�. �⺻ 1�� �����Ǿ� �ֽ��ϴ�. 11���� �����Ѵٸ� 11�̶�� �Է��մϴ�.
 
;�ڸ���<1>: ==> ������ ������ �ڸ���, �� 100���� ������ 3, 10 ~ 99 �����̸� 2��� �Է��ϼ���.

;�������������� �׷��� ������ 001, 002, ... 099, 100 �� �ƴ� 1,2,3 ... 99, 100���� �ٲ�� �˴ϴ�.
; 
;������ ��ġ��<1>: ���� ���ں��� �����Ǵ� ��, �� 1�� �����̰� ���⼭ 2�� �Է��ϸ� 1,3,5,7,9... �̷����� �˴ϴ�.

 
 
;"A-"�� �����Ͽ� �ڸ����� 3�ڸ��� �������� 1�� �־� ������ ������ǥ �Դϴ�.

 
 

;Ȯ���� �׸��Դϴ�.


 

 
;�տ� �ٴ� ���ڸ� "CAD-" �޿� �ٴ� ���ڸ� "-LISP" �ڸ����� �⺻ 1�� ������ �ٲ� �׸��Դϴ�.
 
;�� ���ÿ� �ٸ����� �ڸ����� �⺻���� �߱⿡ 10 ������ ���� �տ� "0"�� ���� �ʾҽ��ϴ�.
 
;�ڸ��� ���鸸ŭ "0"�� ä��� �ʹٸ� ó�� ����ó�� ������ ������ �ڸ����� �Է��ϰ�
;"0"�� ä�� �ʿ䰡 ������ �ڸ��� ����� �׳� ���͸� ġ�� �˴ϴ�.
 
;ó���� �ƹ� ���ڳ� �Է��� �� �Ŀ� ǥ ���ݿ� ���߾� ����(Array)�� �ϰ� �� ������ ���� �˴ϴ�.
 
;���ڰ� ���� ��ġ ������ �ƴ� �¿�� ��߳� �ְų� ������ �ڵ� ������ ���� �ʾƵ� ��������ϴ�.
 
;�ð��� ���� ���ô� ������ "Text Read Direction [X/<Y>]" �� �޽������� "H" ��, ���ι����̶�� �Է��ϸ�
;�� ����ó�� �������� �������� ���ʴ�� ���ڸ� ������ �� �ֽ��ϴ�. 
  


; nth �Լ����� �ʱⰪ�� 0�� �ƴ� 1�� �����ϱ� ���� �Լ�
(defun @nth(&Number &VarName) (nth (1- &Number) &VarName))


; C:TNA ���α׷��� ���ԵǴ� ���ڿ� ���� �� �� ������ ��, �����ϴ� �Լ�
(defun JH_ReplaceTextSelect(&VarName1 &VarName2 &YN / !Index !Enam !EntList !Point)
  (setq !Sel (ssget (list (cons 0 "TEXT"))))
  (if (= &YN "Y")
    (setq !Dir (strcase (getstring "\nText Read Direction [-X/+X/-Y/<+Y>/]: ")))
    (progn  ; ù��° ��ü ���ÿ����� X, Y ���⸸ �ʿ��ϹǷ� �߰��� �κ�
      (setq !Dir (strcase (getstring "\nText Read Direction [X/<Y>]: ")))
		  (if (= !Dir "")
		    (setq !Dir "Y")
		  )
      (setq !Dir (strcat "+" !Dir))
    )
  )
  (if (= !Dir "")
    (setq !Dir "+Y")
  )
  (setq !Index 0)

  (repeat (sslength !Sel)
	  (setq !Ename (ssname !Sel !Index))
	  (setq !EntList (entget !Ename))
	  (if (= (substr !Dir 2 1) "X")
	    (setq !Point (cadr (assoc 10 !EntList)))
	    (setq !Point (caddr (assoc 10 !EntList)))
	  )

; ��ǥ ����Ʈ�� ����
    (setq !Point (JH_StringFill (rtos !Point 2 0) 8 "0" 1))
    (setq !PointList (acad_strlsort (append !PointList (list !Point))))
; ��ǥ�� Ename�� �ϳ��� ����Ʈ�� ����
    (setq !EnameList (append !EnameList (list (list !Point !Ename))))
    (setq !Index (1+ !Index))
  )
  
  (eval (read (strcat "(setq " (eval &VarName1) " !PointList)")))
  (eval (read (strcat "(setq " (eval &VarName2) " !EnameList)")))

  (setq !PointList nil)
  (setq !EnameList nil)
)

; �־��� ���ڿ� ���̺��� ���� ������ �������� �־��� ���ڷ� ä��� ���� �Լ�
(defun JH_StringFill(&InputString &MaxStringLength &FillString &Location / !MaxStringLength !FillString)
  (setq !MaxStringLength (- &MaxStringLength (strlen &InputString)))
  (setq !FillString "")
  (repeat !MaxStringLength
    (setq !FillString (strcat !FillString &FillString))
  )
  (if (= &Location 1)
    (setq !FillString (strcat !FillString &InputString))
    (setq !FillString (strcat &InputString !FillString))
  )
)

; C:TNA ���α׷��� ���ԵǴ� ���ڿ� ���� �� �� ������ ��, �����ϴ� �Լ�
(defun JH_ReplaceTextSelect(&VarName1 &VarName2 &YN / !Index !Enam !EntList !Point)
  (setq !Sel (ssget (list (cons 0 "TEXT"))))
  (if (= &YN "Y")
    (setq !Dir (strcase (getstring "\nText Read Direction [-X/+X/-Y/<+Y>/]: ")))
    (progn  ; ù��° ��ü ���ÿ����� X, Y ���⸸ �ʿ��ϹǷ� �߰��� �κ�
      (setq !Dir (strcase (getstring "\nText Read Direction [X/<Y>]: ")))
		  (if (= !Dir "")
		    (setq !Dir "Y")
		  )
      (setq !Dir (strcat "+" !Dir))
    )
  )
  (if (= !Dir "")
    (setq !Dir "+Y")
  )
  (setq !Index 0)

  (repeat (sslength !Sel)
	  (setq !Ename (ssname !Sel !Index))
	  (setq !EntList (entget !Ename))
	  (if (= (substr !Dir 2 1) "X")
	    (setq !Point (cadr (assoc 10 !EntList)))
	    (setq !Point (caddr (assoc 10 !EntList)))
	  )

; ��ǥ ����Ʈ�� ����
    (setq !Point (JH_StringFill (rtos !Point 2 0) 8 "0" 1))
    (setq !PointList (acad_strlsort (append !PointList (list !Point))))
; ��ǥ�� Ename�� �ϳ��� ����Ʈ�� ����
    (setq !EnameList (append !EnameList (list (list !Point !Ename))))
    (setq !Index (1+ !Index))
  )
  
  (eval (read (strcat "(setq " (eval &VarName1) " !PointList)")))
  (eval (read (strcat "(setq " (eval &VarName2) " !EnameList)")))

  (setq !PointList nil)
  (setq !EnameList nil)
)


; ���ĵ� ��ǥ ����Ʈ�� ������ ���ο� Ename List�� �ۼ��ϴ� �Լ�
(defun JH_NewEnameList(&PointList &EnameList &VarName / !Index !Count !Value)
  (setq !Index 1  !Count 1)
  (repeat (length &PointList)
    (setq !Point (@nth !Index &PointList))
    (setq !Value (@nth 1 (@nth !Count &EnameList)))
    (while (/= !Value !Point)
      (setq !Count (1+ !Count))
      (setq !Value (@nth 1 (@nth !Count &EnameList)))
    )
    (setq !NewEnameList (append !NewEnameList (list (@nth 2 (@nth !Count &EnameList)))))
    (setq !Index (1+ !Index)  !Count 1)
  )

  (eval (read (strcat "(setq " (eval &VarName) " !NewEnameList)")))
  (setq !NewEnameList nil)
)

; �̹� �ԷµǾ� �ִ� ���ڵ��� ������ ��/���� �̷���� ������
; �ڵ����� �ٲٱ� ���� ���α׷�
(defun C:TNA(/ !PrefixString !SuffixString !StartNo !Cipher !CipherStr !Step
               !Index !Entity !Value !Sel !Sel1 !Sel2 !Dir !Dir1 !Dir2)
  (command "undo" "be")
  (JH_ReplaceTextSelect "!PointList1" "!EnameList1" "N")

  (JH_NewEnameList !PointList1 !EnameList1 "!NewEnameList1")
  (if (= (substr !Dir 2 1) "Y")
    (setq !NewEnameList1 (reverse !NewEnameList1))
  )

; ���������� ������ ������ ���Ͽ� ���ڿ��� �ٲٴ� �κ�
  (setq !PrefixString (getstring "\n���� �տ� �ٴ� ����<none>: "))
  (setq !SuffixString (getstring "\n���� �ڿ� �ٴ� ����<none>: "))
  (setq !StartNo (getint "\n���� ����<1>: "))
  (if (not !StartNo)
    (setq !StartNo 1)
  )

  (setq !Cipher (getint "\�ڸ���<1>: "))
  (if (not !Cipher)
    (setq !Cipher 1)
  )

  (setq !Step (getint "\n������ ��ġ��<1>: "))
  (if (not !Step)
    (setq !Step 1)
  )

  (setq !Index 1)
  (repeat (length !NewEnameList1)
    (if (and (>= !Index 1) (< !Index 10))
    	(setq !AddZero (- !Cipher 1)  !CipherStr "")
    )
    (if (and (>= !Index 10) (< !Index 100))
    	(setq !AddZero (- !Cipher 2)  !CipherStr "")
    )
    (if (and (>= !Index 100) (< !Index 1000))
    	(setq !AddZero (- !Cipher 3)  !CipherStr "")
    )

    (setq !CipherStr "")
    (repeat !AddZero
      (setq !CipherStr (strcat !CipherStr "0"))
    )

    (setq !Entity (entget (@nth !Index !NewEnameList1)))
    (setq !Value (strcat !PrefixString !CipherStr (itoa !StartNo) !SuffixString))
    (entmod (subst (cons 1 !Value) (assoc 1 !Entity) !Entity))
    (setq !Index (1+ !Index)  !StartNo (+ !StartNo !Step))
  )

  (command "undo" "e")
  (princ)
)

