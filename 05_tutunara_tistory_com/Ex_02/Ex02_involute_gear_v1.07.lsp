;Gear �׸��� LISP Ver 1.07

;��ó : http://cyg.kr
;������ : ������õ(e58000)
;������ : 2009�� 9�� 28��


;�ֿ亯�� ���
;Pangle : �з°�
;Ref : �̳��κп� ���� ������
;Rif : ��(Rack) ���� �𼭸� ���� ������
;R : ����� �⺻�� ������(��ġ��)
;Rmax : ġ���� �ִ� ������(Rmax���� R�� ���� ���(addendum)�� �˴ϴ�.[�ַ� ��� ������ �մϴ�.])
;Rmin : ġ���� �ּ� ������(R���� Rmin�� ���� �𵧴�(dedendum)�� �˴ϴ�.[�ַ� ��� ���� 10%�� ���� ���� ����մϴ�.])

;module : ����ڷκ��� ��� ���� �Է� �޽��ϴ�.
;teeth : ����ڷκ��� ����� �ռ��� �Է� �޽��ϴ�.
;center : ����ڷκ��� ����� �߽����� �Է� �޽��ϴ�.



; gear������� ����� ������ �Է¹޾� �� �׸��ϴ�.
(DEFUN C:Gear (/ keyin DIA cmdecho osmode clayer step buffer B0 B1 X Y X0 Y0 A R Ref Rif counter Ad De Ar Cs Ce Cp Ss P P0 Pm L0 L1 L2 L3)

  (defun *error* (msg)
    (princ "\n; ����: ") (princ msg)
    (if (/= cmdecho nil) (setvar "cmdecho" cmdecho))
    (if (/= osmode nil) (setvar "osmode" osmode))
    (if (/= clayer nil) (setvar "clayer" clayer))
    (princ)
  ) ;defun



; AutoCAD�� ���� ���� �����մϴ�.
  (setq cmdecho (getvar "cmdecho"));command echo���� �����մϴ�.
  (setq clayer (getvar "clayer"));���� ���̾ �����մϴ�.


; LISP ������ ��Ȱ�� �ϱ����� AutoCAD�� ȯ�溯���� �����մϴ�.
  (setvar "cmdecho" 0);LISP ���� ��Ȳ�� ������ �ʵ��� �����մϴ�.





  (graphscr) ;LISP�� �׸��� ��忡�� �����մϴ�.



;�ʱ� ���� ���� ���մϴ�.
  (if (= GearType nil) (setq GearType "Involute general"));ġ���� ���� �� ��� ������ �����մϴ�.
  
  (if (= module nil) (setq module 2.0));����� �ʱ� ��
  
  (if (= outside nil) (setq outside 0.0));�̳� ���� ������ �ʱ� ��
  (if (= inside nil) (setq inside 0.5));�̻Ѹ� ���� ������ �ʱ� ��
  
  (if (= Pangle nil) (setq Pangle 20.0));�з°�
  (if (= Xshift nil) (setq Xshift 0.0));���� �Ÿ�

  (if (= Backlash nil) (setq Backlash 0.01));�鷡��

  (if (= ARatio nil) (setq ARatio 1.0));�⺻������ �̳����� �Ÿ�(��⿡ ���� ����) �⺻ ��
  (if (= DRatio nil) (setq DRatio 1.25));�⺻������ �̻Ѹ����� �Ÿ�(��⿡ ���� ����) �⺻ ��
  
  (if (= teeth nil) (setq teeth 19));��� �ռ� �ʱ� ��
  
  (if (= center nil) (setq center (list 0.0 0.0 0.0)));��� �߽��� �ʱ� ��

  (if (= series nil) (setq series 10));ġ���� ���е� �ʱ� ��
;�ڿ����� �����ϸ鼭 ��� �׸� �� â������ ������ 6mm�� �� series���� 10�̸� �� 2/1000mm ���е��� �����ϴ�.






; ġ�� ��� �׸��� ���� �ʿ��� ������ �Է� �޽��ϴ�.

; ��� ���� �Է� �޽��ϴ�.
  (setq DIA "Blank")
  (while (/= keyin "Keyin exit")
    (cond
      ((= DIA "M") (progn (princ "\nġ�� 1���� ���� ������ ���� - ���(Module) <") (princ module)))
      ((= DIA "P") (progn (princ "\n�з°�(Pressure angle) Degrees <") (princ Pangle)))
      ((= DIA "O") (progn (princ "\nġ���� �ٱ���(Outside) ���� ������ <") (princ outside)))
      ((= DIA "I") (progn (princ "\n��(Rack) ���� ���� �𼭸� ���� ������ <") (princ inside)))
      ((= DIA "X") (progn (princ "\n��(Rack) ������ ����(modification) �Ÿ� <") (princ Xshift)))
      ((= DIA "S") (progn (princ "\n�κ���Ʈ ��� ȣ�� ����(Subdivision)�� ȣ�� �� <") (princ series)))
      ((= DIA "A") (progn (princ "\nġ���� ������ ��ġ������ �Ÿ�(Addendum) ���� <") (princ ARatio)))
      ((= DIA "D") (progn (princ "\nġ���� �Ѹ����� ��ġ������ �Ÿ�(Dedendum) ���� <") (princ DRatio)))
      ((= DIA "B") (progn (princ "\n�ѽ��� �� �¹����� �� ġ�� ���� �Ÿ�(Backlash) <") (princ Backlash)))
      (T (progn
	   (princ "\n��� �ռ�(������ �������) �Ǵ� [R/M/P/A/D/X/B/O/I/S] <")
	   (princ teeth)
      ));
    );End cond

    (princ ">:")
    (if (= keyin "next")
      (progn (setq buffer B0) (princ buffer) (setq keyin "keyin"))
      (progn
	(setq buffer (strcase (getstring)))
	(if (and (> (strlen buffer) 1) (> (ascii (substr buffer 1 1)) 64) (< (ascii (substr buffer 1 1)) 91))
	  (progn (setq B0 (substr buffer 2 (1- (strlen buffer)))) (setq buffer (substr buffer 1 1)) (setq keyin "next"))
	)
        (if (and (> (strlen buffer) 1) (> (ascii (substr buffer (strlen buffer) 1)) 64) (< (ascii (substr buffer (strlen buffer) 1)) 91))
	  (progn (setq B0 (substr buffer 1 (1- (strlen buffer)))) (setq buffer (substr buffer (strlen buffer) 1)) (setq keyin "next"))
	)
	(if (and (> (strlen buffer) 2) (> (ascii (substr buffer 1 1)) 127))
	  (progn (setq B0 (substr buffer 3 (- (strlen buffer) 2))) (setq buffer (substr buffer 1 2)) (setq keyin "next"))
	)
        (if (and (> (strlen buffer) 2) (> (ascii (substr buffer (strlen buffer) 1)) 127))
	  (progn (setq B0 (substr buffer 1 (- (strlen buffer) 2))) (setq buffer (substr buffer (1- (strlen buffer)) 2)) (setq keyin "next"))
	)
      )
    )

    (cond
      ((= buffer "")
       (cond
	 ((= DIA "Blank") (setq keyin "Keyin exit"))
	 (T (setq DIA "Blank"))
       );End cond
      );
      ((= DIA "X")
       (progn
	 (setq Xshift (atof buffer))
	 (setq DIA "Blank")
       );End progn
      );
      ((or (= buffer "M") (= buffer "��")) (setq DIA "M"))
      ((or (= buffer "P") (= buffer "��")) (setq DIA "P"))
      ((or (= buffer "O") (= buffer "��")) (setq DIA "O"))
      ((or (= buffer "I") (= buffer "��")) (setq DIA "I"))
      ((or (= buffer "X") (= buffer "��")) (setq DIA "X"))
      ((or (= buffer "S") (= buffer "��")) (setq DIA "S"))
      ((or (= buffer "A") (= buffer "��")) (setq DIA "A"))
      ((or (= buffer "D") (= buffer "��")) (setq DIA "D"))
      ((or (= buffer "B") (= buffer "��")) (setq DIA "B"))
      ((or (= buffer "R") (= buffer "��"))
       (princ "\n���") (princ module)
       (princ ",�з°�") (princ Pangle)
       (princ ",���") (princ (* 100 ARatio))
       (princ "%,�𵧴�") (princ (* 100 DRatio))
       (princ "%,����") (princ Xshift)
       (princ ",�鷡��") (princ Backlash)
       (princ ",�̳�����") (princ outside)
       (princ ",�����ձ۱�") (princ inside)
       (princ ",���") (princ series)
      )
      ((or (= buffer "0") (= buffer "0.") (= buffer "0.0") (= buffer ".0"))
       (cond
	 ((= DIA "O") (progn (setq outside 0.0) (setq DIA "Blank")))
	 ((= DIA "I") (progn (setq inside 0.0) (setq DIA "Blank")))
	 ((= DIA "B") (progn (setq Backlash 0.0) (setq DIA "Blank")))
	 (T (princ "0�� ��ȿ���� �ʽ��ϴ�."))
       );End cond
      );
      ((> (atof buffer) 0.0)
       (progn
	 (if (and (= (atoi buffer) 0) (or (= DIA "S") (= DIA "Blank")))
	   (princ "1���� ���� ���� ��ȿ���� �ʽ��ϴ�.")
	   (progn
	     (cond
	       ((= DIA "M") (setq module (atof buffer)))
	       ((= DIA "O") (setq outside (atof buffer)))
	       ((= DIA "I") (setq inside (atof buffer)))
	       ((= DIA "P") (setq Pangle (atof buffer)))
	       ((= DIA "S") (setq series (atoi buffer)))
	       ((= DIA "A") (setq ARatio (atof buffer)))
	       ((= DIA "D") (setq DRatio (atof buffer)))
	       ((= DIA "B") (setq Backlash (atof buffer)))
	       (T (progn
	            (setq teeth (atoi buffer))
		    (setq keyin "Keyin exit")
               )  );
             );End cond
	     (setq DIA "Blank")
	   );End progn
	 );End if
       );End progn
      );
      ((< (atof buffer) 0.0)
       (progn
	 (if (and (= (atoi buffer) 0) (= DIA "Blank"))
	   (princ "-1���� ���� ���� ��ȿ���� �ʽ��ϴ�.")
	   (progn
	     (cond
	       ((= DIA "A") (progn (setq ARatio (atof buffer)) (setq DIA "Blank")))
	       ((= DIA "D") (progn (setq DRatio (atof buffer)) (setq DIA "Blank")))
	       ((= DIA "B") (progn (setq Backlash (atof buffer)) (setq DIA "Blank")))
	       ((= DIA "Blank") (progn (setq teeth (atoi buffer)) (setq keyin "Keyin exit")))
	       (T (princ "������ ��ȿ���� �ʽ��ϴ�."))
	     );End cond
	   );End progn
	 );End if
       );End cond
      );
      (T (progn (princ " �߸� �Է��Ͽ����ϴ�.") (setq keyin "error")))
    );End cond
  );End while



; ����� �߽����� �Է� �޽��ϴ�.
  (princ "\n��� �߽���<") (princ center) (princ ">:") (setq buffer (getpoint))
  (if (/= buffer nil) (setq center buffer));End if






; �Է� ���� �̿��ؼ� �������� �����մϴ�.
  (setq A (/ (* pi Pangle) 180))

  (setq X0 (car center)); ��� �߽��� ��ǥ�� �м��մϴ�.
  (setq Y0 (cadr center)); ��� �߽��� ��ǥ�� �м��մϴ�.



  (cond
    ((< teeth 0)
     (progn;���� ��� �� �� �������� ������ �ٲ��ִ� �κ��Դϴ�.
       (setq Ref inside); ���� ����̹Ƿ� Ref�� Rif�� �ٲ�ϴ�.
       (setq Rif outside); ���� ����̹Ƿ� Ref�� Rif�� �ٲ�ϴ�.

       (setq R (* 0.5 module (abs teeth)));��ġ���� �������� ���մϴ�.
       (setq Ad (* DRatio module));���� ����̹Ƿ� ���(Addendum)�� �𵧴�(Dedendum)�� �ٲ�ϴ�.
       (setq De (* ARatio module));���� ����̹Ƿ� ���(Addendum)�� �𵧴�(Dedendum)�� �ٲ�ϴ�.
     );End progn
    );
    (T
     (progn;�κ���Ʈ ��� �Ϻκ��� ����Ͽ� �� �׸� �� �����ϴ� �����Դϴ�.
       (setq Ref outside);�̳��� ���� � ������ ���� ���մϴ�.
       (setq Rif inside);�̻Ѹ��� ���� � ������ ���� ���մϴ�.

       (setq R (* 0.5 module (abs teeth)));�⺻���� �������� ���մϴ�.
       (setq Ad (* ARatio module));���(Addendum)�� ���մϴ�.
       (setq De (* DRatio module));�𵧴�(Dedendum)�� ���մϴ�.
     );End progn
    );
  );End cond









  (setq osmode (getvar "osmode")) (setvar "osmode" 0);��ü �������� �����ϰ� ��ü������ ���ϴ�.


;�׷����� ���� Ss�� �����մϴ�.
  (setq Ss (ssadd))
  
;Construction ���̾ �������� �׸��ϴ�.
  (command "layer" "m" "Construction" "c" "6" "" "")

  (setq buffer (+ R Ad Xshift (* 0.5 module)))
  (command "line" (list (- X0 buffer) Y0) (list (+ X0 buffer) Y0) "") (setq Ss (ssadd (entlast) Ss));X�� �߽ɼ�
  (command "line" (list X0 (- Y0 buffer)) (list X0 (+ Y0 buffer)) "") (setq Ss (ssadd (entlast) Ss));Y�� �߽ɼ�

  (command "circle" center R) (setq Ss (ssadd (entlast) Ss)); ��ġ��
  (if (/= Xshift 0) (progn (command "circle" center (+ R Xshift)) (setq Ss (ssadd (entlast) Ss))));������

  (command "change" Ss "" "P" "C" "bylayer" "LT" "bylayer" "")







;�̻Ѹ� ġ���� ���� ��ǥ�� ���մϴ�.
  (setq X (+ R Xshift (* -1 De) (* Rif (- 1 (sin A)))))
  (setq Y (* -1 (+ (/ (* (- De Rif Xshift) (cos A)) (sin A)) (* Rif (cos A)))))

  (setq p (list (+ X0 X) (+ Y0 Y)))
  (setq Pm (polar P (* -1 A) (+ Ad De)))

  (setq Cs (atan (* -1 (+ (- De Xshift) (* Rif (- (sin A) 1))) (cos A))
		 (* (- (+ R Xshift (* Rif (- 1 (sin A)))) De) (sin A))))
  (setq Ce (- Cs (/ (+ Y (* Rif (cos A))) R)))

;ġ���� �β��� ����ϴ� ���� ���մϴ�.
  (setq buffer (+ (/ (* pi module) 4)
		  (/ (* Xshift (sin A)) (cos A))
		  (/ Rif (cos A))
		  (/ (- De Xshift Rif) (* (sin A) (cos A)))))
  (if (= DIA "T")
    (setq Ar (+ (/ (* -1 buffer) R) (/ (* -0.5 Backlash) (* R (cos A)))))
    (setq Ar (- (/ (* -1 buffer) R) (/ (* -0.5 Backlash) (* R (cos A)))))
  )










  (setq Ss (ssadd))

;Involute Gear ���̾ Involute ġ���� �׸��ϴ�.
  (command "layer" "m" "Involute Gear" "c" "3" "" "")


;���� arc�� �������� ���ϱ� ���� ������ �׸��ϴ�.
  (command "line" Pm P "") (setq L0 (entlast))

;ġ���� ���� trocoid��� �׸��ϴ�.
  (if (= (- De Xshift Rif) 0.0)
    (if (/= Rif 0.0)
      (progn
	(setq P (list (- (+ X0 R) Rif) Y0))
	(command "arc" "" P) (setq Ss (ssadd (entlast) Ss));�ռ� �׷��� ���� �����Ͽ� ȣ�� �׸��ϴ�.
      );End progn
    );End if
    (progn
      (setq counter (1+ (* 2 series)))
      (while (< series counter)
	(setq counter (1- counter))
	(setq step (/ (* (+ (* 2 series) counter) (- (1+ (* 2 series)) counter) (- Ce Cs))
		      (* 3 series (1+ series))));������ ������Ŵ

	(setq buffer (atan (- (/ (cos A) (sin A)) (/ (* R step) (- De Xshift Rif)))))

	(setq X (+ R Xshift (* -1 De) (* Rif (- 1 (cos buffer)))));������ ���� �� ���� X��ǥ
	(setq Y (- (* R step) (/ (* (- De Xshift Rif) (cos A)) (sin A)) (* Rif (sin buffer)))) ;������ ���� �� ���� Y��ǥ

	(setq buffer (sqrt (+ (expt X 2) (expt Y 2))))

	(setq p (polar center (- (atan Y X) step) buffer))

	(command "arc" "" P) (setq Ss (ssadd (entlast) Ss));�ռ� �׷��� ���� �����Ͽ� ȣ�� �׸��ϴ�.
      );End while
    );End progn
  );End if

  (setq Pm P)

  (setq p (polar center (+ Ar (/ pi (abs teeth))) (- (+ R Xshift) De)))

  (command "arc" Pm "c" center P) (setq L1 (entlast)) (setq Ss (ssadd (entlast) Ss))

  (command "erase" L0 "");���� arc�� �������� ���ϱ� ���� ������ ����ϴ�.




;�̳� ġ���� ���� ��ǥ�� ���մϴ�.
  (if (> (abs Cs) A)
    (progn
      (command "pedit" L1 "y" "j" Ss "" "") (setq L3 (entlast)) (setq Ss (ssadd))
      (setq X (* R (cos A) (cos A)))
      (setq Y (* -1 R (sin A) (cos A)))
      (setq Cp (+ (/ (- De Rif Xshift) (* R (cos A) (sin A))) (/ Rif (* R (cos A)))
		  (/ (* -1 (sin A)) (cos A))))
      (setq buffer (sqrt (+ (expt X 2) (expt Y 2))))
      (setq B0 (/ (+ (* R (cos A) (cos A)) (* -1 R) (* -1 Xshift) De (* -1 Rif (- 1 (sin A))))
		  (* R (cos A) (sin A))))
      (setq P (polar center (- (atan Y X) B0) buffer))
      (setq Pm (polar center (- (atan Y X) B0) (- buffer Ad de)))
    );End progn
    (progn
      (setq Cp 0.0)
      (setq X (+ R Xshift (* -1 De) (* Rif (- 1 (sin A)))))
      (setq Y (* -1 (+ (/ (* (- De Rif Xshift) (cos A)) (sin A)) (* Rif (cos A)))))
      (setq P (list (+ X0 X) (+ Y0 Y)))
      (setq Pm (polar P (- pi A) (+ Ad De)))
    );End progn
  );End if


  (setq buffer (+ (* -1 R (sin A) (cos A))
		  (* (cos A) (sqrt (+ (expt (+ Ad Xshift) 2) (* 2 R (+ Ad Xshift)) (expt (* R (sin A)) 2))))))
  (setq Ce (+ (/ (- buffer (* -1 (+ (/ (* (- De Rif Xshift) (cos A)) (sin A)) (* Rif (cos A))))) (* R (expt (cos A) 2))) Cs))


;���� arc�� �������� ���ϱ� ���� ������ �׸��ϴ�.
  (command "line" Pm P "") (setq L0 (entlast))


;ġ���� �ٱ��� Involute��� �׸��ϴ�.
  (setq counter (1- series))
  (while (> (* 2 series) counter)
    (setq counter (1+ counter))
    (setq step (/ (* (+ series counter) (1+ (- counter series)) (- Ce Cs Cp)) (* 3 series (1+ series))));������ ������Ŵ

    (setq X (+ (+ R Xshift (* -1 De) (* Rif (- 1 (sin A)))) (* (* R (cos A) (sin A)) (+ step Cp))))
    (setq Y (+ (+ (/ (* -1 (- De Xshift Rif) (cos A)) (sin A)) (* -1 Rif (cos A))) (* (* R (expt (cos A) 2)) (+ step Cp))))

    (setq buffer (sqrt (+ (expt X 2) (expt Y 2))))

    (setq P0 P)
    (setq p (polar center (- (atan Y X) (+ step Cp)) buffer))

    (command "arc" "" P) (setq Ss (ssadd (entlast) Ss));�ռ� �׷��� ���� �����Ͽ� ȣ�� �׸��ϴ�.

    (setq buffer (polar p (angle p (cdr (assoc 10 (entget (entlast))))) Ref))
    (if (< (+ R Xshift Ad) (+ (distance center buffer) Ref));������ ������ ��� �׸� �׸��ϴ�.
      (setq counter (1+ (* 2 series)))
    );End if
  );End while

  (if (= 0.0 Ref);���Ⱑ ������ ���⸦ �մϴ�.
    (progn
      (setq P0 P)
      (setq p (polar center Ar (+ R Xshift Ad)))
      (command "arc" P "c" center P0) (setq L1 (entlast)) (setq Ss (ssadd (entlast) Ss));����� �ִ� �ܰ� ���� �׸��ϴ�.
    );End progn
    (progn
      (setq L2 (entlast))
      (setq p (polar center Ar (+ R Xshift Ad)))
      (setq Pm (polar center (angle center P0) (+ R Xshift Ad))) 
      (command "arc" P "c" center Pm) (setq L1 (entlast)) (setq Ss (ssadd (entlast) Ss));����� �ִ� �ܰ� ���� �׸��ϴ�.
      (command "fillet" "r" Ref);���� ������ ���� �����մϴ�.
      (command "fillet" (list L2 P0) (list L1 P)) (setq Ss (ssadd (entlast) Ss));�����մϴ�.
    );End progn
  );End if

  (command "erase" L0 "");���� arc�� �������� ���ϱ� ���� ������ ����ϴ�.








; �׷��� ġ���� �Ϻκ��� �迭�Ͽ� ġ���� �ϼ��մϴ�.
  (command "change" Ss "" "P" "C" "bylayer" "LT" "bylayer" "")
  (command "pedit" L1 "y" "j" Ss "" "") (setq L0 (entlast));ġ�� ��� ��� �����մϴ�.

  (setq Ss (ssadd))

  (command "mirror" L0 "" center P "") (setq L1 (entlast));ġ���� ��Ī���� �ݴ��� ��� �׸��ϴ�.
  (command "pedit" L0 "j" L1 "" "") (setq L0 (entlast)) (setq Ss (ssadd L0 Ss));ġ�� 1���� �ϼ��մϴ�.
  (if (> (abs Cs) A)
    (progn
      (setq Ss (ssadd L3 Ss))
      (command "mirror" L3 "" center P "") (setq L2 (entlast)) (setq Ss (ssadd L2 Ss));ġ���� ��Ī���� �ݴ��� ��� �׸��ϴ�.
    );End progn
  );Enf if
  (command "rotate" Ss "" center (/ (* -1 Ar 180) pi))

  (if (/= (abs teeth) 1); ġ���� 1���̸� �Ʒ� ����� �������� �ʽ��ϴ�.
    (progn
      (command "array" Ss "" "p" center (abs teeth) 360 "");ġ�� 1���� �ռ���ŭ �迭�մϴ�.
      (setq L1 L0)
      (setq counter 1)
      (while (> (abs teeth) counter)
	(setq counter (1+ counter))
	(setq L1 (entnext L1))
	(setq Ss (ssadd L1 Ss))
      );End while
      (command "pedit" L0 "j" Ss "" "")
    );End progn
  );End if




; AutoCAD�� ���� ���� LISP ���� ������ �ǵ����ϴ�.
  (setvar "clayer" clayer)
  (setvar "osmode" osmode)
  (setvar "cmdecho" cmdecho)

  (princ)
)



;�ѱ۷� gear�� �Է� �� ��� �� ���α׷��� ���� �մϴ�.
(DEFUN C:�������� ()
  (C:gear)
)

;�ѱ۷� "���"�� �Է� �� ��� �� ���α׷��� ���� �մϴ�.
(DEFUN C:��� ()
  (C:gear)
)
