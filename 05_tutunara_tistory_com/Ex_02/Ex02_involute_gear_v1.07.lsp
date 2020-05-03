;Gear 그리기 LISP Ver 1.07

;출처 : http://cyg.kr
;제작자 : 오만팔천(e58000)
;제작일 : 2009년 9월 28일


;주요변수 목록
;Pangle : 압력각
;Ref : 이끝부분에 모깎기 반지름
;Rif : 랙(Rack) 공구 모서리 모깎기 반지름
;R : 기어의 기본원 반지름(피치원)
;Rmax : 치형의 최대 반지름(Rmax에서 R을 빼면 어덴덤(addendum)이 됩니다.[주로 모듈 값으로 합니다.])
;Rmin : 치형의 최소 반지름(R에서 Rmin을 빼면 디덴덤(dedendum)이 됩니다.[주로 모듈 값의 10%를 더한 값을 사용합니다.])

;module : 사용자로부터 모듈 값을 입력 받습니다.
;teeth : 사용자로부터 기어의 잇수를 입력 받습니다.
;center : 사용자로부터 기어의 중심점을 입력 받습니다.



; gear명령으로 기어의 정보를 입력받아 기어를 그립니다.
(DEFUN C:Gear (/ keyin DIA cmdecho osmode clayer step buffer B0 B1 X Y X0 Y0 A R Ref Rif counter Ad De Ar Cs Ce Cp Ss P P0 Pm L0 L1 L2 L3)

  (defun *error* (msg)
    (princ "\n; 오류: ") (princ msg)
    (if (/= cmdecho nil) (setvar "cmdecho" cmdecho))
    (if (/= osmode nil) (setvar "osmode" osmode))
    (if (/= clayer nil) (setvar "clayer" clayer))
    (princ)
  ) ;defun



; AutoCAD의 설정 값을 저장합니다.
  (setq cmdecho (getvar "cmdecho"));command echo값을 저장합니다.
  (setq clayer (getvar "clayer"));현재 레이어를 저장합니다.


; LISP 실행을 원활히 하기위해 AutoCAD의 환경변수를 설정합니다.
  (setvar "cmdecho" 0);LISP 실행 상황을 보이지 않도록 설정합니다.





  (graphscr) ;LISP를 그리기 모드에서 실행합니다.



;초기 변수 값을 정합니다.
  (if (= GearType nil) (setq GearType "Involute general"));치형의 종류 및 기어 종류를 선택합니다.
  
  (if (= module nil) (setq module 2.0));모듈의 초기 값
  
  (if (= outside nil) (setq outside 0.0));이끝 모깎기 반지름 초기 값
  (if (= inside nil) (setq inside 0.5));이뿌리 모깎기 반지름 초기 값
  
  (if (= Pangle nil) (setq Pangle 20.0));압력각
  (if (= Xshift nil) (setq Xshift 0.0));전위 거리

  (if (= Backlash nil) (setq Backlash 0.01));백래시

  (if (= ARatio nil) (setq ARatio 1.0));기본원에서 이끝까지 거리(모듈에 대한 비율) 기본 값
  (if (= DRatio nil) (setq DRatio 1.25));기본원에서 이뿌리까지 거리(모듈에 대한 비율) 기본 값
  
  (if (= teeth nil) (setq teeth 19));기어 잇수 초기 값
  
  (if (= center nil) (setq center (list 0.0 0.0 0.0)));기어 중심점 초기 값

  (if (= series nil) (setq series 10));치형의 정밀도 초기 값
;자연수로 증가하면서 곡선을 그릴 때 창성원의 지름이 6mm일 때 series값이 10이면 약 2/1000mm 정밀도를 갖습니다.






; 치형 곡선을 그리기 위해 필요한 정보를 입력 받습니다.

; 모듈 값을 입력 받습니다.
  (setq DIA "Blank")
  (while (/= keyin "Keyin exit")
    (cond
      ((= DIA "M") (progn (princ "\n치형 1개에 대한 지름의 길이 - 모듈(Module) <") (princ module)))
      ((= DIA "P") (progn (princ "\n압력각(Pressure angle) Degrees <") (princ Pangle)))
      ((= DIA "O") (progn (princ "\n치형의 바깥쪽(Outside) 모깎기 반지름 <") (princ outside)))
      ((= DIA "I") (progn (princ "\n랙(Rack) 공구 날끝 모서리 모깎기 반지름 <") (princ inside)))
      ((= DIA "X") (progn (princ "\n랙(Rack) 공구의 전위(modification) 거리 <") (princ Xshift)))
      ((= DIA "S") (progn (princ "\n인볼류트 곡선을 호로 세분(Subdivision)한 호의 수 <") (princ series)))
      ((= DIA "A") (progn (princ "\n치형의 끝에서 피치원까지 거리(Addendum) 비율 <") (princ ARatio)))
      ((= DIA "D") (progn (princ "\n치형의 뿌리에서 피치원까지 거리(Dedendum) 비율 <") (princ DRatio)))
      ((= DIA "B") (progn (princ "\n한쌍의 기어를 맞물렸을 때 치면 사이 거리(Backlash) <") (princ Backlash)))
      (T (progn
	   (princ "\n기어 잇수(음수는 내륜기어) 또는 [R/M/P/A/D/X/B/O/I/S] <")
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
      ((or (= buffer "M") (= buffer "ㅡ")) (setq DIA "M"))
      ((or (= buffer "P") (= buffer "ㅔ")) (setq DIA "P"))
      ((or (= buffer "O") (= buffer "ㅐ")) (setq DIA "O"))
      ((or (= buffer "I") (= buffer "ㅑ")) (setq DIA "I"))
      ((or (= buffer "X") (= buffer "ㅌ")) (setq DIA "X"))
      ((or (= buffer "S") (= buffer "ㄴ")) (setq DIA "S"))
      ((or (= buffer "A") (= buffer "ㅁ")) (setq DIA "A"))
      ((or (= buffer "D") (= buffer "ㅇ")) (setq DIA "D"))
      ((or (= buffer "B") (= buffer "ㅠ")) (setq DIA "B"))
      ((or (= buffer "R") (= buffer "ㄱ"))
       (princ "\n모듈") (princ module)
       (princ ",압력각") (princ Pangle)
       (princ ",어덴덤") (princ (* 100 ARatio))
       (princ "%,디덴덤") (princ (* 100 DRatio))
       (princ "%,전위") (princ Xshift)
       (princ ",백래시") (princ Backlash)
       (princ ",이끝모깎기") (princ outside)
       (princ ",공구둥글기") (princ inside)
       (princ ",곡선수") (princ series)
      )
      ((or (= buffer "0") (= buffer "0.") (= buffer "0.0") (= buffer ".0"))
       (cond
	 ((= DIA "O") (progn (setq outside 0.0) (setq DIA "Blank")))
	 ((= DIA "I") (progn (setq inside 0.0) (setq DIA "Blank")))
	 ((= DIA "B") (progn (setq Backlash 0.0) (setq DIA "Blank")))
	 (T (princ "0은 유효하지 않습니다."))
       );End cond
      );
      ((> (atof buffer) 0.0)
       (progn
	 (if (and (= (atoi buffer) 0) (or (= DIA "S") (= DIA "Blank")))
	   (princ "1보다 작은 수는 유효하지 않습니다.")
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
	   (princ "-1보다 작은 수는 유효하지 않습니다.")
	   (progn
	     (cond
	       ((= DIA "A") (progn (setq ARatio (atof buffer)) (setq DIA "Blank")))
	       ((= DIA "D") (progn (setq DRatio (atof buffer)) (setq DIA "Blank")))
	       ((= DIA "B") (progn (setq Backlash (atof buffer)) (setq DIA "Blank")))
	       ((= DIA "Blank") (progn (setq teeth (atoi buffer)) (setq keyin "Keyin exit")))
	       (T (princ "음수는 유효하지 않습니다."))
	     );End cond
	   );End progn
	 );End if
       );End cond
      );
      (T (progn (princ " 잘못 입력하였습니다.") (setq keyin "error")))
    );End cond
  );End while



; 기어의 중심점을 입력 받습니다.
  (princ "\n기어 중심점<") (princ center) (princ ">:") (setq buffer (getpoint))
  (if (/= buffer nil) (setq center buffer));End if






; 입력 값을 이용해서 변수들을 설정합니다.
  (setq A (/ (* pi Pangle) 180))

  (setq X0 (car center)); 기어 중심점 좌표를 분석합니다.
  (setq Y0 (cadr center)); 기어 중심점 좌표를 분석합니다.



  (cond
    ((< teeth 0)
     (progn;내륜 기어 일 때 변수들을 적절히 바꿔주는 부분입니다.
       (setq Ref inside); 내륜 기어이므로 Ref와 Rif를 바뀝니다.
       (setq Rif outside); 내륜 기어이므로 Ref와 Rif를 바뀝니다.

       (setq R (* 0.5 module (abs teeth)));피치원의 반지름을 구합니다.
       (setq Ad (* DRatio module));내륜 기어이므로 어덴덤(Addendum)과 디덴덤(Dedendum)이 바뀝니다.
       (setq De (* ARatio module));내륜 기어이므로 어덴덤(Addendum)과 디덴덤(Dedendum)이 바뀝니다.
     );End progn
    );
    (T
     (progn;인볼류트 곡선의 일부분을 사용하여 기어를 그릴 때 설정하는 변수입니다.
       (setq Ref outside);이끝의 모깎기 곡선 반지름 값을 정합니다.
       (setq Rif inside);이뿌리의 모깎기 곡선 반지름 값을 정합니다.

       (setq R (* 0.5 module (abs teeth)));기본원의 반지름을 구합니다.
       (setq Ad (* ARatio module));어덴덤(Addendum)을 구합니다.
       (setq De (* DRatio module));디덴덤(Dedendum)을 구합니다.
     );End progn
    );
  );End cond









  (setq osmode (getvar "osmode")) (setvar "osmode" 0);객체 스냅값을 저장하고 객체스냅을 끕니다.


;그려지는 선을 Ss에 저장합니다.
  (setq Ss (ssadd))
  
;Construction 레이어에 보조선을 그립니다.
  (command "layer" "m" "Construction" "c" "6" "" "")

  (setq buffer (+ R Ad Xshift (* 0.5 module)))
  (command "line" (list (- X0 buffer) Y0) (list (+ X0 buffer) Y0) "") (setq Ss (ssadd (entlast) Ss));X축 중심선
  (command "line" (list X0 (- Y0 buffer)) (list X0 (+ Y0 buffer)) "") (setq Ss (ssadd (entlast) Ss));Y축 중심선

  (command "circle" center R) (setq Ss (ssadd (entlast) Ss)); 피치원
  (if (/= Xshift 0) (progn (command "circle" center (+ R Xshift)) (setq Ss (ssadd (entlast) Ss))));전위원

  (command "change" Ss "" "P" "C" "bylayer" "LT" "bylayer" "")







;이뿌리 치형의 시작 좌표를 구합니다.
  (setq X (+ R Xshift (* -1 De) (* Rif (- 1 (sin A)))))
  (setq Y (* -1 (+ (/ (* (- De Rif Xshift) (cos A)) (sin A)) (* Rif (cos A)))))

  (setq p (list (+ X0 X) (+ Y0 Y)))
  (setq Pm (polar P (* -1 A) (+ Ad De)))

  (setq Cs (atan (* -1 (+ (- De Xshift) (* Rif (- (sin A) 1))) (cos A))
		 (* (- (+ R Xshift (* Rif (- 1 (sin A)))) De) (sin A))))
  (setq Ce (- Cs (/ (+ Y (* Rif (cos A))) R)))

;치형의 두께를 결경하는 각을 구합니다.
  (setq buffer (+ (/ (* pi module) 4)
		  (/ (* Xshift (sin A)) (cos A))
		  (/ Rif (cos A))
		  (/ (- De Xshift Rif) (* (sin A) (cos A)))))
  (if (= DIA "T")
    (setq Ar (+ (/ (* -1 buffer) R) (/ (* -0.5 Backlash) (* R (cos A)))))
    (setq Ar (- (/ (* -1 buffer) R) (/ (* -0.5 Backlash) (* R (cos A)))))
  )










  (setq Ss (ssadd))

;Involute Gear 레이어에 Involute 치형을 그립니다.
  (command "layer" "m" "Involute Gear" "c" "3" "" "")


;연속 arc의 시작점을 정하기 위한 라인을 그립니다.
  (command "line" Pm P "") (setq L0 (entlast))

;치형의 안쪽 trocoid곡선을 그립니다.
  (if (= (- De Xshift Rif) 0.0)
    (if (/= Rif 0.0)
      (progn
	(setq P (list (- (+ X0 R) Rif) Y0))
	(command "arc" "" P) (setq Ss (ssadd (entlast) Ss));앞서 그려진 선에 접선하여 호를 그립니다.
      );End progn
    );End if
    (progn
      (setq counter (1+ (* 2 series)))
      (while (< series counter)
	(setq counter (1- counter))
	(setq step (/ (* (+ (* 2 series) counter) (- (1+ (* 2 series)) counter) (- Ce Cs))
		      (* 3 series (1+ series))));각도를 증가시킴

	(setq buffer (atan (- (/ (cos A) (sin A)) (/ (* R step) (- De Xshift Rif)))))

	(setq X (+ R Xshift (* -1 De) (* Rif (- 1 (cos buffer)))));위성원 위의 한 점의 X좌표
	(setq Y (- (* R step) (/ (* (- De Xshift Rif) (cos A)) (sin A)) (* Rif (sin buffer)))) ;위성원 위의 한 점의 Y좌표

	(setq buffer (sqrt (+ (expt X 2) (expt Y 2))))

	(setq p (polar center (- (atan Y X) step) buffer))

	(command "arc" "" P) (setq Ss (ssadd (entlast) Ss));앞서 그려진 선에 접선하여 호를 그립니다.
      );End while
    );End progn
  );End if

  (setq Pm P)

  (setq p (polar center (+ Ar (/ pi (abs teeth))) (- (+ R Xshift) De)))

  (command "arc" Pm "c" center P) (setq L1 (entlast)) (setq Ss (ssadd (entlast) Ss))

  (command "erase" L0 "");연속 arc의 시작점을 정하기 위한 라인을 지웁니다.




;이끝 치형의 시작 좌표를 구합니다.
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


;연속 arc의 시작점을 정하기 위한 라인을 그립니다.
  (command "line" Pm P "") (setq L0 (entlast))


;치형의 바깥쪽 Involute곡선을 그립니다.
  (setq counter (1- series))
  (while (> (* 2 series) counter)
    (setq counter (1+ counter))
    (setq step (/ (* (+ series counter) (1+ (- counter series)) (- Ce Cs Cp)) (* 3 series (1+ series))));각도를 증가시킴

    (setq X (+ (+ R Xshift (* -1 De) (* Rif (- 1 (sin A)))) (* (* R (cos A) (sin A)) (+ step Cp))))
    (setq Y (+ (+ (/ (* -1 (- De Xshift Rif) (cos A)) (sin A)) (* -1 Rif (cos A))) (* (* R (expt (cos A) 2)) (+ step Cp))))

    (setq buffer (sqrt (+ (expt X 2) (expt Y 2))))

    (setq P0 P)
    (setq p (polar center (- (atan Y X) (+ step Cp)) buffer))

    (command "arc" "" P) (setq Ss (ssadd (entlast) Ss));앞서 그려진 선에 접선하여 호를 그립니다.

    (setq buffer (polar p (angle p (cdr (assoc 10 (entget (entlast))))) Ref))
    (if (< (+ R Xshift Ad) (+ (distance center buffer) Ref));조건이 맞으면 곡선을 그만 그립니다.
      (setq counter (1+ (* 2 series)))
    );End if
  );End while

  (if (= 0.0 Ref);모깎기가 있으면 모깎기를 합니다.
    (progn
      (setq P0 P)
      (setq p (polar center Ar (+ R Xshift Ad)))
      (command "arc" P "c" center P0) (setq L1 (entlast)) (setq Ss (ssadd (entlast) Ss));기어의 최대 외각 선을 그립니다.
    );End progn
    (progn
      (setq L2 (entlast))
      (setq p (polar center Ar (+ R Xshift Ad)))
      (setq Pm (polar center (angle center P0) (+ R Xshift Ad))) 
      (command "arc" P "c" center Pm) (setq L1 (entlast)) (setq Ss (ssadd (entlast) Ss));기어의 최대 외각 선을 그립니다.
      (command "fillet" "r" Ref);모깎기 반지름 값을 지정합니다.
      (command "fillet" (list L2 P0) (list L1 P)) (setq Ss (ssadd (entlast) Ss));모깎기합니다.
    );End progn
  );End if

  (command "erase" L0 "");연속 arc의 시작점을 정하기 위한 라인을 지웁니다.








; 그려진 치형의 일부분을 배열하여 치형을 완성합니다.
  (command "change" Ss "" "P" "C" "bylayer" "LT" "bylayer" "")
  (command "pedit" L1 "y" "j" Ss "" "") (setq L0 (entlast));치형 곡선을 모두 연결합니다.

  (setq Ss (ssadd))

  (command "mirror" L0 "" center P "") (setq L1 (entlast));치형을 대칭시켜 반대편 곡선을 그립니다.
  (command "pedit" L0 "j" L1 "" "") (setq L0 (entlast)) (setq Ss (ssadd L0 Ss));치형 1개를 완성합니다.
  (if (> (abs Cs) A)
    (progn
      (setq Ss (ssadd L3 Ss))
      (command "mirror" L3 "" center P "") (setq L2 (entlast)) (setq Ss (ssadd L2 Ss));치형을 대칭시켜 반대편 곡선을 그립니다.
    );End progn
  );Enf if
  (command "rotate" Ss "" center (/ (* -1 Ar 180) pi))

  (if (/= (abs teeth) 1); 치형이 1개이면 아래 명령은 실행하지 않습니다.
    (progn
      (command "array" Ss "" "p" center (abs teeth) 360 "");치형 1개를 잇수만큼 배열합니다.
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




; AutoCAD의 설정 값을 LISP 실행 전으로 되돌립니다.
  (setvar "clayer" clayer)
  (setvar "osmode" osmode)
  (setvar "cmdecho" cmdecho)

  (princ)
)



;한글로 gear을 입력 할 경우 위 프로그램을 실행 합니다.
(DEFUN C:ㅎㄷㅁㄱ ()
  (C:gear)
)

;한글로 "기어"를 입력 할 경우 위 프로그램을 실행 합니다.
(DEFUN C:기어 ()
  (C:gear)
)
