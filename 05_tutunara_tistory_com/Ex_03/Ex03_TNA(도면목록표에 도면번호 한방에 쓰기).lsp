;청산님 말씀------------------------------------------------------------
;순차적으로 처음부터 문자를 쓰는 일이라면 쉬운 작업인데 지금 올리는 것은 기존에 이미 쓰여진 도면 핸들과 상관없이

;문자의 좌표값을 기준으로 수평 또는 수직으로 자동으로 문자가 바뀌도록 한 것을 조금 수정했습니다.

 

;일단 리습을 오토캐드 지원 경로중에 한 곳에 복사를 하고 (load "TNA")로 호출한 후 사용하면 됩니다.

 

;차례대로 그 과정을 설명하자면, 먼저 "TNA"라고 명령어를 입력합니다.

 

 

 

;위 그림과 같은 도면목록표가 있다고 가정할 때,


 

 
;그림처럼 수정하고자 하는 문자를 선택합니다.
 
;선택이 끝나면 아래 순서와 같은 메시지가 출력됩니다.
 
;Text Read Direction [X/<Y>]  ==> 문자의 방향을 묻습니다. 세로 방향이므로  "y" 또는 기본 설정이므로 엔터를 칩니다.
 
;숫자 앞에 붙는 문자<none>: ==> 숫자 앞에 붙는 문자를 입력합니다. 없으면 그냥 엔터만 치면 됩니다.
 
;숫자 뒤에 붙는 문자<none>: ==> 숫자 뒤에 붙는 문자를 입력합니다. 역시 없으면 엔터만 칩니다.
 
;시작 숫자<1>: ==> 숫자의 시작값을 입력합니다. 기본 1로 설정되어 있습니다. 11부터 시작한다면 11이라고 입력합니다.
 
;자릿수<1>: ==> 숫자의 마지막 자릿수, 즉 100에서 끝나면 3, 10 ~ 99 이하이면 2라고 입력하세요.

;　　　　　　　 그렇지 않으면 001, 002, ... 099, 100 이 아닌 1,2,3 ... 99, 100으로 바뀌게 됩니다.
; 
;증가할 수치값<1>: 시작 숫자부터 증가되는 값, 즉 1이 시작이고 여기서 2를 입력하면 1,3,5,7,9... 이런식이 됩니다.

 
 
;"A-"로 시작하여 자릿수가 3자리에 증가값이 1을 주어 수정한 도면목록표 입니다.

 
 

;확대한 그림입니다.


 

 
;앞에 붙는 문자를 "CAD-" 뒷에 붙는 문자를 "-LISP" 자릿수를 기본 1로 했을때 바뀐 그림입니다.
 
;위 예시와 다른점은 자릿수를 기본으로 했기에 10 이하의 숫자 앞에 "0"이 붙지 않았습니다.
 
;자릿수 공백만큼 "0"을 채우고 싶다면 처음 예시처럼 마지막 숫자의 자릿수를 입력하고
;"0"을 채울 필요가 없으면 자릿수 물어볼때 그냥 엔터만 치면 됩니다.
 
;처음에 아무 문자나 입력을 한 후에 표 간격에 맞추어 복사(Array)를 하고 이 리습을 쓰면 됩니다.
 
;문자가 같은 위치 선상이 아닌 좌우로 어긋나 있거나 문자의 핸들 순서가 맞지 않아도 상관없습니다.
 
;시간이 없어 예시는 없지만 "Text Read Direction [X/<Y>]" 이 메시지에서 "H" 즉, 가로방향이라고 입력하면
;위 예시처럼 좌측부터 우측으로 차례대로 문자를 변경할 수 있습니다. 
  


; nth 함수에서 초기값을 0이 아닌 1로 시작하기 위한 함수
(defun @nth(&Number &VarName) (nth (1- &Number) &VarName))


; C:TNA 프로그램에 포함되는 문자열 선택 및 그 값들을 비교, 정렬하는 함수
(defun JH_ReplaceTextSelect(&VarName1 &VarName2 &YN / !Index !Enam !EntList !Point)
  (setq !Sel (ssget (list (cons 0 "TEXT"))))
  (if (= &YN "Y")
    (setq !Dir (strcase (getstring "\nText Read Direction [-X/+X/-Y/<+Y>/]: ")))
    (progn  ; 첫번째 객체 선택에서는 X, Y 방향만 필요하므로 추가한 부분
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

; 좌표 리스트를 구함
    (setq !Point (JH_StringFill (rtos !Point 2 0) 8 "0" 1))
    (setq !PointList (acad_strlsort (append !PointList (list !Point))))
; 좌표와 Ename을 하나의 리스트로 묶음
    (setq !EnameList (append !EnameList (list (list !Point !Ename))))
    (setq !Index (1+ !Index))
  )
  
  (eval (read (strcat "(setq " (eval &VarName1) " !PointList)")))
  (eval (read (strcat "(setq " (eval &VarName2) " !EnameList)")))

  (setq !PointList nil)
  (setq !EnameList nil)
)

; 주어진 문자열 길이보다 값이 작을때 나머지를 주어진 문자로 채우기 위한 함수
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

; C:TNA 프로그램에 포함되는 문자열 선택 및 그 값들을 비교, 정렬하는 함수
(defun JH_ReplaceTextSelect(&VarName1 &VarName2 &YN / !Index !Enam !EntList !Point)
  (setq !Sel (ssget (list (cons 0 "TEXT"))))
  (if (= &YN "Y")
    (setq !Dir (strcase (getstring "\nText Read Direction [-X/+X/-Y/<+Y>/]: ")))
    (progn  ; 첫번째 객체 선택에서는 X, Y 방향만 필요하므로 추가한 부분
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

; 좌표 리스트를 구함
    (setq !Point (JH_StringFill (rtos !Point 2 0) 8 "0" 1))
    (setq !PointList (acad_strlsort (append !PointList (list !Point))))
; 좌표와 Ename을 하나의 리스트로 묶음
    (setq !EnameList (append !EnameList (list (list !Point !Ename))))
    (setq !Index (1+ !Index))
  )
  
  (eval (read (strcat "(setq " (eval &VarName1) " !PointList)")))
  (eval (read (strcat "(setq " (eval &VarName2) " !EnameList)")))

  (setq !PointList nil)
  (setq !EnameList nil)
)


; 정렬된 좌표 리스트를 가지고 새로운 Ename List를 작성하는 함수
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

; 이미 입력되어 있는 문자들이 일정한 행/열로 이루어져 있을때
; 자동으로 바꾸기 위한 프로그램
(defun C:TNA(/ !PrefixString !SuffixString !StartNo !Cipher !CipherStr !Step
               !Index !Entity !Value !Sel !Sel1 !Sel2 !Dir !Dir1 !Dir2)
  (command "undo" "be")
  (JH_ReplaceTextSelect "!PointList1" "!EnameList1" "N")

  (JH_NewEnameList !PointList1 !EnameList1 "!NewEnameList1")
  (if (= (substr !Dir 2 1) "Y")
    (setq !NewEnameList1 (reverse !NewEnameList1))
  )

; 최종적으로 정리된 값들을 비교하여 문자열을 바꾸는 부분
  (setq !PrefixString (getstring "\n숫자 앞에 붙는 문자<none>: "))
  (setq !SuffixString (getstring "\n숫자 뒤에 붙는 문자<none>: "))
  (setq !StartNo (getint "\n시작 숫자<1>: "))
  (if (not !StartNo)
    (setq !StartNo 1)
  )

  (setq !Cipher (getint "\자릿수<1>: "))
  (if (not !Cipher)
    (setq !Cipher 1)
  )

  (setq !Step (getint "\n증가할 수치값<1>: "))
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

