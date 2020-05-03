;;선택점 중심에서 장공 그리기 
(defun c:ss(/ os cla ort cp dia rad len hlen p1 p2 p3 p4 p5 p6) ; 지역변수와 defun 정의,명령어 JG 

;아래 두줄은 명령 실행중 취소를 하거나 에러 발생시 에러 메세지를 띄우고 osmode, clayer, orthomode 를 초기값으로 되돌림 
(defun *error* (msg)(princ "error: ")(princ msg) 
(setvar "osmode" os) (setvar "clayer" cla)(setvar "orthomode" ort) (princ) ) 
;;에러 핸들러 끝 

(prompt "\n 선택점 중심에서 장공 그리기") ; 화면에 보여질 문장 

(setq os (getvar "osmode")) ; 현재의 osmode를 os 함수에 기억시킴 
(setq cla (getvar "clayer")) ;현재의 layer를 cla 함수에 기억시킴 
(setq ort (getvar "orthomode")) ;현재의 orthomode를 ort 함수에 기억시킴 
(setvar "osmode" 36) ; Int, cen (원 선택 또는 중심점 선택) 

(setq cp (getpoint "\n≫ Slot의 중심점을 찍으세요 : ")) ; 사용자 입력접 요구하는 구문 

(setvar "osmode" 48) ;qua,int (원의 사분점 또는 중심점 선택을 위해서)

(if (= dia nil) (setq dia 9)) ; dia값이 nil일 때 (폭)초기값 9로 설정 

(setq sdia dia) ; dia 초기값 9를 sdia 함수로 재정의 

(setq dia (getdist (strcat "\n≫ Slot의 폭을 찍거나(2P) 값을 입력하세요["(itoa sdia)"] : "))) 
; dia 함수의 값 입력을 요구 getdist로 거리값(입력 또는 마우스 점), strcat,(itoa sdia)는위 초기 지정했던 값을 화면에 보여지게 함 

(if (= dia nil) (setq dia sdia)) ; dia함수값의 입력이 없을 때 위에서 지정한 sdia의 값을 취해 dia 함수로 대체함 

(setq rad (/ dia 2)) ; 입력된 dia값을 반으로 나눔(선태점이 슬롯폭의 중심점에 오게 하기 하기 위한 준비작업) 

(setvar "orthomode" 1) ; 거리를 마우스로 선택시 직교 사용을 하기위한 

(if (= len nil) (setq len 20)) ;len 값이 nil일 때 길이의 초기값 20로 설정 

(setq nlen len) ; len 초기값 20를 nlen 함수로 재정의 

(setq len (getdist (strcat "\n≫ Slot의 길이를 찍거나(2P) 값을 입력하세요["(itoa nlen)"] : "))) 

; len 함수의 값 입력을 요구 getdist로 거리값(입력 또는 마우스 점), strcat,(itoa nlen)는위 초기 지정했던 값을 화면에 보여지게 함 
(if (= len nil) (setq len nlen)) ; len함수값의 입력이 없을 때 위에서 지정한 nlen의 값을 취해 len 함수로 대체함 
(setq hlen (/ len 2)) ; 입력된 len값을 반으로 나눔(선택점이 슬롯길이의 중심점에 오게 하기 하기 위한 준비작업) 

; 아래는 그림을 그리기 위한 각각의 위치(점)에 대한 정의. 
; 위치점함수(polar 점, 각도-X축을 기중으로 하는 상대적인 각도), 길이) 로 구성됨 
(setq pt1 (polar cp (/ pi 2) rad)) 

; 점 'pt1'은... Base point 'cp'에서 위쪽90도(3.14/2=1.57radian)각도로 지정폭의 절반인 'rad' 거리에 위치 
(setq pt2 (polar pt1 0 hlen)) 
; 점 'pt2'는... 'pt1'에서 0도의 각도로 지정길이의 절반인 'hlen' 거리에 위치 
(setq pt3 (polar pt2 (+ (/ pi 2) pi) dia)) 
; 점 'pt3'은... 'pt2'에서 270도((3.14/2)+3.14)의 각도로 지정폭만큼인 'dia' 거리에 위치 
(setq pt4 (polar pt3 pi (* hlen 2))) 
; 점 'pt4'는... 'pt3'에서 180도((3.14radian)의 각도로 지정길이의 절반인 'hlen'의 두배(len값) 거리에 위치 
(setq pt5 (polar pt4 (/ pi 2) dia)) 
; 점 'pt5'는... 'pt4'에서 90도(3.14/2=1.57radian)의 각도로 지정폭만큼인 'dia' 거리에 위치 
(setvar "osmode" 0) 
; 넓은 화면에서 작게 그릴 때 none이 아닌 다른 osmode로 인해 주변 객체점에 클릭이 되어 발생되는 형상로유를 방지하기 위해 none으로 설정 

;(command "layer" "s" "0" "") ; 사용자 레이어 설정 
(command "pline" pt5 pt2 "a" pt3 "l" pt4 "a" pt5 "") ; Pline 명령으로 지정된 각 점을 연결...a는 arc, l은 line 
(command "rotate" (entlast) "" cp "\\") ; 그려진 마지막 객체(슬롯)을 회전명령으로 회전 사용자 입력을 요함 
(command "undo" "e") ; 작업을 끝냄 

(setvar "osmode" os) ; 초기에 기억했던 osmode 설정치로 되돌림 
(setvar "clayer" cla) ; 초기에 기억했던 layer 설정치로 되돌림 
(princ) ;; 마지막 응답값인..nil을 안보여주게 됨 

) ;defun  끝
