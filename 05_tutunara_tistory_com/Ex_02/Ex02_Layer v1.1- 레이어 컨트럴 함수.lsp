;;******* Layer control **********************************************************************
;;-------- Layer List
; 1Lst	   : 레이어 list 화 하기
;;-------- 레이어 제어 리습 명령어
; LAI		   : 모든레이어 한번에 켜기,동결해제,잠금해제하기
;-------- ON / OFF
; LON	   : 모든레이어 켜기
; LOF		: 선택하여 레이어 끄기 (다중선택 가능)
; LEF		   : 선택한 나머지 객체의 레이어 끄기 (다중선택 가능)
;-------- LOCK / UNLOCK
; LUK		: 모든레이어 잠금 해제
; LOK		: 선택한 객체의 레이어 잠그기 (다중선택 가능)
; LEK		: 선택한 나머지 객체의 레이어 잠그기 (다중선택 가능)
;-------- FREEZE / THAW
; LTH		: 모든레이어 동결해제
; LFR		: 선택한 레이어 동결 (다중선택 가능, 현재 레이어 동결안됨)
; LEF		   : 선택한 나머지 객체의 레이어 동결시키기 (다중선택 가능)

; 1vix	   : 선택한 객체 숨기기 (레이어와 무관)
; 1vis	   : 선택한 객체 보이기 (레이어와 무관)

;;-------- in BATCH, FREEZE / THAW 
; 1po          : 배치에서 선택 동결 on/off 
; 1poo        : 배치에서 선택 동결 연속
; 1px          : 배치에서 동결 해제 thaw 
; 1pf          : 배치에서 선택반전 동결
; 1tw          : dview twist

;;-------- Layer ON / OFF 
; 142	        : LAYER ALL ON & OFF
; 142q	        : LAYER ALL ON
; 142x	        : LAYER ALL OFF
; 141	        : Select Lock
; 141x	        : Select Unlock 
; 14f	           : Select FREEZE
; 14c	        : Select color on ->색상별 레이어 켜기(부착된 레이어 제외)
; 152	        : Quick layer thaw 
; 152x	        : Quick layer off ->block/xref의 attach된 도면에서도 작동되는 강력한 layer off                                                        
; 15f	           : Quick layer freeze
;;-------- Layer Change 
; 13b	        : Bind Layer change ->bind되어 -$0$- 표기된 레이어을 변경후 삭제
; 13n	        : Layer Rename
; 13c	        : color change
; 13l	           : Linetype change
; 13d	        : Layer change(color no)
; 1asl	        : 선택한 객체와 같은레이어의 모든객체 선택리습
; 1xll           : xref 레어어 off 된거 리스트화시켜서 화면에 써주기
; mvln         : MVline을 MS공간에


;;===== LAYER =============================================================
; 아키모아 운영진 "행복한하루"
; 레이어 list 화 하기
; 2007.09.19 수정(2007.10.03) 

(defun c:1Lst(/ lt os laname-list laname lacolor-list lacolor la n maxlen th len 
               ent lalinetype-list lalinetype p1 p2 p3 p4 @p1 @p2 @p3 @p4 cla @k @ed #ss #k @la #num
               t-len t-maxlen cho)
;->*error* start   
 (defun *error* (msg)(princ "error: ")(princ msg)
 (setvar "osmode" os) (setvar "clayer" cla)
 (princ))
;-<*error* end
 (setvar "cmdecho" 0)
 (prompt "\n레이어 list 화 하기..")
 (command "undo" "be")
 (setq lt (getvar "ltscale"))
 (setq os (getvar "osmode"))
 (setq cla (getvar "clayer"))
 (command "layer" "t" "*" "")
 (command "layer" "u" "*" "")
 (command "layer" "on" "*" "")
 (setq laname-list '() lacolor-list '() lalinetype-list '() maxlen 0 n 0 t-maxlen 0)
 (setq th (* lt 3))
 (setq cho (getint "\n 1.영역지정<1> / 2.전체지정<엔터>:"))
 (if (= cho 1)
  (progn (prompt "\n>> 영역을 선택해주세요")
   (setq #ss (ssget))
   (setq #k 0)
   (repeat (sslength #ss)
    (setq la (entget (ssname #ss #k)))   
    (setq laname (cdr (assoc 8 la)))
    (setq laname-list (append laname-list (list laname)))
    (setq #k (1+ #k))
   ) ;repeat
   (setq laname-list (str_memb laname-list))
  );progn
  (progn
   (setq la (tblnext "layer" t))
   (while la
    (setq laname (cdr (assoc 2 la)))
    (setq laname-list (append laname-list (list laname)))
    (setq la (tblnext "layer"))
   );while
  );progn
 );if

 (setq laname-list (acad_strlsort laname-list))
 (setq #num 0)
 (repeat (length laname-list)
  (setq laname (nth #num laname-list))
  (setq @la (tblsearch "layer" laname))
  (setq lacolor (cdr (assoc 62 @la))
        lalinetype (cdr (assoc 6 @la)))
  (setq len (car (cadr (textbox (list (cons 1 laname) (cons 40 th))))))
  (setq maxlen (max maxlen len))
  (setq t-len (car (cadr (textbox (list (cons 1 lalinetype) (cons 40 th))))))
  (setq t-maxlen (max t-maxlen t-len))
  (setq lacolor-list (append lacolor-list (list lacolor)))
  (setq lalinetype-list (append lalinetype-list (list lalinetype)))
  (setq #num (1+ #num))
 ) ;repeat

 (setq p1 (getpoint "\n 표를 삽일할 포인트 :")) 
 (setq p2 (polar p1 0 (* maxlen 1.2))) 
 (setq p3 (polar p2 0 maxlen))
 (setq p4 (polar p3 0 (* t-maxlen 1.2)))

 (setq @p1 (polar p1 (+ (/ pi 2) pi) th) 
       @p2 (polar p2 (+ (/ pi 2) pi) (/ th 2))
       @p3 (polar p3 (+ (/ pi 2) pi) th)
       @p4 (polar p4 (+ (/ pi 2) pi) th))

 (setvar "osmode" 0)
 (setq ss (ssadd))
 (repeat (length laname-list)
   (setq ss1 (ssadd))
   (command "text" @p1 th 0 (nth n laname-list)) (ssadd (entlast) ss) (ssadd (entlast) ss1)
   (command "line" @p2 (polar @p2 0 maxlen) "") (ssadd (entlast) ss) (ssadd (entlast) ss1)
   (command "text" @p3 th 0 (nth n lalinetype-list)) (ssadd (entlast) ss) (ssadd (entlast) ss1)
   (command "text" @p4 th 0 (rtos (nth n lacolor-list) 2 0)) (ssadd (entlast) ss) (ssadd (entlast) ss1)
   (setq @k 0)
   (repeat (sslength ss1)
     (setq @ed (entget (ssname ss1 @k)))
     (entmod (subst (cons 8 (nth n laname-list)) (assoc 8 @ed) @ed))
     (setq @k (1+ @k))
   );repeat      
   (setq @p1 (polar @p1 (+ (/ pi 2) pi) (* th 2)))
   (setq @p2 (polar @p2 (+ (/ pi 2) pi) (* th 2)))
   (setq @p3 (polar @p3 (+ (/ pi 2) pi) (* th 2)))
   (setq @p4 (polar @p4 (+ (/ pi 2) pi) (* th 2)))
   (setq n (1+ n))
 );repeat
 (command "change" ss "" "p" "c" "bylayer" "lt" "bylayer" "")
 (setvar "osmode" os)
 (setvar "clayer" cla)
 (command "undo" "e")
(princ)
);defun

(defun str_memb (str_lst / a b)
  (setq b (reverse str_lst))
  (mapcar '(lambda (x) (if (= (member x a) nil) (setq a (cons x a)))) b)
  a
)

;Layer initialization -----------------------------------------------------------------------------------------------------------------------------
(defun c:lai(/ cla ss k la-lis ed la k) 
(setvar "cmdecho" 0)
(princ "\n ::::: Layer initialization :::::")
(command "layer" "t" "*" "")
(command "layer" "on" "*" "")
(command "layer" "u" "*" "")
(princ "\n ::::: Layer All On,Thaw,Unlock completion :::::")
(princ) 
) ;defun

;Layer On -----------------------------------------------------------------------------------------------------------------------------
(defun c:lon(/ cla ss k la-lis ed la k) 
(setvar "cmdecho" 0)
(princ "\n ::::: Layer On :::::")
(command "layer" "on" "*" "")
(princ "\n ::::: Layer On completion :::::")
(princ) 
) ;defun

;Layer Off -----------------------------------------------------------------------------------------------------------------------------
(defun c:lof(/ cla ss k la-lis ed la k) 
(princ "\n ::::: Layer Off :::::")
(setq cla (getvar "clayer")) 
(setq ss (ssget)) 
(setq k 0 la-lis '()) 
(setvar "cmdecho" 0)
(repeat (sslength ss) 
  (setq ed (entget (ssname ss k)) 
        la (cdr (assoc 8 ed))) 
  (setq la-lis (append la-lis (list la))) 
  (setq k (1+ k)) 
) ;repeat
(setq la-lis (str_memb la-lis)) 
(command "undo" "be") 
(foreach x la-lis (progn 
  (if (= x cla) (command "layer" "off" x "y" "") 
   (command "layer" "off" x "")) 
)
(princ "\n[")(princ x)(princ "] layer is off.")
) ;foreach
(command "undo" "e") 
(princ "\n ::::: Layer Off completion :::::")
(princ) 
) ;defun

;Layer Except Off -----------------------------------------------------------------------------------------------------------------------------
 (defun c:leo(/ cla ss k la-lis ed la k) 
(princ "\n ::::: Layer Except Off :::::")
(setq cla (getvar "clayer")) 
(setq ss (ssget)) 
(setq k 0 la-lis '()) 
(setvar "cmdecho" 0)
(repeat (sslength ss) 
  (setq ed (entget (ssname ss k)) 
        la (cdr (assoc 8 ed))) 
  (setq la-lis (append la-lis (list la))) 
  (setq k (1+ k)) 
) ;repeat
(setq la-lis (str_memb la-lis)) 
(command "undo" "be")
(command "layer" "off" "*" "y" "") 
(foreach x la-lis (progn 
(command "layer" "on" x "") 
)
(princ "\n[")(princ x)(princ "] layer is on.")
) ;foreach
(command "undo" "e") 
(princ "\n ::::: Layer Except Off completion :::::")
(princ "\n\n ::::: All Layer On Function : [ LON ] :::::")
(princ) 
) ;defun

;Layer Unlock -----------------------------------------------------------------------------------------------------------------------------
(defun c:luk(/ cla ss k la-lis ed la k) 
(setvar "cmdecho" 0)
(princ "\n ::::: Layer Unlock :::::")
(command "layer" "u" "*" "")
(princ "\n ::::: Layer Unlock completion :::::")
(princ) 
) ;defun

;Layer lock -----------------------------------------------------------------------------------------------------------------------------
(defun c:lok(/ cla ss k la-lis ed la k) 
(princ "\n ::::: Layer Lock :::::")
(setq cla (getvar "clayer")) 
(setq ss (ssget)) 
(setq k 0 la-lis '()) 
(setvar "cmdecho" 0)
(repeat (sslength ss) 
  (setq ed (entget (ssname ss k)) 
        la (cdr (assoc 8 ed))) 
  (setq la-lis (append la-lis (list la))) 
  (setq k (1+ k)) 
) ;repeat
(setq la-lis (str_memb la-lis)) 
(command "undo" "be") 
(foreach x la-lis 
  (progn 
  (command "layer" "lo" x "")
  ) 
(princ "\n[")(princ x)(princ "] layer is lock.")
) ;foreach
(command "undo" "e") 
(princ "\n ::::: Layer Lock completion :::::")
(princ) 
) ;defun

;Layer Except Lock -----------------------------------------------------------------------------------------------------------------------------
 (defun c:lek(/ cla ss k la-lis ed la k) 
(princ "\n ::::: Layer Except Lock :::::")
(setq cla (getvar "clayer")) 
(setq ss (ssget)) 
(setq k 0 la-lis '()) 
(setvar "cmdecho" 0)
(repeat (sslength ss) 
  (setq ed (entget (ssname ss k)) 
        la (cdr (assoc 8 ed))) 
  (setq la-lis (append la-lis (list la))) 
  (setq k (1+ k)) 
) ;repeat
(setq la-lis (str_memb la-lis)) 
(command "undo" "be")
(command "layer" "lo" "*" "") 
(foreach x la-lis (progn 
(command "layer" "u" x "") 
)
(princ "\n[")(princ x)(princ "] layer is unlock.")
) ;foreach
(command "undo" "e") 
(princ "\n ::::: Layer Except Lock completion :::::")
(princ) 
) ;defun

;Layer Thaw -----------------------------------------------------------------------------------------------------------------------------
(defun c:lth(/ cla ss k la-lis ed la k) 
(setvar "cmdecho" 0)
(princ "\n ::::: Layer Thaw :::::")
(command "layer" "t" "*" "")
(princ "\n ::::: Layer Thaw completion :::::")
(princ) 
) ;defun

;Layer Freeze-----------------------------------------------------------------------------------------------------------------------------
(defun c:lfr(/ cla ss k la-lis ed la k) 
(princ "\n ::::: Layer Freeze :::::")
(setq cla (getvar "clayer")) 
(setq ss (ssget)) 
(setq k 0 la-lis '()) 
(setvar "cmdecho" 0)
(repeat (sslength ss) 
  (setq ed (entget (ssname ss k)) 
        la (cdr (assoc 8 ed))) 
  (setq la-lis (append la-lis (list la))) 
  (setq k (1+ k)) 
) ;repeat
(setq la-lis (str_memb la-lis)) 
(command "undo" "be") 
(foreach x la-lis 
  (progn 
  (command "layer" "f" x "")
  ) 
(princ "\n[")(princ x)(princ "] layer is Freeze.")
) ;foreach
(command "undo" "e") 
(princ "\n ::::: Layer Freeze completion :::::")
(princ) 
) ;defun

;Layer Except Freeze -----------------------------------------------------------------------------------------------------------------------------
 (defun c:lef(/ cla ss k la-lis ed la k) 
(princ "\n ::::: Layer Except Freeze :::::")
(setq cla (getvar "clayer")) 
(setq ss (ssget)) 
(setq k 0 la-lis '()) 
(setvar "cmdecho" 0)
(repeat (sslength ss) 
  (setq ed (entget (ssname ss k)) 
        la (cdr (assoc 8 ed))) 
  (setq la-lis (append la-lis (list la))) 
  (setq k (1+ k)) 
) ;repeat
(setq la-lis (str_memb la-lis)) 
(command "undo" "be")
(command "layer" "f" "*" "") 
(foreach x la-lis (progn 
(command "layer" "t" x "") 
)
(princ "\n[")(princ x)(princ "] layer is Thaw.")
) ;foreach
(command "undo" "e") 
(princ "\n ::::: Layer Except Freeze completion :::::")
(princ) 
) ;defun
;서브루틴함수-----------------------------------------------------------------------------------------------------------------------------
(defun str_memb (str_lst / a b) 
  (setq b (reverse str_lst)) 
  (mapcar '(lambda (x) (if (= (member x a) nil) (setq a (cons x a)))) b) 
  a 
)

;;----------배치에서 선택 동결 on/off "행복한하루" (2007.03.16)--------------------------- 
;select vplayer freeze
(defun c:1po(/ ss n k)
(prompt "\nSelect vp freeze->")
(setq ss (ssget))
(setq n (sslength ss))
(setq k 0)
(repeat n
  (setq la (cdr (assoc 8 (entget (ssname ss k)))))
  (command "vplayer" "f" la "" "")
  (setq k (1+ k))
)
(princ)
)
;;------------------------------------- 
(defun c:1poo (/ ss ed la)
  (prompt "\n 배치에서 선택한것 동결..")
  (setq ss (car (nentsel "\n선택 동결 객체 선택:")))
  (command "undo" "be")
  (while ss
    (setq ed (entget ss))
    (setq la (cdr (assoc 8 ed)))
    (command "vplayer" "f" la "c" "")
    (setq ss (car (nentsel "\n선택 동결 객체 선택:")))
  )
  (command "undo" "e")
(princ)
)

;;----select vplayer thaw-----------
(defun c:1px() ; vplayer all on
         (command "vplayer" "t" "*" "" "")
         (princ)
)  

;; 배치에서 선택반전 동결"행복한하루2007.09. 18"
(defun c:1pf(/ ss k lalist ed la)
  (prompt "\n 배치에서 선택한것만 남기고 다 동결..")
  (setq ss (ssget))
  (if ss
   (progn
    (setq k 0 lalist '())
    (repeat (sslength ss)
     (setq ed (entget (ssname ss k))
           la (cdr (assoc 8 ed)))
     (if la (setq lalist (append lalist (list la))))
     (setq k (1+ k))
    );repeat
    (command "undo" "be")
    (command "vplayer" "f" "*" "c" "")
    (foreach x lalist (command "vplayer" "t" x "c" ""))
    (command "undo" "e")
   );progn
  );if
(princ)
)



;;-------------------------------------
; http://cafe.daum.net/archimore "행복한하루"
; dview > tw
; 2008.07.13 , 2008.08.09 ucs회전 추가
;;-------------------------------------
(defun c:1tw (/ ent ed en x10 x11 stp enp $ang ang) 
(setq ent (car (entsel "\n라인을 선택하세요<or enter>:"))) 
(if ent 
  (progn 
   (setq ed (entget ent) 
         en (cdr (assoc 0 ed))) 
   (if (= en "LINE") 
    (progn 
     (setq x10 (cdr (assoc 10 ed)) x11 (cdr (assoc 11 ed))) 
     (if (< (cadr x10) (cadr x11)) 
      (setq stp x10 enp x11) 
      (setq stp x11 enp x10) 
     ) 
     (setq ang (rtd (angle stp enp))) 
     (if (> ang 90)  
      (setq $ang (- 180 ang)) 
      (setq $ang (- 360 ang)) 
     ) 
     (command "undo" "be")
     (command "dview" "" "tw" $ang "")
     (command "ucs" "z" (- 0 $ang)) 
     (command "undo" "e")
    ) 
   ) 
  ) 
) 
(princ) 
) 
(defun rtd (a)(/ (* a 180.0) pi)) 
;;-------------------------------------
(defun c:1tww ( / ent vl_obj x10 x11 stp enp ang $ang) 
        (vl-load-com)  
        (if (setq ent (car (entsel "\n라인을 선택하세요<or enter>:")))  
                (progn 
                        (setq vl_obj (vlax-ename->vla-object ent)) 
                        (setq x10 (vlax-curve-getstartpoint vl_obj)) 
                        (setq x11 (vlax-curve-getendpoint vl_obj)) 
                        (if (< (cadr x10) (cadr x11)) 
                                (setq stp x10 enp x11) 
                                (setq stp x11 enp x10) 
                        ) 
                        (setq ang (rtd (angle stp enp))) 
                        (if (> ang 90)  
                                (setq $ang (- 180 ang)) 
                                (setq $ang (- 360 ang)) 
                        ) 
                        (command "dview" "" "tw" $ang "") 
                ) 
        ) 
        (princ) 
) 
(defun rtd (a)(/ (* a 180.0) pi))
;;=====================================
;LAYER ALL ON & OFF
;;-------------------------------------
(defun c:142() (command "layer" "on" "*" "")
   (prompt "\nAll layer is on")(prin1))
(defun c:142q() (command "layer" "on" "*" ""))
(defun c:142x() (command "layer" "off" "*" "" "")
   (prompt "\nAll layer is off")(prin1)
)
;;=====================================
;  Select Lock(0004cho_i)
;;-------------------------------------
(defun c:141(/ ss n k en cly oly)
   (prompt "\nCommand: Select Lock...")
   (prompt "\nLock 레이어만을 선택-> ")
   (setq ss (ssget))
   (setq n (sslength ss))
   (setq k 0 tn n)
   (setq cly (cdr (assoc 8 (entget (ssname ss (1- n))))))
   (while (<= 1 n)
       (setq en (ssname ss k))
       (setq oly (cdr (assoc 8 (entget en))))
       (command "layer" "lo" oly "")
       (setq n (- n 1))
       (setq k (+ k 1))
    )
    (princ)
)
;;=====================================
;  Select Unlock(0004cho_i)
;;-------------------------------------
(defun c:141x(/ ss n k en cly oly)
   (prompt "\nCommand: Select Unlock...")
   (prompt "\nUnlock 레이어만을 선택<*.*>-> ")
   (setq ss (ssget))
   (if (= ss nil) (progn
       (command "layer" "u" "*" "")
       (prompt "\nAll layer is unlock")
   ))
   (if (/= ss nil)(progn
     (setq n (sslength ss))
     (setq k 0 tn n)
     (setq cly (cdr (assoc 8 (entget (ssname ss (1- n))))))
     (while (<= 1 n)
       (setq en (ssname ss k))
       (setq oly (cdr (assoc 8 (entget en))))
       (command "layer" "u" oly "")
       (setq n (- n 1))
       (setq k (+ k 1))
     )
    ))
    (princ)
)

;;================================================
;  SELECT FREEZE(9812cho_i)
;;------------------------------------------------
(defun c:14f (/ ss n k en cly oly)
   (prompt "\nCommand: select Freeze...")
   (prompt "\n켤 레이어만을 선택-> ")
   (setq ss (ssget))
   (setq n (sslength ss))
   (setq k 0)
   (setq cly (cdr (assoc 8 (entget (ssname ss (1- n))))))
   (command "layer" "s" cly "")
   (command "layer" "off" "*" "" "")
   (while (<= 1 n)
       (setq en (ssname ss k))
       (setq oly (cdr (assoc 8 (entget en))))
       (command "layer" "on" oly "")
       (setq n (- n 1))
       (setq k (+ k 1))
    )
    (princ)
)

;;=================================================================
;  Select color on(2007.01.주말농부)
;  ->색상별 레이어 켜기(부착된 레이어 제외)
;;-----------------------------------------------------------------
(defun c:14c (/ en1 la0 lac la-lst la1 lac1 la2 la3 k)
   (prompt " 색상별 레이어 켜기...")
   (setq en1 (car (entsel))) ;entity name
   (setq la0 (cdr (assoc 8 (entget en1))))
   (setq lac (cdr (assoc 62 (entget en1))))
   (if (= lac nil) (setq lac (cdr (nth 3 (tblsearch "layer" la0))))  )

   (setq la-lst (list lac))
   (setq la1 (cdr (cadr (tblnext "layer" t))))
   (setq lac1 (cdr (nth 3 (tblsearch "layer" la1))))
   (if (= (abs lac1) lac)
       (setq la-lst (cons la1 la-lst)) )
   (setq la2 (tblnext "layer"))

   (while la2
       (setq lac1 (cdr (nth 3 la2)));layer color 축출
       (setq la2 (cdr (cadr la2)));layer name
       (if (and (/= (wcmatch la2 "*|*") T) (= (abs lac1) lac))
           (setq la-lst (cons la2 la-lst)) )
       (setq la2 (tblnext "layer"))
   )
   (setq la-lst (vl-remove lac la-lst))
   (setq la-lst (vl-sort la-lst '<))

   (setq la3 (nth 0 la-lst))
   (setvar "clayer" la3)
   (command "layer" "off" "*" "" "")
   (setq k 0)
   (setq lengnum (length la-lst))
   (repeat lengnum
      (setq la3 (nth k la-lst))
      (command "layer" "on" la3 "")
      (setq k (+ k 1))
   )
   (prin1 la-lst)
   (prompt "\nColor number=")(prin1 lac)(prompt "인 레이어을 켬")
   (prompt "(") (prin1 lengnum)(prompt "개)")
   (princ)
)

;;===============================
; Quick layer thaw(2004.3 CHO_I)
;;-------------------------------
(defun c:152 (/ choi d_ndata list1 list2 list3 list32 list33 db1_2 db1_8 db3_2 db3_8
                    db32_2 db32_8 db1_la db3_la db32_la yn1 yn3)
   (setq choi (getvar "clayer"))
   (setq d_ndata (nentsel))
   (setq list1 (car d_ndata)
         list2 (nth 2 d_ndata)
         list3 (nth 3 d_ndata)
   )(terpri)
   (prompt "*list1 : ") (prin1 list1)(terpri)
   (prompt "*list2 : ") (prin1 list2)(terpri)
   (prompt "*list3 : ") (prin1 list3)(terpri)
   (setq list32 (cadr list3))
   (setq list33 (caddr list3))
 ;;list1 구하기
   (setq db1_2 (assoc 2 (entget list1)))
   (setq db1_8 (assoc 8 (entget list1)))  (setq db1_la (cdr db1_8))
       ;화면출력<list1>
       (prompt "*<1>화일이름|블럭이름 : ")(prin1 db1_2)(terpri)
       (prompt "*   Layer name : ")(prin1 db1_la)(terpri)
 ;;list3 구하기
   (if (/= list3 nil) (progn
       (setq db3_2 (assoc 2 (entget (car list3))))
       (setq db3_8 (assoc 8 (entget (car list3))))  (setq db3_la (cdr db3_8))
       ;화면출력<list3>
       (prompt "*<3>화일이름|블럭이름 : ")(prin1 db3_2)(terpri)
       (prompt "    현재 속한 Layer : ")(prin1 db3_la)(terpri)
   ))
 ;;list32 구하기
   (if (/= list32 nil) (progn
       (setq db32_2 (assoc 2 (entget list32)))
       (setq db32_8 (assoc 8 (entget list32)))  (setq db32_la (cdr db32_8))
       ;화면출력<list3.2>
       (prompt "*<3.2>Xref 화일이름 : ")(prin1 db32_2)(terpri)
       (prompt "      Attach Layer : ")(prin1 db32_la)(terpri)
   ))
 ;;list33 구하기
   (if (/= list33 nil) (progn
       (setq db33_2 (assoc 2 (entget list33)))
       (setq db33_8 (assoc 8 (entget list33)))  (setq db33_la (cdr db33_8))
       ;화면출력<list3.3>
       (prompt "*<3.3>Xref 화일이름 : ")(prin1 db33_2)(terpri)
       (prompt "      Attach Layer : ")(prin1 db33_la)(terpri)
   ))
;;Layer freeze
   (if (and (/= list2 nil) (/= db1_la db3_la))
        (progn
         (setq last_laf db1_la)
         (setvar "clayer" "0")
         (command "layer" "off" "*" "" "")
         (command "layer" "on" db1_la "")
        )
        (progn
         (setvar "clayer" db1_la)
         (command "layer" "off" "*" "" "")
         (command "layer" "on" db1_la "")
        )
  )
  (prin1 db1_la)(prompt " ---> Layer ON")
  (prin1)
)

;;================================================================================
; Quick layer off(97-99주말농부)
; ->block 또는 xref의 attach된 도면에서도 작동되는 강력한 layer off 명령어
;;--------------------------------------------------------------------------------
(defun c:152x (/ choi d_ndata list1 list2 list3 list32 list33 db1_2 db1_8 db3_2 db3_8
                    db32_2 db32_8 db1_la db3_la db32_la yn1 yn3)
   (setq choi (getvar "clayer"))
   (prompt "\nQuick layer off...")
   (setq d_ndata (nentsel))
   (setq list1 (car d_ndata)
         list2 (nth 2 d_ndata)
         list3 (nth 3 d_ndata)
   )(terpri)
   (prompt "*list1 : ") (prin1 list1)(terpri)
   (prompt "*list2 : ") (prin1 list2)(terpri)
   (prompt "*list3 : ") (prin1 list3)(terpri)
   (setq list32 (cadr list3))
   (setq list33 (caddr list3))
 ;;list1 구하기
   (setq db1_2 (assoc 2 (entget list1)))
   (setq db1_8 (assoc 8 (entget list1)))  (setq db1_la (cdr db1_8))
       ;화면출력<list1>
       (prompt "*<1>화일이름|블럭이름 : ")(prin1 db1_2)(terpri)
       (prompt "*   Layer name : ")(prin1 db1_la)(terpri)
 ;;list3 구하기
   (if (/= list3 nil) (progn
       (setq db3_2 (assoc 2 (entget (car list3))))
       (setq db3_8 (assoc 8 (entget (car list3))))  (setq db3_la (cdr db3_8))
       ;화면출력<list3>
       (prompt "*<3>화일이름|블럭이름 : ")(prin1 db3_2)(terpri)
       (prompt "    현재 속한 Layer : ")(prin1 db3_la)(terpri)
   ))
 ;;list32 구하기
   (if (/= list32 nil) (progn
       (setq db32_2 (assoc 2 (entget list32)))
       (setq db32_8 (assoc 8 (entget list32)))  (setq db32_la (cdr db32_8))
       ;화면출력<list3.2>
       (prompt "*<3.2>Xref 화일이름 : ")(prin1 db32_2)(terpri)
       (prompt "      Attach Layer : ")(prin1 db32_la)(terpri)
   ))
 ;;list33 구하기
   (if (/= list33 nil) (progn
       (setq db33_2 (assoc 2 (entget list33)))
       (setq db33_8 (assoc 8 (entget list33)))  (setq db33_la (cdr db33_8))
       ;화면출력<list3.3>
       (prompt "*<3.3>Xref 화일이름 : ")(prin1 db33_2)(terpri)
       (prompt "      Attach Layer : ")(prin1 db33_la)(terpri)
   ))
;;Layer freeze
   (if (/= db1_la "0") (progn
       (prin1 db1_la)(prompt "을 OFF 하길 원합니까?<Y> : ")
       (setq yn1 (getstring))
       (if (= yn1 "") (setq yn1 "Y"))
       (setq yn1 (strcase yn1))
       (if (= yn1 "Y")(progn
           (setq last_laf db1_la)
           (if (= db1_la choi) (setvar "clayer" "0"))
           (command "layer" "off" db1_la "")
       ))
   ))
   (if (and (/= list3 nil) (= db1_la "0")) (progn
       (prin1 db3_la)(prompt "을 OFF 하길 원합니까?<Y> : ")
       (setq yn3 (getstring))
       (if (= yn3 "") (setq yn3 "Y"))
       (setq yn3 (strcase yn3))
       (if (= yn3 "Y") (progn
           (setq last_laf db3_la)
           (if (= db3_la choi) (setvar "clayer" "0"))
           (command "layer" "off" db3_la "")
       ))
   ))
   (prin1)
)

;;========================================
; Quick layer freeze x (2004.3 CHO_I)
;;------------------------------------------------------------------------
(defun c:15f (/ choi d_ndata list1 list2 list3 list32 list33 db1_2 db1_8 db3_2 db3_8
                    db32_2 db32_8 db1_la db3_la db32_la yn1 yn3)
   (setq choi (getvar "clayer"))
   (setq d_ndata (nentsel))
   (setq list1 (car d_ndata)
         list2 (nth 2 d_ndata)
         list3 (nth 3 d_ndata)
   )(terpri)
   (prompt "*list1 : ") (prin1 list1)(terpri)
   (prompt "*list2 : ") (prin1 list2)(terpri)
   (prompt "*list3 : ") (prin1 list3)(terpri)
   (setq list32 (cadr list3))
   (setq list33 (caddr list3))
 ;;list1 구하기
   (setq db1_2 (assoc 2 (entget list1)))
   (setq db1_8 (assoc 8 (entget list1)))  (setq db1_la (cdr db1_8))
       ;화면출력<list1>
       (prompt "*<1>화일이름|블럭이름 : ")(prin1 db1_2)(terpri)
       (prompt "*   Layer name : ")(prin1 db1_la)(terpri)
 ;;list3 구하기
   (if (/= list3 nil) (progn
       (setq db3_2 (assoc 2 (entget (car list3))))
       (setq db3_8 (assoc 8 (entget (car list3))))  (setq db3_la (cdr db3_8))
       ;화면출력<list3>
       (prompt "*<3>화일이름|블럭이름 : ")(prin1 db3_2)(terpri)
       (prompt "    현재 속한 Layer : ")(prin1 db3_la)(terpri)
   ))
 ;;list32 구하기
   (if (/= list32 nil) (progn
       (setq db32_2 (assoc 2 (entget list32)))
       (setq db32_8 (assoc 8 (entget list32)))  (setq db32_la (cdr db32_8))
       ;화면출력<list3.2>
       (prompt "*<3.2>Xref 화일이름 : ")(prin1 db32_2)(terpri)
       (prompt "      Attach Layer : ")(prin1 db32_la)(terpri)
   ))
 ;;list33 구하기
   (if (/= list33 nil) (progn
       (setq db33_2 (assoc 2 (entget list33)))
       (setq db33_8 (assoc 8 (entget list33)))  (setq db33_la (cdr db33_8))
       ;화면출력<list3.3>
       (prompt "*<3.3>Xref 화일이름 : ")(prin1 db33_2)(terpri)
       (prompt "      Attach Layer : ")(prin1 db33_la)(terpri)
   ))
;;Layer freeze
   (if (/= db1_la "0") (progn
       (prin1 db1_la)(prompt "을 OFF 하길 원합니까?<Y> : ")
       (setq yn1 (getstring))
       (if (= yn1 "") (setq yn1 "Y"))
       (setq yn1 (strcase yn1))
       (if (= yn1 "Y")(progn
           (setq last_laf db1_la)
           (if (= db1_la choi) (setvar "clayer" "0"))
           (command "layer" "f" db1_la "")
       ))
   ))
   (if (and (/= list3 nil) (= db1_la "0")) (progn
       (prin1 db3_la)(prompt "을 OFF 하길 원합니까?<Y> : ")
       (setq yn3 (getstring))
       (if (= yn3 "") (setq yn3 "Y"))
       (setq yn3 (strcase yn3))
       (if (= yn3 "Y") (progn
           (setq last_laf db3_la)
           (if (= db3_la choi) (setvar "clayer" "0"))
           (command "layer" "f" db3_la "")
       ))
   ))
   (prin1)
)
    
;;=========================================================================
;  Bind Layer change (2006.12.주말농부)
;  ->bind되어 -$0$- 표기된 레이어을 변경후 삭제하는 명령어
;  ->블럭내부에 존재하는 레이어는 이름이 $ 표기 뒷자리이름+!형태로 변경됨.
;  ->오류 수정 (2007.5.15)
;;------------------------------------------------------------------------------------------------------------------------------------
(defun c:13b (/ la1 la2 la nla oldla newla la-lst listnum n1 k1 key$
               sleng ss1 j1 j2 j3)
   (setvar "clayer" "0")
   (setq la1 (tblnext "layer" t))
   (setq la1 (cdr (cadr la1)));layer name
   (setq la-lst (list la1))
   (setq la2 (tblnext "layer"))
   (while la2
       (setq la2 (cdr (cadr la2)));layer name
       (if (and (/= (wcmatch la2 "*|*") T) (= (wcmatch la2 "*$*") T))
           (setq la-lst (cons la2 la-lst)) )
       (setq la2 (tblnext "layer"))
   )

   (setq la-lst (vl-remove "0" la-lst))
   (setq la-lst (vl-sort la-lst '<))
   (setq listnum (length la-lst))
   (setq n1 0 j1 0 j2 0 j3 0)
   (repeat listnum
      (setq oldla (nth n1 la-lst))
      (setq k1 0 key$ 0)
      (setq sleng (strlen oldla))
      (while (and (/= key$ "$") (> sleng 3))
         (setq key$ (substr oldla (- sleng k1) 1))
         (if (= key$ "$") (setq newla (substr oldla (- sleng (- k1 1)) k1)))
         (setq k1 (1+ k1))
      )
      (setq ss1 (ssget "x" (list (cons 8 oldla))))
      (setq la (tblsearch "layer" newla))
;->1.대상이 있고 레이어 있으면
      (if (and (/= ss1 nil) (/= la nil) )(progn
          (command "change" ss1 "" "p" "la" newla "")
          (setq j1 (1+ j1))
      ))
      (setq ss1 (ssget "x" (list (cons 8 oldla))))
;->2.대상이 없고 레이어 있으면
      (if (and (= ss1 nil) (/= la nil) )(progn
          (setq nla newla)
          (while (/= la nil)
              (setq nla (strcat nla "!"))
              (setq la (tblsearch "layer" nla))
          )
          (command "rename" "la" oldla nla)
          (setq j3 (1+ j3))
      ))
;->3.레이어 없으면
      (if (= (tblsearch "layer" newla) nil)(progn
          (command "rename" "la" oldla newla)
          (setq j2 (1+ j2))
      ))
      (setq n1 (1+ n1))
   )
   (command "purge" "la" "*" "n")
   (prompt "..$0$..표시가 포함된 레이어<") (prin1 (- listnum 1))
   (prompt "개>을 정리하였습니다.")(terpri)
   (prompt "layer change=")(princ j1) (prompt "개 / layer rename=")(princ j2)(prompt "개 / block layer<!>=")(princ j3)
   (princ)
)


;;========================================================
;  Layer Rename (1998.04 / 2006.10 주말농부)
;  ->현재 레이어의 이름을 바꾸는 명령어
;    기존에 레이어가 있으면 뒤에 !표시를 추가하여 변경함
;    중복된 레이어을 정리할때 사용하는 명령어
;  ->default값 설정(2007.01.08)
;;--------------------------------------------------------
(defun c:13n (/ la cl nla ss1 en1 elist oldla newla sleng k1 key$)
   (setq la nil nla nil)
   (setq ss1 (entsel "\n레이어선택<현재레이어>:"))(terpri)
   (if (/= ss1 nil)(progn
      (setq en1 (car ss1)) ;entity name
      (setq elist (assoc 8 (entget en1))) ;layer name list
      (setq cl (cdr elist)) ;layer name
      )
      (setq cl (getvar "clayer"))
   );if end
   (prompt "Old layer name : ")(princ cl)
   (setq oldla cl)
   (setq sleng (strlen oldla))
   (setq k1 0 key$ 0)
(if (= (wcmatch oldla "*$*") T)(progn
   (while (and (/= key$ "$") (> sleng 2))
      (setq key$ (substr oldla (- sleng k1) 1))
      (if (= key$ "$") (setq newla (substr oldla (- sleng (- k1 1)) k1)))
      (setq k1 (1+ k1))
   )
   )
   (setq newla oldla)
)
   (prompt "\nNew layer name<")(princ newla)(prompt"> :")
   (setq nla (strcase (getstring)))
   (if (= nla "") (setq nla newla))
   (setq la (tblsearch "layer" nla))
   (while (/= la nil)
       (setq nla (strcat nla "!"))
       (setq la (tblsearch "layer" nla))
       (prompt nla)
   )
   (command "rename" "la" cl nla)
   (prompt "\n레이어 변경 : ")(princ cl) (prompt " -> ") (princ nla)
   (prin1)
)

;;=====================================================
;  색상바꾸기(2002.03 주말농부)
;  ->현재레이어 또는 선택객체의 색상을 바꾸는 명령어
;  ->선택객체바꾸기 추가(2008.1.6)
;;------ color change ---------------------------------
(defun c:13c (/ cl ss ocn ncn)
   (setq cl (getvar "clayer"))
   (prompt "\n색상변경할 객체선택<현재레이어 변경>->")
   (setq ss (ssget))
   (if ss
      (progn
         (setq ocn 256)
         (setq ncn (acad_colordlg ocn))
         (if (= ncn 256) (setq ncn "bylayer"))
         (if (= ncn 0) (setq ncn "byblock"))
         (vl-cmdf "change" ss "" "p" "c" ncn "")
      )
      (progn
         (setq entl (tblsearch "layer" cl))
         (setq ocn (cdr (assoc 62 entl)))
         (setq ncn (acad_colordlg ocn nil))
         (vl-cmdf "layer" "c" ncn cl "")
      )
   )
   (prin1)
)    

;;LINETYPE CHANGE (92cho_I)
(defun c:13l (/ ss lc ln)
    (prompt "\nCommand: Linetype Change...")
    (prompt "\nSelect objects to change ->")
    (setq ss (ssget))
    (setq ln (assoc 6 (entget (car
      (entsel "\nPoint to entity on target layer ->")
    ))))
    (setq lc (cdr ln))
    (if (= lc nil)
        (setq lc "bylayer"))
    (command "change" ss "" "p" "lt" lc "")
    (princ)
)

;;layer CHANGE (color no)
(defun c:13d (/ ents ent entl laname)
  (command "undo" "be")
  (setvar "cmdecho" 0)
  (setq cl (getvar "clayer"))
  (prompt " (color, ltype) Bylayer를 레이어속성에 맞추어 변경하기")
  (setq ents (ssget ":L"))
  (defun dxf (id lst)(cdr (assoc id lst)))
  (defun layer_cn (laname) (dxf 62 (tblsearch "layer" laname)))
  (defun layer_lt (laname) (dxf 6  (tblsearch "layer" laname)))
  (setq i 0)
  (repeat (sslength ents)
    (setq entl (entget (setq ent (ssname ents i))))
    (if (= (dxf 62 entl) nil)
      (progn
        (setq laname (dxf 8 entl))
        (setq entl (entmod (append entl (list (cons 62 (layer_cn laname))) )))
      )
    )
    (if (= (dxf 6 entl) nil)
      (progn
        (setq laname (dxf 8 entl))
        (setq entl (entmod (append entl (list (cons 6 (layer_lt laname))))))
      )
    )
    (setq i (1+ i))
  )
(setq ss1 (entsel "\nPoint to entity on target layer<layer list>:"))
          (if (or (= ss1 0) (= ss1 nil)) (setq ss1 "clayer"))
           (setq lname (cdr (assoc 8 (entget (car ss1)))))
           (command "change" ents "" "p" "la" lname "")
  (command "undo" "end")
)
;==============선택한 객체와 같은레이어의 모든객체 선택리습 ==========================
(defun c:1as (/ ss index ent old8 layernamelist layernametxt ss2 a kw ent-s ents
              lays lay-s ent2-1 ent2-2 ent2-1s es) 
  (setq hl (getvar "highlight")) 
  (prompt "\nSelect Layer Modify...") 
  (setq p1 (getpoint "\nFirst corner :")) 
  (setq p2 (getcorner p1 "\nSecond corner :")) 
  (prompt "\n 객체 선택...") 
  (setq ent1 (ssget)) 
  (if (/= ent1 nil) 
    (progn 
      (setq ent-s (sslength ent1)) 
      (setq ents 0) 
      (setq lays 0) 
      (setq lay-s nil) 
      (setq ent2-1 (ssadd)) 
      (repeat ent-s 
        (setq lay  (cdr (assoc 8 (entget (ssname ent1 ents))))) 
        (setq lay-s (cons lay lay-s)) 
        (setq ents (+ ents 1)) 
      );repeat 
      (repeat (vl-list-length lay-s) 
        (setq ent2 (ssget "w" p1 p2 (list (cons 8 (nth lays lay-s))))) 
        (setq ent2-1s (sslength ent2)) 
        (setq es 0) 
        (repeat ent2-1s 
          (setq ent2-2 (ssname ent2 es)) 
          (setq ent2-1 (ssadd ent2-2 ent2-1)) 
          (setq es (+ es 1)) 
        );repeat 
        (setq lays (+ lays 1)) 
      );repeat 
    );progn 
  );if 
  (setq a "\n편집선택[복사(C)/이동(M)/삭제(E)/특성일치(MA)] <선택하기>:") 
  (initget "Copy Move Erase MAtchprop") 
  (setq kw (getkword a)) 
  (cond 
    ((= kw "Copy") (command "copy" ent2-1 "")) 
    ((= kw "Move") (command "move" ent2-1 "")) 
    ((= kw "Erase") (command "erase" ent2-1 "")) 
    ((= kw "MAtchprop") (command "matchprop" pause ent2-1 "")) 
  );cond 
  (setvar "highlight" 1) 
  (redraw) 
  (princ) 
);defun slm end

;==============xref 레어어 off 된거 리스트화시켜서 화면에 써주기  ==========================
(defun c:1xll (/ lt os lay cecolor s_cen v_len h_len textheight tabp tinp item layerlist layername textlens textlens2 maxlen lucp lup rup ldp rdp) 
  (princ "\n XREF레이어 리스트") 
  (setq lt (getvar "ltscale")) 
  (setq os (getvar "osmode")) 
  (setq lay (getvar "clayer")) 
  (setq cecolor (getvar "cecolor")) 
  (setq textheight (* lt 3)) 
  (setq item (tblnext "LAYER" T)) 
  (setvar "osmode" 0) 
  (setvar "cecolor" "256") 
  (setq maxlen 0) 
  (setq layerlist nil) 
  (while item 
    (setq layername (cdr (assoc 2 item))) 
    (setq onmode (cdr (assoc 62 item))) 
    (if (and (wcmatch layername "*|*") (< onmode 0)) 
      (progn 
        (setq layerlist (append layerlist (list layername))) 
        (setq textlens (car (cadr (textbox (list (cons 1 layername) (cons 40 textheight)))))) 
        (setq maxlen (max maxlen textlens)) 
      ) 
    ) 
    (setq item (tblnext "LAYER")) 
  ) 
  (if layerlist 
    (progn 
      (command "undo" "g") 
      (setvar "clayer" "0") 
      (setq tabp (getpoint "\n 좌측상단 기준점 선택하세요 : ")) 
      (if tabp 
        (progn 
          (setq textlens2 (car (cadr (textbox (list (cons 1 "X-REF OFF") (cons 40 textheight)))))) 
          (if (< maxlen textlens2) (setq maxlen textlens2)) 
          (setq lucp (list (+ (car tabp) (* (/ (+ maxlen (* textheight 2)) 2))) (cadr tabp))) 
          (setq lup (list (car tabp) (cadr tabp))) 
          (setq rup (list (+ (car tabp) (* (+ maxlen (* textheight 2)))) (cadr tabp))) 
          (setq ldp (list (car tabp) (- (cadr tabp) (* (+ (length layerlist) 2) (* textheight 1.75))))) 
          (setq rdp (list (+ (car tabp) (* (+ maxlen (* textheight 2)))) (- (cadr tabp) (* (+ (length layerlist) 2) (* textheight 1.75))))) 
          (setq tinp (list (+ (car tabp) (* textheight)) (- (cadr tabp) (* (* textheight 1.75))))) 
          (command "pline" (list (+ (car lup) (* (+ (/ textheight 2) (/ (- maxlen textlens2) 2)))) (cadr lup)) lup ldp rdp rup (list (- (car rup) (* (+ (/ textheight 2) (/ (- maxlen textlens2) 2)))) (cadr rup)) "") 
          (command "text" "j" "mc" lucp textheight 0 "X-REF OFF") 
          (foreach x layerlist 
            (setq tinp (list (car tinp) (- (cadr tinp) (* (* textheight 1.75))))) 
            (command "text" tinp textheight 0 x) 
            (setq ent (entget (entlast))) 
            (entmod (subst (cons 8 x) (assoc 8 ent) ent)) 
          ) 
        ) 
      ) 
      (command "undo" "e") 
    ) 
  ) 
  (setvar "cecolor" cecolor) 
  (setvar "clayer" lay) 
  (setvar "osmode" os) 
  (princ) 
) 

;;========== MVGUIDE(MVline을 MS공간에) ==============================================
;;; Converts current model space viewport 
;;; limits into a polyline
;;------------------------------------------------------------------------------------------------------------------
(defun C:MVln (/ prgmod)
	(setq prgmod (getmode '("TILEMODE" "CLAYER" "OSMODE")))
 	(setvar "OSMODE" 0)
 	(mvdoc)
 	(setmode prgmod)
	(alert sclstr)
	(princ)
);defun
;;-------------------------------------------------------------------------------------------------------------------
(defun mvdoc (/ curlay icnt vwctr usrent fl pt1 pt2 pt3 pt4 scl set1 vwnumb vwsize pxc xsz pyc ysz dx dy)
	
	(if (= (getvar "TILEMODE") 1)
		(progn
			(setvar "TILEMODE" 0)
	 		(command "PSPACE")
	 	);progn
	);if

	(if (= (getvar "CVPORT") 1) 
		(progn
	 		(setq fl T)
	   		(command "MSPACE")
	   		(getpoint "\nPick a viewport")
 		);progn
	);if

	(setq
  		vwctr (getvar "VIEWCTR")  	;centrepoint of viewport
	  	vwsize (getvar "VIEWSIZE") 	;height of viewport
	  	vwnumb (getvar "CVPORT")   	;no. of current viewport
	);setq

	(command "PSPACE")

	(setq
		set1 (ssget "X" '((0 . "VIEWPORT"))) 	;get selection set of viewports
		icnt -1
	);setq

	(while (and icnt (< (setq icnt (1+ icnt)) (sslength set1))) 
		(setq usrent (entget (ssname set1 icnt)))
		(if (= (dxf 69 usrent) vwnumb) (setq icnt nil))
	);while

	(setq          					
  		xsz (dxf 40 usrent) 
 	 	ysz (dxf 41 usrent)
 	 	scl (/ vwsize ysz) 
	  	sclstr (strcat   "Viewport Scale is 1:" (rtos scl 2 2))
 	 	pxc  (car vwctr)
 	 	pyc  (cadr vwctr)
 	 	dx  (* xsz scl 0.5)
 		dy  (* ysz scl 0.5)
 	 	pt1 (list (- pxc dx) (- pyc dy))
 	 	pt2 (list (+ pxc dx) (- pyc dy))
  		pt3 (list (+ pxc dx) (+ pyc dy))
 	 	pt4 (list (- pxc dx) (+ pyc dy))
	);setq

	(command "MSPACE")
	(command "LAYER" "M" "DEFPOINTS" "")
	(command "PLINE" pt1 "W" 0 0 pt2 pt3 pt4 "C")

	(if (/= (dxf 8 usrent) "VIEWPORT") 
		(entmod (subst (cons 8 "VIEWPORT") (assoc 8 usrent) usrent))
	);if

	(if fl (command "PSPACE"))  			
);defun
;;-------------------------------------------------------------------------------
(defun getmode (mod1 / mod)
	(repeat (length mod1)
		(setq 
 			mod (append mod (list (list (car mod1) (getvar (car mod1)))))
			mod1 (cdr mod1)
		);setq
	);repeat
 	mod
);defun
;;-------------------------------------------------------------------------------
(defun setmode (mod1)
	(repeat (length mod1)
	  	(setvar (caar mod1) (cadar mod1))
  		(setq mod1 (cdr mod1))
 	);repeat
);defun
;;--------------------------------------------------------------------------------
(defun dxf (code elist) (cdr (assoc code elist)))
(princ)