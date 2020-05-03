;;
;; ** 선택한 객체 라이타입 스케일 변경 하기 **
;; 
;; Modify : - Linetype scale 입력 받게 변경  by nadau 2009-0806 

(defun c:ct(/ #a #b #index)
(setq #a (ssget))

;(setq #b 1) ; 스케일값 1
 (setq #b (getreal "\n변경할 LineType Scale을 입력하시오 :") )

(setq #index 0)
  (if #b (repeat (sslength #a)
       (vla-put-linetypescale(vlax-ename->vla-object(ssname #a #index)) #b )
       (setq #index (1+ #index))
))(princ) );;repeat,if,defun
