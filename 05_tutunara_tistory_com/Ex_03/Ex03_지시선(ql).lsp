;;=======================================================
;  지시선그리기(2007.5.3 주말농부)
;  ->치수스타일 변경없이 일정규격의 지시선을 그려줌.
;  ->화살촉선택 및 지시선의 형식을 지정.
;;------ quick leader -----------------------------------
(defun c:ql(/ a kw os ldr asz clrd pt1 pt2 pt3 dw)
;->*error* start
 (defun *error* (msg)(princ "error: ")(princ msg)
 (setvar "osmode" os)(setvar "dimldrblk" ldr)
 (setvar "dimasz" asz)(setvar "dimclrd" clrd)
 (princ))
;-<*error* end
   (setq os (getvar "osmode"))
   (setq ldr (getvar "dimldrblk"))
   (setq asz (getvar "dimasz"))
   (setq clrd (getvar "dimclrd"))
   (setvar "osmode" 512)
   (setvar "dimclrd" 256) ;치수선 색상
   (prompt " 지시선그리기...")
;->화살촉 선택
   (setq a "\n화살촉 선택[작은점(1)/닫고채움(2)/건축눈금(3)/오른쪽각도(4)] <작은점(1)>: ")
   (initget "1 2 3 4")
   (setq kw (getkword a))
   (if (= kw nil) (setq kw "1"))
   (cond ((= kw "1")
          (progn (setvar "dimldrblk" "_DOTSMALL") (setvar "dimasz" 4))) ;작은점
         ((= kw "2")
          (progn (setvar "dimldrblk" ".")(setvar "dimasz" 3))) ;닫고채움
         ((= kw "3")
          (progn (setvar "dimldrblk" "_ARCHTICK") (setvar "dimasz" 2))) ;건축눈금
         ((= kw "4")
          (progn (setvar "dimldrblk" "_OPEN90")(setvar "dimasz" 3))) ;오른쪽각도
   )
;-<
   (setq pt1 (getpoint "\n지시선 시작점 지정->"))
   (setvar "osmode" 0)
   (setq pt2 (getpoint pt1 "\n다음점 지정->"))
   (setq pt3 (getpoint pt2 "\n다음점 지정->"))
   (setq dw (getstring "\n지시선 형식[직선(St)/스플라인(S)] <직선(ST)>: "))
   (if (= dw nil) (setq dw "st"))
   (if pt3
     (command "leader" pt1 pt2 pt3 "f" dw "" "" "n")
     (command "leader" pt1 pt2  "f" dw "" "" "n")
   )
   (setvar "osmode" os)
   (setvar "dimldrblk" ldr)
   (setvar "dimasz" asz)
   (setvar "dimclrd" clrd)
(prin1))
