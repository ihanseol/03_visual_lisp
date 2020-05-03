;;====================================================================
;  벽체 중심선 그리기 (2007.6.주말농부)
;  ->두선의 중간에 중심선을 그리는 명령어(평행유무 관계없음)
;    라인일때만 인식됨 / 연속선택 기능
;  ->중심선 레이어 선택기능 추가(2007.7.1)


(defun C:CL (/ ss)
  (prompt "\n\t 원의 중심선 그리기             ")
  (setvar "cmdecho" 0)
  (command "layer" "s" "1" "")
  (command "undo" "be")
     ;->*error* start
  (defun *error* (msg)
    (princ "error: ")
    (princ msg)
    (command "ucs" "p")
    (princ)
  )
     ;-<*error* end
  (command "ucs" "")
  (prompt "\nSelect circle or arc to generate center line ")
  (setq ss (ssget (list (cons 0 "circle"))))
  (setq ss (remove_dubble-ent ss))
  (cenline ss)
  (command "ucs" "p")
  (command "undo" "e")
  (setvar "cmdecho" 0)  (princ)
)

;;///////////////////// 중복중심 검토 시작.
(defun remove_dubble-ent (ents    /     cir_list new_list i
     n    entl     cen_p    cen_n    rad_sz
     ent1    entl1    rad_sz1
    )
  (setq new_list nil)
  (setq i 0
 n (sslength ents)
  )
  (repeat n
    (setq ent    (ssname ents i)
   entl    (entget ent)
   cen_p    (cdr (assoc 10 entl))
   cen_n    (cdr (assoc 0 entl))
   rad_sz   (cdr (assoc 40 entl))
   cir_list (list cen_p cen_n)
    )
    (if (member cir_list new_list)
      (progn
 (setq nl1 (length new_list)
       nl2 (length (member cir_list new_list))
 )
 (setq i2 (- nl1 nl2))
 (setq ent1 (ssname ents i2))
 (setq entl1 (entget ent1))
 (setq rad_sz1 (cdr (assoc 40 entl1)))
 (if (> rad_sz rad_sz1)
   (setq ents (ssdel ent1 ents))
   (setq ents (ssdel ent ents))
 )
      )
      (setq new_list (append new_list (list cir_list))
     i      (1+ i)
      )
    )
  )
  ents
)
;;/////////////////// 중복중심검토 끝

(defun CENLINE (ss   /   cla  os   k  ed   ins  rad xins yins rad
  leng2   px1  py1  px2  py2  px3  py3 px4  py4  pp1
  pp2  pp3  pp4
        )
  (setq cla (getvar "clayer")
 os  (getvar "osmode")
  )
  (command "layer" "s" "1" "")
  (setq k 0)
  (COMMAND "OSMODE" 0)
  (repeat (sslength ss)
    (setq ed (entget (ssname ss k)))
    (setq ins  (cdr (assoc 10 ed))
   rad  (cdr (assoc 40 ed))
   xins (car ins)
   yins (cadr ins)
    )
    (cond ((<= rad 1.0) (setq leng2 0.5))
   ((and (<= rad 3.0) (> rad 1.0)) (setq leng2 1.0))
   ((and (<= rad 6.0) (> rad 3.0)) (setq leng2 1.5))
   ((and (<= rad 10.0) (> rad 6.0)) (setq leng2 2.0))
   ((and (<= rad 15.0) (> rad 10.0)) (setq leng2 2.5))
   ((and (<= rad 25.0) (> rad 15.0)) (setq leng2 5.0))
   ((and (<= rad 50.0) (> rad 25.0)) (setq leng2 6.5))
   ((and (<= rad 75.0) (> rad 50.0)) (setq leng2 10.0))
   ((and (<= rad 100.0) (> rad 75)) (setq leng2 12))
   ((and (<= rad 150.0) (> rad 100)) (setq leng2 15))
   ((and (<= rad 200.0) (> rad 150.0)) (setq leng2 20))
   ((and (<= rad 300.0) (> rad 200.0)) (setq leng2 25))
   ((and (<= rad 400.0) (> rad 300.0)) (setq leng2 35))
   ((and (<= rad 500.0) (> rad 400.0)) (setq leng2 45))
   ((and (<= rad 750.0) (> rad 500.0)) (setq leng2 60))
   ((and (<= rad 1000.0) (> rad 750.0)) (setq leng2 75))
   ((and (<= rad 1500.0) (> rad 1000.0)) (setq leng2 100))
   ((> rad 1500.0) (setq leng2 120.0))
    )     ;cond
    (setq px1 (+ (+ xins rad) leng2)
   py1 yins
   px2 (- px1 (* 2 rad) (* 2 leng2))
   py2 yins
   px3 xins
   py3 (+ (+ yins rad) leng2)
   px4 xins
   py4 (- py3 (* 2 rad) (* 2 leng2))
    )
    (setq pp1 (list px1 py1)
   pp2 (list px2 py2)
   pp3 (list px3 py3)
   pp4 (list px4 py4)
    )
    (command "layer" "s" "1" "")
    (command "line" pp1 pp2 "" "line" pp3 pp4 "")
    (setq k (1+ k))
  )     ;repeat
  (command "layer" "s" cla "")
  (command "osmode" os)
)
   

  
     