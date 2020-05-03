(defun c:MM (/ ss sslist i index linelist linenumlist ent old10 old11 dist lnum count temp total total1 subtotal txt pt)
  (vl-load-com)                                                                                            ;---->total1 추가함
  (princ "\n Eltity Length Sum")
  (setvar "cmdecho" 0)

  (setq ss (ssget (list (cons 0 "LINE,CIRCLE,ARC,POLYLINE,LWPOLYLINE,ELLIPSE,SPLINE"))))
  (setq zzz (sslength ss))
  (if ss
    (progn
      (setq index 0)
      (setq linelist nil)
      (setq linenumlist nil)
      (repeat (sslength ss)
        (setq ent (ssname ss index))
        (command "lengthen" ent "")
        (setq dist (GETVAR "PERIMETER"))
        (if (apply 'or (mapcar '(lambda (x) (equal dist x 1)) linelist))
          (progn
            (setq temp 0)
            (setq lnum nil)
            (foreach x linelist
              (if (equal dist x 1)
                (setq lnum temp)
              )
              (setq temp (1+ temp))
            )
            (setq count 0)
            (setq temp nil)
            (foreach x linenumlist
              (if (= count lnum)
                (setq temp (append temp (list (+ x 1))))
                (setq temp (append temp (list x)))
              )
              (setq count (1+ count))
            )
            (setq linenumlist temp)
          )
          (progn
            (setq linelist (append linelist (list dist)))
            (setq linenumlist (append linenumlist (list 1)))
          )
        )
        (setq index (1+ index))
      )
      (setq txt "")
      (setq count 0)
      (setq total 0)
      (setq subtotal 0)
      (repeat (length linelist)
        (if (= count 0)
          (if (> (nth count linenumlist) 1)

            ;;;;(setq txt (strcat "\nL = " (rtos (/ (nth count linelist) 1.0) 2 2) "*" (rtos (nth count linenumlist) 2 0) "개"))
            ;;;;(setq txt  (strcat "\nL = " (rtos (/ (nth count linelist) 1.0) 2 2)))

            (setq txt (strcat "\nL = " (rtos (/ (nth count linelist) 1000.0) 2 2) "*" (rtos (nth count linenumlist) 2 0) "개"))
            (setq txt  (strcat "\nL = " (rtos (/ (nth count linelist) 1000.0) 2 2)))
          )
          (if (> (nth count linenumlist) 1)

            ;;;;(setq txt (strcat txt " + " (rtos (/ (nth count linelist) 1.0) 2 2) "*" (rtos (nth count linenumlist) 2 0) "개"))
            ;;;;(setq txt (strcat txt " + " (rtos (/ (nth count linelist) 1.0) 2 2)))

            (setq txt (strcat txt " + " (rtos (/ (nth count linelist) 1000.0) 2 2) "*" (rtos (nth count linenumlist) 2 0) "개"))
            (setq txt (strcat txt " + " (rtos (/ (nth count linelist) 1000.0) 2 2)))
          )
        )
        (setq subtotal (* (/ (nth count linelist) 1) (nth count linenumlist)))
        (setq total (+ subtotal total))
        (setq total1 (/ total 1000));------------->추가함
        (setq count (1+ count))
      )
      ;;;;(setq txt (strcat txt " = " (rtos total 2 2) "m"))
      ;(setq txt (strcat txt " = " (rtos total1 2 2) "m"));------->total에서 total1 변경함
      ;(setq txt (strcat txt " : 객체수 " (rtos zzz 2 0) " 개"))
      ;(princ txt)
(if (not (setq tth (getreal (strcat "\n현재 TextSize : <" (rtos(getvar "textsize") 2 1) "> 입니다. 원하는 Text 크기를 입력하세요 : "))))
    (setq tth (getvar "textsize"))
    (setvar "textsize" tth) 
  )
      (setq pt (getpoint "\n 텍스트 삽입점 : "))

      (command "text" pt "" "" (rtos total1 2 2))
	  	  
    )
  )
  (princ)
)

