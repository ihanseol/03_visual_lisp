(defun c:kk()
   (if (= jd1 nil)
      (progn
    
        (setq jd1 (getreal "\ninput text height"))
      );of progn
    );of if 

       (graphscr)
        (setvar "cmdecho" 0)
        (setq os (getvar "osmode" ))
        (setvar "osmode" 0)
        (setq p1 (getpoint "\n�������� ��ġ Ŭ��! "))
        (setq p2 (polar p1 (* pi 1.5) (* jd1 2)))
        (command "COLOR" "blue" "")
        (command "bpoly" p1 "")    ;  CHECK!
        (command "area" "o" "l" "")
        (setq e1 (* 0.000001 (getvar "area"))) 
        (setq d1 (* e1 0.3025))
        (setq dm (rtos e1 2 2))
        (setq dp (rtos d1 2 1))
        (setq bm (strcat  dm "��"))
        (setq bp (strcat  dp "��"))
        
;       (setq bn (strcat "Area=" d2 "ha"))

        (command "COLOR" "yellow" "")
        (command "text" p1 jd1 "" bm)
        (command "text" p2 jd1 "" bp)
        (setvar "osmode" os)

);


