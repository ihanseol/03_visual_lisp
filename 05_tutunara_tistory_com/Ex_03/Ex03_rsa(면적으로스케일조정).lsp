(defun c:rsa(/ ss old_area new_area newscale bp) 
  (setq ss (entsel "\n 면적을구하려는 객체 : ")) 
  (if ss 
    (progn 
      (command "area" "e" ss) 
      (setq old_area (/ (getvar "area") 1000000.0)) 
      (setq new_area (getreal (strcat "\n 새면적<" (rtos old_area 2 4) "> : "))) 
      (if new_area 
        (progn 
          (setq newscale (sqrt (/ new_area  old_area))) 
          (setq bp (getpoint "\n 스케일변경 기준점 : ")) 
          (if bp 
            (progn 
              (setq os (getvar "osmode")) 
              (setvar "osmode" 0) 
              (command "scale" ss "" bp newscale) 
              (setvar "osmode" os) 
            ) 
          ) 
        ) 
      ) 
    ) 
  ) 
  (princ) 
) 

