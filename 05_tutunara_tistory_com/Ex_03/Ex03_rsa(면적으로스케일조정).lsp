(defun c:rsa(/ ss old_area new_area newscale bp) 
  (setq ss (entsel "\n ���������Ϸ��� ��ü : ")) 
  (if ss 
    (progn 
      (command "area" "e" ss) 
      (setq old_area (/ (getvar "area") 1000000.0)) 
      (setq new_area (getreal (strcat "\n ������<" (rtos old_area 2 4) "> : "))) 
      (if new_area 
        (progn 
          (setq newscale (sqrt (/ new_area  old_area))) 
          (setq bp (getpoint "\n �����Ϻ��� ������ : ")) 
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

