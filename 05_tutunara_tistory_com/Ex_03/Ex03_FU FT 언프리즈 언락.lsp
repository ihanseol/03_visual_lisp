(defun C:FU() ; ��ü���̾ Unlock
   (prompt " = Un-Lock Layer (*)")
   (command "Layer" "U" "*" "")(prin1))
;;;-------------------------------------------------------------------------

(defun C:FT() ; ��ü���̾ Unfreeze
   (prompt " = Un-Freeze Layer (*)")
   (command "Layer" "T" "*" "")(prin1))

