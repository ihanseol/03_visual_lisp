(defun C:FU() ; 전체레이어를 Unlock
   (prompt " = Un-Lock Layer (*)")
   (command "Layer" "U" "*" "")(prin1))
;;;-------------------------------------------------------------------------

(defun C:FT() ; 전체레이어를 Unfreeze
   (prompt " = Un-Freeze Layer (*)")
   (command "Layer" "T" "*" "")(prin1))

