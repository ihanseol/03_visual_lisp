;; 도면에 기입되는 치수를 모두 특정 레이어로 설정하여 기입되게 하기

(vl-load-com)
(vlr-command-reactor nil '((:vlr-commandWillStart . start_Dimm)))
(vlr-command-reactor nil '((:vlr-commandEnded . end_Dimm)))
(vlr-command-reactor nil '((:vlr-commandCancelled . cancel_Dimm)))

(defun start_Dimm (calling-reactor start_DimmInfo / the_Dimmstart)

   (setq OldLayer (getvar "clayer"))
   (setq laynam "sDIM")
   (cond ((= (wcmatch (nth 0 start_DimmInfo) "*DIM*") T) (setvar "clayer" laynam)))

(princ)
)

(defun end_Dimm (calling-reactor end_DimmInfo / the_Dimmend)

   (cond ((= (wcmatch (nth 0 end_DimmInfo) "*DIM*") T) (setvar "clayer" OldLayer)))

(princ)
)

(defun cancel_Dimm (calling-reactor cancel_DimmInfo / the_Dimmcancel)

   (cond ((= (wcmatch (nth 0 cancel_DimmInfo) "*DIM*") T) (setvar "clayer" OldLayer)))

(princ)
)

(princ)


