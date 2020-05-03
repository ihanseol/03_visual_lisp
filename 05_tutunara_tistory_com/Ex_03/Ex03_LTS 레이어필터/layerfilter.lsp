(defun C:LTS ()
  (if (< (atoi (getvar "ACADVER")) 15)
    (alert "This tool requires Release 2000 or higher")
    (progn
      (vl-load-com)
      (vl-vbarun "LayerFilterDel")
    )
  )
)

(princ)
