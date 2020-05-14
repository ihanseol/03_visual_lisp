
;; ─────  Two Point Break
(defun c:PBreak ;; B    ; Two Point Break
    (/ os SelSet pt1 pt2 ObjList)
    (vl-load-com)
    (setq AcDoc (vla-get-ActiveDocument (vlax-get-Acad-Object)))
    (setq AcLay (vla-get-Layers AcDoc))
    (vla-StartUndoMark AcDoc)
    (setvar "cmdecho" 0)
    (setq os (getvar "osmode"))
    (setvar "osmode" 37)
    (princ "\n Break")
    ;(setq SelSet (ssget (list (cons 0 "LINE,LWPOLYLINE")(cons 8 "AS-BMXM"))))
    (setq SelSet (ssget (list (cons 0 "LINE,LWPOLYLINE,ARC,CIRCLE"))))
    (setq pt1 (getpoint "\nEnter 1st Point : "))
    (setq pt2 (getpoint pt1 "\nEnter 2nd Point : "))
    (setq ObjList (vl-remove-if 'listp (mapcar 'cadr (ssnamex SelSet))))
    (setvar "osmode" 0)
    (foreach item ObjList
        (setq VObj (vlax-ename->vla-object item))
        (if (eq (vla-get-Lock (vla-item AcLay (vla-get-Layer VObj))) :vlax-false)
            (if (and
                    (eq (vla-get-ObjectName VObj ) "AcDbCircle")
                    (> (angle (vlax-get VObj  'Center) pt1)(angle (vlax-get VObj  'Center) pt2))
                    ;(< (abs (- (angle (vlax-get VObj  'Center) pt2)(angle (vlax-get VObj  'Center) pt1))) 3.14 )
                )
                (command "break" item pt2 pt1)
                (command "break" item pt1 pt2)
            )
        )
    )
    (setvar "osmode" os)
    (setvar "snapmode" 0)
    (vla-EndUndoMark AcDoc)
    (princ)
)