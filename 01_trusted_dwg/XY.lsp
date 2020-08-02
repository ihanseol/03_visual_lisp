(vl-load-com)
(defun makeLayer (newlayername newlayerColor)
	(entmake	
		(list 
			(cons 0 "LAYER")
			(cons 100 "AcDbSymbolTableRecord")
            (cons 100 "AcDbLayerTableRecord")
            (cons 2  newlayername)				;layer name
            (cons 70 0)
			(cons 62 newlayerColor)				;layer color
		)
	)
)

(defun maketextstyle (newstylename)
	(entmake
		(list
			(cons 0 "style")
			(cons 100 "AcDbSymbolTableRecord") 
			(cons 100 "AcDbTextStyleTableRecord") 
			(cons 2 newstylename)						;style name
			(cons 3 "arial.ttf")						;font
			;(cons 4 "whgtxt.shx")						;bigfont
			(cons 70 0)
			(cons 40 0)									;fixed height / 0=non
			(cons 41 1)									;width factor
			(cons 50 0)									;oblique angle
			(cons 71 0)									;2=mirrored in X / 4=mirrored in Y
		)
	)
)

(defun searchmleaderstyle (mleaderstylename / mleaderstyledict searchresult)
	(if
		(setq mleaderstyledict (dictsearch (namedobjdict) "acad_mleaderstyle"))
		(foreach x mleaderstyledict
			(if 
				(and
					(= (car x) 3)
					(eq (strcase (cdr x)) (strcase mleaderstylename))
				)
				(setq searchresult t)
			)
		)
	)
searchresult
)


(defun makemleaderstyle (newmleadername newmleadertextstyle)
	(setq mleaderstyles (vla-item (vla-get-Dictionaries (vla-get-ActiveDocument (vlax-get-acad-object))) "ACAD_MLEADERSTYLE"))
	(setq newmleaderstyle (vla-AddObject mleaderstyles newmleadername "AcDbMLeaderStyle"))
	(vla-put-TextStyle newmleaderstyle newmleadertextstyle)
	(vla-put-TextAngleType newmleaderstyle "1")
	(vla-put-ArrowSize newmleaderstyle "100")
	(vla-put-DoglegLength newmleaderstyle "100")
	(vla-put-TextHeight newmleaderstyle "100")
	(vla-put-TextLeftAttachmentType newmleaderstyle "3")
	(vla-put-TextRightAttachmentType newmleaderstyle "3")
)


(defun c:xy( / oldlayer oldcmleaderstyle pt1 pt2 xcoordinate ycoordinate allcoordinate)
	(if
		(= (tblsearch "style" "xycoordinates") nil)
		(maketextstyle "xycoordinates")
		(princ)
	)
	(setq oldlayer (getvar "clayer"))
	(if
		(= (tblsearch "layer" "xycoordinates") nil)
		(progn
			(makelayer "xycoordinates" 1)
			(setvar "clayer" "xycoordinates")
		)
		(setvar "clayer" "xycoordinates")
	)
	(setq oldcmleaderstyle (getvar "cmleaderstyle"))

	(if
		(= (searchmleaderstyle "xycoordinates") nil)
		(progn
			(makemleaderstyle "xycoordinates" "xycoordinates")
			(setvar "cmleaderstyle" "xycoordinates")
		)
		(setvar "cmleaderstyle" "xycoordinates")
	)
	(setq pt1 (getpoint "\nClick point to measure"))
	(if pt1
		(progn
			(setq pt2 (getpoint "\nClick point to place coordinates"))
			(if pt2
				(progn
					(setq xcoordinate (strcat "X=" (rtos (cadr pt1) 2 4)))
					(setq ycoordinate (strcat "Y=" (rtos (car pt1) 2 4)))
					(setq allcoordinate (strcat xcoordinate "\n" ycoordinate))
					(command "mleader" pt1 pt2 allcoordinate)
				)
			)
		)
	)
	(setvar "clayer" oldlayer)
	(setvar "cmleaderstyle" oldcmleaderstyle)
	(princ)
)

(defun c:xyt( / newvalue)
	(setq newvalue (getint "\n input new size for text and arrow       "))
	(if
		(= newvalue nil)
		(princ "\ndo not leave new size empty!!!")
		(progn
			(if
				(= (searchmleaderstyle "xycoordinates") nil)
				(progn
					(if
						(= (tblsearch "style" "xycoordinates") nil)
						(maketextstyle "xycoordinates")
					)
					(if
						(= (tblsearch "layer" "xycoordinates") nil)
						(makelayer "xycoordinates" 1)
					)
					(if
						(= (searchmleaderstyle "xycoordinates") nil)
						(makemleaderstyle "xycoordinates" "xycoordinates")
					)
					(vla-put-ArrowSize newmleaderstyle newvalue)
					(vla-put-DoglegLength newmleaderstyle newvalue)
					(vla-put-TextHeight newmleaderstyle newvalue)
				)
				(progn
					(vla-put-ArrowSize newmleaderstyle newvalue)
					(vla-put-DoglegLength newmleaderstyle newvalue)
					(vla-put-TextHeight newmleaderstyle newvalue)
				)
			)
		)
	)
	(princ)
)
