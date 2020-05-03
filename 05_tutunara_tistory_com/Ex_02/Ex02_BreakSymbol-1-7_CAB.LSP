;;  _______________________________o o_________________________________________
;;                              ''' U '''                                      

;;;=======================[ BreakSymbol CAB.lsp ]==============================
;;; Author: Copyright?2011 Charles Alan Butler 
;;; Contact @  www.TheSwamp.org    
;;; Thread  http://www.theswamp.org/index.php?topic=37531
;;; Version: 1.7  June 17, 2011
;;; Purpose: Add Break Symbols (LWplines) to the drawing
;;;    User picks end points & then moves cursor to adjust scale
;;;    Moving the cursor past the mid point should reverse the symbol
;;;    Moving the cursor in the other direction should flip the symbol
;;;    Note that symmetrical shapes may only reverse, use symbol 3 to
;;;    see the full effects.
;;;    Enter Key is a special case, draw shape at scale 1 with filp/mirror & exit
;;;
;;;    Version 1+ uses current layer & pline width settings    
;;;    Version 1.1 At the first prompt you may enter a number to change the shape #
;;;    Version 1.2 Allow separate plines, see shape 21
;;;    Version 1.3 Allow any number of plines, see shape 21
;;;    Version 1.4 Allow user to extend the break line with + - keys
;;;    Version 1.4a Added shape 27
;;;    Version 1.5 Added variable extensions, see variable ExtType
;;;    Version 1.6 Updated DCL Layout
;;;    Version 1.7 Added shape 30
;;;    
;;;    The command line call is BrkSymDCL and starts a dialog box
;;;    The DCL code is graciously provided by Lee Mac,  www.lee-mac.com 
;;;    You can call the routine from lisp with (BrkSym <shape number> <extFactor>) 
;;;    1-30 are valid shapes at this time. If you have a favorite break symbol 
;;;    email or post it and I will add it to the routine.
;;;    A better way to call the routine:
;;;    ^C^C(BrkSym <shape number> )
;;;   
;;;    Note that <extFactor> is an extension factor used by shapes that allow the break
;;;    line to extend past the points picked, see shape 1 as an example. While scaleing
;;;    the shape press + or - keys to extend or shrink the extension of the line
;;;
;;;    Returns:  none
;;;
;;; Thanks Gary Davidson Fowler of Richardson, Texas for shape #11


;;;=====================================================================
;;;   THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED     ;
;;;   WARRANTY.  ALL IMPLIED WARRANTIES OF FITNESS FOR ANY PARTICULAR  ;
;;;   PURPOSE AND OF MERCHANTABILITY ARE HEREBY DISCLAIMED.            ;
;;;                                                                    ;
;;;  You are hereby granted permission to use, copy and modify this    ;
;;;  software without charge, provided you do so exclusively for       ;
;;;  your own use or for use by others in your organization in the     ;
;;;  performance of their normal duties, and provided further that     ;
;;;  the above copyright notice appears in all copies and both that    ;
;;;  copyright notice and the limited warranty and restricted rights   ;
;;;  notice below appear in all supporting documentation.              ;
;;;=====================================================================

(setq *BrkSymMax* 30) ; Max number of Symbols
(setq ExtType nil)  ; type of extension formula to use
(vl-load-com)

;; command line entry point
;; Global variable *BrkSymType* is used to determing the prompt or skip
;;  prompt if it is set to a valid number.
(defun c:BrkSym ()
  (if (not (numberp *BrkSymType*))
    (progn
     (setq ans (getint (strcat "\nEnter the number 1-" (itoa *BrkSymMax*) " for break symbol: ")))
     (and ans (< 0 ans (1+ *BrkSymMax*)) (setq *BrkSymType* ans))
    )
   )
  (if (numberp *BrkSymType*)
    (BrkSym *BrkSymType*)
  )
  (princ)
)

;;  Break Symbol Clear
(defun c:BSC()  
  (setq *BrkSymType* nil)
  (c:BrkSym)
  (princ)
)
    

;;  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(defun BrkSym (typ / CurSpace upe ups dist ang ent ent2 *error*
               ang1 cp dist input ump upe ps rd lr mp sf ud num extAllow) 
  ;;  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  
  ;; error function & Routine Exit
  (defun *error* (msg)
    (if (not (member msg '("console break" "Function cancelled" "quit / exit abort" "")))
       (princ (strcat "\nError: " msg))
    )
    (princ)
  )  ; end error function

  ;;  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    
  ;;  Library Routine by CAB 03/22/2009
  ;;  Expects pts to be a list of 2D or 3D points, uses current layer
  ;;  blst is a bulge list '((idx bulge) (2  0.414214) ...) or nil
  ;;  wlst is a width list '((idx start end) (2  0.0 1.5) ...) or nil
  ;;  Returns new pline object
  (defun makepline (spc pts blst wlst / norm elv pline bulge wdata)
    (setq norm (trans '(0 0 1) 1 0 t)
          elv  (caddr (trans (car pts) 1 norm))
    )
    (setq pline
           (vlax-invoke spc 'addlightweightpolyline
             (apply 'append
                    (mapcar '(lambda (pt)
                               (setq pt (trans pt 1 norm))
                               (list (car pt) (cadr pt))
                             )
                            pts))))
    (vla-put-elevation pline elv)
    (vla-put-normal pline (vlax-3d-point norm))
    (foreach bulge blst
       (vla-setBulge pline
         (car bulge)      ;; index
         (cadr bulge)     ;; bulge
       )
    )
    (foreach wdata wlst
       (vla-setWidth pline
         (car wdata)      ;; index
         (cadr wdata)     ;; start width
         (caddr wdata)    ;; end width
       )
    )
    pline
  )
  ;;  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  
  ;;D. C. Broad, Jr.
  ;;(sideof <ray-origin> <another-point-on-ray> <point-to-be-tested>)
  (defun sideof (p1 p2 p / r)
    (setq r (cond ((equal p1 p 1e-10) 0)
                  (t (sin (- (angle p1 p) (angle p1 p2))))))
    (if (equal r 0 1e-10) 0 r)
  )
  ;;return values
  ;;negative = point is to the right side of the ray
  ;;0 = point is on the ray
  ;;otherwise point is on the left side of the ray.
  ;;P1 should not equal P2 for meaningful results.
  ;;  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



  

  ;;  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  ;;  Create the Break Symbol, returns a list of pline vla-objects
  (defun makesym (typ ps mp pe ud lr sf spc / dist ang p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 
  	p12 p13 p14 rp up dn plst blist wlist)   

    ;;  DO NOT Change variable mp !!!!!!





    
    ;;D. C. Broad  -modification of Joe Burke's method  5/18/2003
    ;;arguments: pl - point list to mirror, p1 p2 - mirror points
    ;;returns a mirrored point -limited testing done.
    (defun MirrorPt (pl p1 p2 / mang)
      (setq mang (* 2 (angle p1 p2)))
      (mapcar '(lambda (p)
          (polar p1 (- mang (angle p1 p)) (distance p1 p)))
       pl
      )
    )

    ;; Calculate the extension length
    ;; ef is an positive intiger or zero
    (defun extfCalc(ef)
      (cond
        ((null ExtType)
          (cond
            ((or (not (numberp ef)) (zerop ef)) 0)
            ((* ef (* 0.125 (distance ups upe))))
          ))
        ((= ExtType 0) (* ef 20))
        ((= ExtType 1) ; use units
          (cond
            ((not (numberp ef)) 1)
            (ef)
          ))
        ((= ExtType 2)
          (cond
            ((not (numberp ef)) (* (getvar "dimscale")(getvar "dimtxt")))
            ((* ef (getvar "dimscale")(getvar "dimtxt")))
          ))
      )
    )

    
    (setq dist (distance ps pe)
          ang  (angle ps pe)
          rp   (polar mp (+ ang (/ pi 2)) 20))

    (or (numberp extf) (setq extf 0)) ; reset the extension Factor 
    (setq extAllow nil) ; reset the allow the extension flag

    
    ;;  --+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+---
    (cond
      ((= typ 0) ; debug shape, do not use
        (setq p2 (polar mp (+ pi ang) (* sf 0.184883 dist))
              p3 (polar p2 (+ 0.785398 ang) (* sf 0.130732 dist))
              p4 (polar p3 (+ 5.49779 ang) (* sf 0.130732 dist))
        )
        (setq plst (list (list ps p2 p3 p4 pe)))
             
      )

      ((= typ 1)
        (setq p2 (polar mp (+ pi ang) (* sf 0.0833006 dist))
              p5 (polar mp ang (* sf 0.0833006 dist))
              p3 (polar p2 (+ 1.25664 ang) (* sf 0.1667 dist))
              p4 (polar p5 (+ 4.39824 ang) (* sf 0.1667 dist))
              ;;  add the extensions to the base line past the picked points
              ps (polar ps (+ pi ang) (extfCalc extf))
              pe (polar pe ang (extfCalc extf))
        )
        (setq plst (list (list ps p2 p3 p4 p5 pe)))
      )
      
      ((= typ 2)
        (setq p2 (polar mp (+ pi ang) (* sf 0.0611081 dist))
              p3 (polar p2 (+ ang 3.80095)  (* sf 0.0824362 dist))
              p4 (polar p3 (+ ang 0.349066) (* sf 0.0957685 dist))
              p5 (polar p4 (+ ang 4.9383)   (* sf 0.161931 dist))
              p6 (polar p5 (+ ang 1.34488)  (* sf 0.161931 dist))
              p7 (polar p6 (+ ang 5.93412)  (* sf 0.0957685 dist))
              p8 (polar p7 (+ ang 2.48223)  (* sf 0.0824362 dist))
              ps (polar ps (+ pi ang) (extfCalc extf))
              pe (polar pe ang (extfCalc extf))
        )
        (setq plst (list (list ps p2 p3 p4 p5 p6 p7 p8 pe)))
      )
      
      ((= typ 3)
        (setq p2 (polar mp (+ pi ang) (* sf 0.103502 dist)) 
              p3 (polar p2 (+ ang 5.044) (* sf 0.445917 dist))
              p4 (polar p3 (+ ang 1.71042) (* sf 0.289812 dist))
              p5 (polar p4 (+ ang 5.53269) (* sf 0.0842185 dist))
              p6 (polar p5 (+ ang 1.79769) (* sf 0.125246 dist))
              p7 (polar p6 (+ ang 5.86431) (* sf 0.0650671 dist))
              p8 (polar p7 (+ ang 1.98968) (* sf 0.105629 dist))
              ps (polar ps (+ pi ang) (extfCalc extf))
              pe (polar pe ang (extfCalc extf))
        )
        (setq plst (list (list ps p2 p3 p4 p5 p6 p7 p8 pe)))
      )

      ((= typ 4)
        (setq p2 (polar mp (+ pi ang) (* sf 0.0711984 dist))
              p5 (polar mp ang (* sf 0.0711984 dist))
              p3 (polar p2 (+ 1.5708 ang) (* sf 0.135509 dist))
              p4 (polar p5 (+ 4.71239 ang) (* sf 0.135509 dist))
              ps (polar ps (+ pi ang) (extfCalc extf))
              pe (polar pe ang (extfCalc extf))
        )
        (setq plst (list (list ps p2 p3 p4 p5 pe)))
      )

      ((= typ 5)
        (setq p2 (polar mp (+ pi ang) (* sf 0.11442 dist))
              p5 (polar mp ang (* sf 0.11442 dist))
              p3 (polar p2 (+ 1.85801 ang) (* sf 0.162663 dist))
              p4 (polar p5 (+ 4.99961 ang) (* sf 0.162663 dist))
              ps (polar ps (+ pi ang) (extfCalc extf))
              pe (polar pe ang (extfCalc extf))
        )
        (setq plst (list (list ps p2 p3 p4 p5 pe)))
      )
     
      
      ((= typ 6)
        (setq p2 (polar mp (+ 3.14159265 ang) (* sf 0.13894840 dist))
              p3 (polar p2 (+ 0.24861912 ang) (* sf 0.26468801 dist))
              p4 (polar p3 (+ 3.72528794 ang) (* sf 0.27329273 dist))
              p5 (polar mp (+ 0.00000000 ang) (* sf 0.11760129 dist))
              ps (polar ps (+ pi ang) (extfCalc extf))
              pe (polar pe ang (extfCalc extf)))
        (setq plst (list (list ps p2 p3 p4 p5 pe)))
      )


      
      ((= typ 7)
        (setq p2 (polar mp (+ pi ang) (* sf 0.20864 dist))
              p5 (polar mp ang (* sf 0.20864 dist))
              p3 (polar p2 (+ 0.785398 ang) (* sf 0.295062 dist))
              p4 (polar p5 (+ 3.92699 ang) (* sf 0.295062 dist))
              ps (polar ps (+ pi ang) (extfCalc extf))
              pe (polar pe ang (extfCalc extf))
        )
        (setq plst  (list (list ps p2 p3 p4 p5 pe))
              blist '(((1 0.414214)(3 -0.414214))))
      )

      
      ((= typ 8)
        (setq p2 (polar mp (+ pi ang) (* sf 0.20951 dist))
              p4 (polar mp ang (* sf 0.20951 dist))
              ps (polar ps (+ pi ang) (extfCalc extf))
              pe (polar pe ang (extfCalc extf))
        )
        (setq plst  (list (list ps p2 mp p4 pe))
              blist '(((1 -1.0)(2 1.0))))
      )
      
      ((= typ 9)
        (setq p2 (polar mp (+ pi ang) (* sf 0.209405 dist))
              p3 (polar p2 (+ 1.2257 ang) (* sf 0.200362 dist))
              p4 (polar p3 (+ 0.785398 ang) (* sf 0.0522846 dist))
              p5 (polar p4 (+ 5.49779 ang) (* sf 0.0522846 dist))
              p6 mp
              p10 (polar mp ang (* sf 0.209405 dist))
              p9 (polar p10 (+ 4.36749 ang) (* sf 0.200362 dist))
              p8 (polar p9 (+ 3.92699 ang) (* sf 0.0522846 dist))
              p7 (polar p8 (+ 2.35619 ang) (* sf 0.0522846 dist))
              ps (polar ps (+ pi ang) (extfCalc extf))
              pe (polar pe ang (extfCalc extf))
       )
        (setq plst  (list (list ps p2 p3 p4 p5 p6 p7 p8 p9 p10 pe))
              blist '(((1  -0.036608) (2  -0.223252) (3  -0.220393) (4  -0.0368406)
              	        (5  0.0373227) (6  0.217898) (7  0.22187) (8  0.0377929))))
      )

      
      ((= typ 10)
        (setq p2 (polar mp (+ pi ang) (* sf 0.203218 dist))
              p4 (polar mp ang (* sf 0.203218 dist))
              ps (polar ps (+ pi ang) (extfCalc extf))
              pe (polar pe ang (extfCalc extf))
        )
        (setq plst  (list (list ps p2 mp p4 pe))
              blist '(((1 -0.54222)(2 0.54222))))
      )

      
      ;; Thanks Gary Davidson Fowler of Richardson, Texas for this shape
      ((= typ 11) 
        (setq p2 (polar mp (+ pi ang) (* sf 0.0980317 dist)) 
              p3 (polar p2 (+ ang 4.46741) (* sf 0.121257 dist))
              p4 (polar p3 ang (* sf 0.156851 dist))
              p5 (polar p4 (+ ang 1.81578) (* sf 0.242515 dist))
              p6 (polar p5 ang (* sf 0.156851 dist))
              p7 (polar mp ang (* sf 0.0980317 dist))
              ps (polar ps (+ pi ang) (extfCalc extf))
              pe (polar pe ang (extfCalc extf))
        )
        (setq plst  (list (list ps p2 p3 p4 p5 p6 p7 pe))
              blist '(((2 1.0)(4 -1.0))))
      )



      ((= typ 12)
        (setq p2 (polar mp (+ pi ang) (* sf 0.119978 dist))
              p7 (polar mp ang (* sf 0.119978 dist))
              p3 (polar p2 (+ 1.5708 ang) (* sf 0.149243 dist))
              p4 (polar p3 (+ 0 ang) (* sf 0.119978 dist))
              p6 (polar p7 (+ 4.71239 ang) (* sf 0.149243 dist))
              p5 (polar p6 (+ pi ang) (* sf 0.119978 dist))
              ps (polar ps (+ pi ang) (extfCalc extf))
              pe (polar pe ang (extfCalc extf))
        )
        (setq plst (list (list ps p2 p3 p4 p5 p6 p7 pe)))
      )



      ((= typ 13)
        (setq p2 (polar mp (+ pi ang) (* sf 0.103775 dist))
              p3 (polar mp ang (* sf 0.103775 dist))
              ps (polar ps (+ pi ang) (extfCalc extf))
              pe (polar pe ang (extfCalc extf))
        )
        (setq plst  (list (list ps p2 p3 pe))
              blist '(((1 1.0))))
      )

      
      ((= typ 14)
        (setq p2 (polar mp (+ pi ang) (* sf 0.103775 dist))
              p3 (polar p2 (+ 5.79488 ang) (* sf 0.183162 dist))
              p4 (polar mp ang (* sf 0.103775 dist))
              ps (polar ps (+ pi ang) (extfCalc extf))
              pe (polar pe ang (extfCalc extf))
        )
        (setq plst (list (list ps p2 p3 p4 pe))
              blist '(((1 -1.66608)(2 -4.0141))))
      )

      
      ;|((= typ 14) ; Pipe End, no scaling
        (setq plst (list ps mp pe mp pe)
              blist (list (list 0 -0.5)(list 1 -0.5)(list 2 -0.5)(list 3 -0.5)))
      )|;

      
      ((= typ 15)
        (setq sf 1) ; scale override
        (setq p2 (polar ps (+ 1.08202 ang) (* sf 0.0569229 dist))
              p3 (polar p2 (+ 0.207172 ang) (* sf 0.190275 dist))
              p5 (polar mp (+ 5.98129 ang) (* sf 0.300663 dist))
              p6 (polar p5 (+ 0.207172 ang) (* sf 0.190275 dist))
              p7 (polar p6 (+ 1.5708 ang) (* sf 0.100515 dist))
              p8 (polar p7 (+ 2.93442 ang) (* sf 0.190275 dist))
        )
        (setq plst (list (list ps p2 p3 mp p5 p6 p7 p8 mp p5 p6 pe))
              blist '(((0 -0.249373)(1 -0.164157)(2 -0.0892296)(3 0.0892296)
                       (4 0.164157)(5 0.531819)(6 0.164157)(7 0.0892296)
                       (8 0.0892296)(9 0.164157)(10 0.249373))))
      )



      ((= typ 16) ; S shape, no scaling
        (setq plst (list (list ps mp pe))
              blist '(((0 -0.5)(1 0.5))))
      )

      ((= typ 17) ; debug shape 16 replaced
        (setq p2 (polar mp (+ pi ang) (* sf 0.303148 dist))
              p3 (polar p2 (+ 1.5708 ang) (* sf 0.0815385 dist))
              p4 (polar p2 (+ 4.71239 ang) (* sf 0.0815385 dist))
              p5 (polar mp (+ 0.0 ang) (* sf 0.303148 dist))
              p6 (polar p5 (+ 1.5708 ang) (* sf 0.0815385 dist))
              p7 (polar p5 (+ 4.71239 ang) (* sf 0.0815385 dist)))
        (setq plst (list (list p2 p3 ps p4 p2 p5 p6 pe p7 p5)))
      )

       
      ((= typ 18)
        (setq p2 (polar ps ang (* sf 0.163077 dist))
              p3 (polar p2 (+ 1.1781 ang) (* sf 0.0882565 dist))
              p4 (polar p2 (+ 5.10509 ang) (* sf 0.0882565 dist))
              p5 (polar pe (+ pi ang) (* sf 0.163077 dist))
              p6 (polar p5 (+ 1.96349 ang) (* sf 0.0882565 dist))
              p7 (polar p5 (+ 4.3197 ang) (* sf 0.0882565 dist)))
        (setq plst (list (list p2 p3 ps p4 p2 p5 p6 pe p7 p5)))
        (setq blist '(((0 -0.198909)(3 -0.198912)(5 0.1989)(8 0.198907)(9 0.668163))))
      )


      ((= typ 19)
        (setq p3 (polar mp (+ 4.31969 ang) (* sf 0.123835 dist))
              p4 (polar p3 ang (* sf 0.0473896 dist))
              p5 (polar p4 (+ 4.71239 ang) (* sf 0.15137 dist))
              p6 (polar p4 ang (* sf 0.0473896 dist))
              ps (polar ps (+ pi ang) (extfCalc extf))
              pe (polar pe ang (extfCalc extf))
        )
        (setq plst (list (list ps mp p3 p4 p5 p4 p6 mp pe)))
      )

      
      ((= typ 20)
        (setq p2 (polar mp   (+ 5.28488776 ang) (* sf 0.00000039 dist))
              p3 (polar p2   (+ 1.96349304 ang) (* sf 0.12383545 dist))
              p4 (polar p3   (+ 6.28318408 ang) (* sf 0.04738967 dist))
              p5 (polar p4   (+ 1.57079510 ang) (* sf 0.15137049 dist))
              p6 (polar p5   (+ 4.71238776 ang) (* sf 0.15137046 dist))
              p7 (polar p6   (+ 6.28318465 ang) (* sf 0.04738967 dist))
              p8 (polar p7   (+ 4.31968965 ang) (* sf 0.12383551 dist))
              p9 (polar p8   (+ 5.10508569 ang) (* sf 0.12383551 dist))
              p10 (polar p9  (+ 3.14159069 ang) (* sf 0.04738967 dist))
              p11 (polar p10 (+ 4.71238759 ang) (* sf 0.15137046 dist))
              p12 (polar p11 (+ 1.57079437 ang) (* sf 0.15137046 dist))
              p13 (polar p12 (+ 3.14159069 ang) (* sf 0.04738975 dist))
              p14 (polar mp  (+ 5.28488776 ang) (* sf 0.00000039 dist))
              ps (polar ps   (+ pi ang) (extfCalc extf))
              pe (polar pe ang (extfCalc extf)))
        (setq plst (list (list ps p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 pe)))
      )

      ((= typ 21)
        (setq p2 (polar mp (+ pi ang) (* sf 0.0760021 dist))
              p3 (polar p2 (+ 1.5708 ang) (* sf 0.131369 dist))
              p4 (polar p2 (+ 4.71239 ang) (* sf 0.131369 dist))
              p5 (polar mp ang (* sf 0.076002 dist))
              p6 (polar p5 (+ 1.5708 ang) (* sf 0.131369 dist))
              p7 (polar p5 (+ 4.71239 ang) (* sf 0.131369 dist))
              ps (polar ps (+ pi ang) (extfCalc extf))
              pe (polar pe ang (extfCalc extf)))
        (setq plst (list (list ps p2 p3 p2 p4) (list pe p5 p6 p5 p7)))
        (setq blist '(((1 -0.5)(2 0.5)(3 -0.5)(4 -1.0))
                      ((1 -0.5)(2 0.5)(3 -0.5)(4 -1.0))))
      )

      ((= typ 22)
        (setq p2 (polar mp (+ pi ang) (* sf 0.181567 dist))
              p3 (polar p2 (+ 5.25304 ang) (* sf 0.223704 dist))
              p4 (polar p3 (+ 1.27721 ang) (* sf 0.428486 dist))
              p5 (polar mp  ang (* sf 0.148015 dist)))
        (setq plst (list (list ps p2 p3 p4 p5 pe)))
       
      
        (setq mp2 (polar mp (+ 4.71239 ang) (* sf 0.138685 dist))
              p2  (polar mp2 (+ pi ang) (* sf 0.169757 dist))
              p3  (polar p2  (+ 5.25304 ang) (* sf 0.236959 dist))
              p4  (polar p3  (+ 1.27721412 ang) (* sf 0.395271 dist))
              p5  (polar mp2  ang (* sf 0.139158 dist)))
        (setq plst (append plst (list(list (polar ps (+ 4.71239 ang) (* sf 0.138685 dist))
                          p2 p3 p4 p5
                          (polar pe (+ 4.71239 ang) (* sf 0.138685 dist))))))
      )



      ((= typ 23)
        (setq p2 (polar mp (+ pi ang) (* sf 0.0117064 dist))
              p3 (polar p2 (+ 2.30107 ang) (* sf 0.113934 dist))
              p4 (polar p3 (+ 0.840526 ang) (* sf 0.113934 dist))
              p5 (polar p2 (+ 3.98212 ang) (* sf 0.113934 dist))
              p6 (polar p5 (+ 5.44266 ang) (* sf 0.113934 dist))
              ps (polar ps (+ pi ang) (extfCalc extf)))
        (setq plst (list (list ps p2) (list p4 p3 p2 p5 p6)))
      
        (setq p2 (polar mp ang (* sf 0.0642957 dist))
              p3 (polar p2 (+ 2.30107 ang) (* sf 0.113934 dist))
              p4 (polar p3 (+ 0.840526 ang) (* sf 0.113934 dist))
              p5 (polar p2 (+ 3.98212 ang) (* sf 0.113934 dist))
              p6 (polar p5 (+ 5.44266 ang) (* sf 0.113934 dist))
              pe (polar pe ang (extfCalc extf)))
        (setq plst (append plst (list (list p4 p3 p2 p5 p6)(list p2 pe))))
      )

      ((= typ 24)
        (setq p2 (polar mp (+ pi ang) (* sf 0.0320268 dist))
              p3 (polar p2 (+ 1.1781 ang) (* sf 0.137991 dist))
              p4 (polar p3 (+ 4.31969 ang) (* sf 0.275982 dist))
              ps (polar ps (+ pi ang) (extfCalc extf)))
        (setq plst (list (list ps p2 p3 p4)))
      
        (setq p2 (polar mp ang (* sf 0.0320268 dist))
              p3 (polar p2  (+ 1.1781 ang) (* sf 0.137991 dist))
              p4 (polar p3  (+ 4.31969 ang) (* sf 0.275982 dist))
              pe (polar pe ang (extfCalc extf)))
        (setq plst (append plst (list(list pe p2 p3 p4))))
      )

      ((= typ 25)
        (setq p2 (polar mp (+ 1.32719 ang) (* sf 0.21395 dist))
              p3 (polar p2 (+ 4.19573 ang) (* sf 0.152334 dist))
              p4 (polar mp (+ 4.468781 ang) (* sf 0.21395 dist))
              p5 (polar p4 (+ 1.17412 ang) (* sf 0.152334 dist)))
        ;;  to keep horiz lines perfectly parallel must use up & left
       (setq up (/ pi 2)
             dn (+ up pi))
        (setq plst (list (list p3 p2 p4 p5)
            (list (polar ps (+ up ang)  (* sf 0.0455395 dist))
                  (polar (polar mp (+ up ang) (* sf 0.0455395 dist)) (+ pi ang) (* sf 0.0281497 dist)))
            (list (polar ps (+ dn ang) (* sf 0.0455395 dist))
                  (polar (polar mp (+ dn ang) (* sf 0.0455395 dist)) (+ pi ang) (* sf 0.0758733 dist)))
            (list (polar pe (+ up ang)  (* sf 0.0455395 dist))
                  (polar (polar mp (+ up ang) (* sf 0.0455395 dist)) ang (* sf 0.070617 dist)))
            (list (polar pe (+ dn ang) (* sf 0.0455395 dist))
                  (polar (polar mp (+ dn ang) (* sf 0.0455395 dist)) ang (* sf 0.0354313 dist)))))
      )


      ((= typ 26)
       (setq dn (* pi 1.5))       
       (setq p2 (polar pe (+ dn ang) (* sf 0.155119 dist))
             p3 (polar ps (+ dn ang) (* sf 0.23297 dist))
             p4 (polar mp (+ dn ang) (* sf 0.0779317 dist)))
        (setq plst (list (list ps p2)(list p3 p4)))
      )

      
      ((= typ 27) ; shape #1 doubled at 1/3 intervals
         (setq mp (polar ps ang (/ dist 3.))
              p2 (polar mp ang (* sf 0.0416497 dist))
              p3 (polar p2 (+ 4.39824 ang) (* sf 0.08335 dist))
              p4 (polar p3 (+ 1.76867 ang) (* sf 0.161697 dist))
              p5 (polar p4 (+ 4.39824 ang) (* sf 0.08335 dist))
        )
        (setq mp (polar pe (+ pi ang) (/ dist 3.))
              p6 (polar mp (+ pi ang) (* sf 0.0416497 dist))
              p7 (polar p6 (+ 1.25664 ang) (* sf 0.08335 dist))
              p8 (polar p7 (+ 4.91027 ang) (* sf 0.161697 dist))
              p9 (polar p8 (+ 1.25664 ang) (* sf 0.08335 dist))
              ps (polar ps (+ pi ang) (extfCalc extf))
              pe (polar pe ang (extfCalc extf))
        )
       (setq plst (list (list ps p5 p4 p3 p2 p6 p7 p8 p9 pe)))
      )

      ;; * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      ;;  test shape for mutiple copies of shape #1
      ;;  for now we will use + - keys to add & remove shapes
      ;;  prevent zero shapes
      ((= typ 28)
       (setq extf (max extf 1))
       (setq sf (/ sf extf)
             step (/ dist (1+ extf))
             td 0
             plst (list ps))
       (while (< (setq td (+ td step)) dist)
         (setq p10 (polar ps ang td))
         (setq p2 (polar p10 (+ pi ang) (* sf 0.0833006 dist))
               p5 (polar p10 ang (* sf 0.0833006 dist))
               p3 (polar p2 (+ 1.25664 ang) (* sf 0.1667 dist))
               p4 (polar p5 (+ 4.39824 ang) (* sf 0.1667 dist))
         )
         (setq plst (append plst (list p2 p3 p4 p5)))
       )
       (setq plst (list(append plst (list pe))))
      )
      ;; * * * * * * * * * * * * * * * * * * * * * * * * * * * *

      
      ((= typ 29)
        (setq p2 (polar mp (+ pi ang) (* sf 0.14692117 dist)))
        (setq p3 (polar mp (+ 1.76744 ang) (* sf 0.240523 dist)))
        (setq p4 (polar mp (+ 1.63675 ang) (* sf 0.285938 dist)))
        (setq p5 (polar mp (+ 1.59884 ang) (* sf 0.296692 dist)))
        (setq p6 (polar mp (+ 1.56029 ang) (* sf 0.295311 dist)))
        (setq p7 (polar mp (+ 1.5344 ang) (* sf 0.267725 dist)))
        (setq p8 (polar mp (+ 1.52622 ang) (* sf 0.246182 dist)))
        (setq p9 (polar mp (+ 1.51004 ang) (* sf 0.111573 dist)))

        (setq plst (list ps p2 p3 p4 p5 p6 p7 p8 p9 mp))
        (setq p2 (polar mp ang (* sf 0.14692117 dist)))
        (setq p3 (polar mp (+ 4.90904 ang) (* sf 0.240523 dist)))
        (setq p4 (polar mp (+ 4.77834 ang) (* sf 0.285938 dist)))
        (setq p5 (polar mp (+ 4.74043 ang) (* sf 0.296692 dist)))
        (setq p6 (polar mp (+ 4.70188 ang) (* sf 0.295311 dist)))
        (setq p7 (polar mp (+ 4.676 ang) (* sf 0.267725 dist)))
        (setq p8 (polar mp (+ 4.66781 ang) (* sf 0.246182 dist)))
        (setq p9 (polar mp (+ 4.65163 ang) (* sf 0.111573 dist)))

        (setq plst (list (append plst (list p9 p8 p7 p6 p5 p4 p3 p2 pe))))

        (setq blist '(((1 -0.01899010)(2 -0.03919428)(3 -0.07276893)(4 -0.50901243)
                       (5 -0.09869613)(6 -0.01590600)(7 -0.02056094)(10 0.02056094)
                       (11 0.01590600)(12 0.09869613)(13 0.50901243)(14 0.07276893)
                       (15 0.03919428)(16 0.01899010))))
      )



      ((= typ 30)
        (setq ps (polar mp (+ 3.141593 ang) (* sf 0.500000 dist))
              p2 (polar mp (+ 3.035661 ang) (* sf 0.461079 dist))
              p3 (polar mp (+ 2.765996 ang) (* sf 0.386967 dist))
              p4 (polar mp (+ 1.951664 ang) (* sf 0.230708 dist))
              p5 (polar mp (+ 1.087343 ang) (* sf 0.148903 dist))
              p6 (polar mp (+ 0.308494 ang) (* sf 0.117823 dist))
              p7 (polar mp (+ 3.410003 ang) (* sf 0.117823 dist)))
        (setq plst (list (list ps p2 p3 p4 p5 p6 p7)))
        (setq blist '(((0 -0.00648556)(1 -0.0556546)(2 -0.195221)(3 -0.196063)
                       (4 -0.0817012)(5 -1.0)(6 -0.742172))))
        (setq wlist (list (list (list 0 0 (* sf 1.5)))))
       

        (setq pe (polar mp (+ 6.283185 ang) (* sf 0.500000 dist))
              p2 (polar mp (+ 6.177100 ang) (* sf 0.461079 dist))
              p3 (polar mp (+ 5.907588 ang) (* sf 0.386967 dist))
              p4 (polar mp (+ 5.093256 ang) (* sf 0.230708 dist))
              p5 (polar mp (+ 4.129306 ang) (* sf 0.147559 dist))
              p6 (polar mp (+ 3.410002 ang) (* sf 0.117823 dist))
              p7 (polar mp (+ 0.308496 ang) (* sf 0.117823 dist)))
        (setq plst (append plst (list (list pe p2 p3 p4 p5 p6 p7))))
        (setq blist (append blist '(((0 -0.00648556)(1 -0.0556546)(2 -0.195221)(3 -0.196063)
                       (4 -0.0817012)(5 -1.0)(6 -0.742172)))))
        (setq wlist (append wlist (list (list (list 0 0 (* sf 1.5))))))
      )




      
      ((princ "\nShape Missing..."))

       
      ;;  --+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+---
    ) ; cond

    ;; allow the extension function
    (setq extAllow (vl-position typ '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 19 20 21 23 24 26 27 28))) 

    
    (if plst
      (progn
       (if (not ud)
        (setq plst (mapcar (function (lambda(x) (MirrorPt x ps pe))) plst))
       )
       (if (not lr)
        (setq plst (mapcar (function (lambda(x) (MirrorPt x mp rp))) plst))
       )
       (if (and blist (or (and ud (not lr)) (and (not ud) lr))) ; mirror the blist
         (setq blist (mapcar(function(lambda(x)(mapcar(function(lambda(y)(list (car y) (-(cadr y))))) x))) blist))
       )
       (setq idx -1)
       (mapcar (function (lambda(x)(setq idx (1+ idx))
                           (makepline spc x (if blist (nth idx blist)) (if wlist (nth idx wlist))))) plst)
      )
    ) 
  ) ; defun



  
  ;; CAB
  (defun txt2num  (txt)
    (cond ((distof txt 5))
          ((distof txt 2))
          ((distof txt 1))
          ((distof txt 4))
          ((distof txt 3))
          (T nil)))


  

  
  ;;  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ;;         S T A R T    H E R E          
  ;;  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ;;                                       
  ;;                __                     
  ;;               /  )   __               
  ;;              /  (   (  \              
  ;;             (    )   )  \             
  ;;              oooO   (    )            
  ;;                      Oooo             
  ;;                                       

  ;;  Loop for user selections of points
  ;;  Number keys change shape number
  ;;  ENTER only exists
  (while
    (progn
      (setvar "ErrNo" 0) ; reset variable
      (initget 128)
      (setq ups (getpoint (strcat "Starting point of break line (Shape #" (itoa typ) "): ")))
      
      (cond
        ((= (getvar "errno") 52) ; enter key
         nil ; exit loop
         )
        ((null ups) (princ "\nNothing entered."))
        ((and (not (vl-consp ups))(setq num (txt2num ups))) ; change shape number
         (if (< 0 num (1+ *BrkSymMax*))
           (setq typ (fix num) *BrkSymType* typ)
           (princ "\nNumber out of range.")
         )
        )
        ((and (vl-consp ups)
              (vl-consp (setq upe (getpoint ups "\nPick end point: Move cursor after."))))
         ;;  Got two points
         nil
        )
        ((and (listp ups) (null upe)) (princ "\nBad 2nd point."))
        
        ((princ "\nInvalid entry."))
      )
      
    )
  )

  ;;  Make shape, cursor position scales & flipps shape
  ;;  + or - mat extend the break line if allowed
  (if (and ups upe)
    (progn
      (princ "\nPress any key to keep original, pick point to keep shown. ")
      (setq dist  (distance ups upe)
            ang1  (angle ups upe)
            rd    (/ dist 2.)
            mp    (polar ups ang1 rd)
      )

(setq CurSpace
  (if (or (= acmodelspace (vla-get-activespace (vla-get-activedocument (vlax-get-acad-object))))
          (= :vlax-true (vla-get-mspace (vla-get-activedocument (vlax-get-acad-object)))))
      (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object)))
      (vla-get-paperspace (vla-get-activedocument (vlax-get-acad-object)))
  )
)
      ;|  Error in paper space - ACAD 2006
      (setq CurSpace
        (if (= 1 (getvar "CVPORT"))
          (vla-get-paperspace  (vla-get-activedocument (vlax-get-acad-object)))
          (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object)))))
|;
      (while                            ; stay in loop until one of the COND statements return nil
        (progn
          (setq input (vl-catch-all-apply 'grread (list t (+ 1 2 8) 0)))

          (cond
            ((vl-catch-all-error-p input)
             (and ent (mapcar 'vla-delete ent))       ; delete previous entity
             ;(and ent (vla-delete ent))
             ;(and ent2 (vla-delete ent2))
             (princ "\nUser Quit ")
             nil                        ; exit
            )

            ((= 5 (car input))          ; pointing device
               ;;  determine which quadrant the cursor is in.
               ;;  draw the symbol for that quadrant and scaled relative to distance from midpoint
                (and ent (mapcar 'vla-delete ent))       ; delete previous entity
                (setq cp (cadr input)            ; cursor point
                      sf (/ (distance cp mp) rd) ; scale factor
                )
                ;;  Test cursor point relative to MidPoint
                ;;  check for above or below break line
                (setq ud (minusp (sideof ups upe cp))) ; minusp = normal, else reverse ps pe
                (setq lr (minusp (sideof mp (polar mp (+ ang1 (/ pi 2)) 20) cp)))
                (setq ent (makesym typ ups mp upe ud lr sf CurSpace))
             t                          ; stay in loop
            )


            ;;  Enter Key is a special case, draw shape at scale 1 with filp/mirror & exit
            ((and (= 2 (car input)) (= (setq key (cadr input)) 13)) ; [ENTER]
               ;;  determine which quadrant the cursor is in.
               ;;  draw the symbol for that quadrant 
                (and ent (mapcar 'vla-delete ent))       ; delete previous entity
                (setq cp (if cp cp mp)             ; cursor point
                      sf 1)                        ; scale factor
                ;;  Test cursor point relative to MidPoint
                ;;  check for above or below break line
                (setq ud (minusp (sideof ups upe cp))) ; minusp = normal, else reverse ps pe
                (setq lr (minusp (sideof mp (polar mp (+ ang1 (/ pi 2)) 20) cp)))
                (setq ent (makesym typ ups mp upe ud lr sf CurSpace))
             nil                        ; exit in loop
            )



            ((= 2 (car input))          ; Keyboard input
             (cond
               ((and (not extAllow) (or (= 43 key)(= 45 key)))
                (princ "\nEntension function not allowed for this shape."))

               ((= 43 key) ; + increment the extf variable
                (cond
                  ((not (numberp extf)) (setq extf 1))
                  ((setq extf (1+ extf)))
                )
               )
               
               ((= 45 key) ; - deincrement the extf variable
                (cond
                  ((not (numberp extf)) (setq extf 0))
                  ((setq extf (max 0 (1- extf ))))
                )
               )
               
               
               (t
                (and ent (mapcar 'vla-delete ent))       ; delete previous entity
                (makesym typ ups mp upe t t 1 CurSpace) ;  restore original break symbol
                nil                        ; exit in loop
               )
             )
            )

            ((= 3 (car input))          ; Selected 3d point
             nil                        ; exit in loop
            )
          )                             ; cond
        )                               ; progn
      )                                 ; while
    )                                   ; progn
  )
  (*error* "")
  (gc)(gc)
  (princ)
)
(prompt "\nBreak Symbol Creator loaded.  Type BrkSym to run it.")
(princ)


;;  >-=((((? ?父.反???反??.. >-=((((? ?父.反???反??.. >-=((((? 
;;  DCL interface  
;;  by Lee Mac at www.lee-mac.com 
;;  Thanks Lee

(defun c:BrkSymDCL ( / *error* _GetSavePath _WriteDCL dch dclfname imgdata path result versionnumber )

  (setq VersionNumber "1.7")
  
  (defun *error* ( msg )
    (if dch (unload_dialog dch))
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )

  (setq imgdata
   '(
      ("01"
        (058 053 046 041 000)
        (025 040 009 025 025)
        (100 058 053 046 041)
        (025 025 040 009 025)
        (001 001 001 001 001)
      )
      ("02"
        (056 062 053 050 046 037 043 000)
        (024 030 026 042 026 030 025 025)
        (100 056 062 053 050 046 037 043)
        (024 024 030 026 042 026 030 025)
        (002 002 002 002 002 002 002 002)
      )
      ("03"
        (052 055 051 053 050 052 044 000)
        (025 030 028 035 032 048 025 025)
        (100 052 055 051 053 050 052 044)
        (025 025 030 028 035 032 048 025)
        (003 003 003 003 003 003 003 003)
      )
      ("04"
        (057 057 042 042 000) (025 011 038 025 025) (100 057 057 042 042)
        (025 025 011 038 025) (004 004 004 004 004)
      )
      ("05"
        (061 066 033 038 000)
        (025 009 040 025 025)
        (100 061 066 033 038)
        (025 025 009 040 025)
        (006 006 006 006 006)
      )
      ("06"
        (061 038 061 036 000)
        (025 016 031 025 025)
        (100 061 038 061 036)
        (025 025 016 031 025)
        (001 001 001 001 001)
      )
      ("07"
        (048 047 046 043 041 038 035 032 029 050 051 052 053 056 058 061 064 067 070 070 050 000 049)
        (039 036 033 031 028 027 026 025 025 007 010 013 016 018 021 022 023 024 025 025 045 025 042)
        (049 048 047 046 043 041 038 035 032 050 050 051 052 053 056 058 061 064 067 100 050 029 050)
        (042 039 036 033 031 028 027 026 025 004 007 010 013 016 018 021 022 023 024 025 004 025 045)
        (002 002 002 002 002 002 002 002 002 002 002 002 002 002 002 002 002 002 002 002 002 002 002)
      )
      ("08"
        (029 031 033 036 039 042 045 047 049 050 070 068 066 063 060 057 054 052 050 050 070 000)
        (028 031 033 034 035 034 033 031 028 025 021 018 016 015 014 015 016 018 021 025 025 025)
        (029 029 031 033 036 039 042 045 047 049 070 070 068 066 063 060 057 054 052 050 100 029)
        (025 028 031 033 034 035 034 033 031 028 025 021 018 016 015 014 015 016 018 021 025 025)
        (003 003 003 003 003 003 003 003 003 003 003 003 003 003 003 003 003 003 003 003 003 003)
      )
      ("09"
        (046 046 047 048 048 049 050 070 000 036 036 036 036 037 037 038 038 039 039 040 040 040 041 041 042 042 042 043 043
         062 062 061 061 060 060 070 069 069 068 067 067 066 065 064 064 029 030 030 031 032 032 033 034 035 035 044 044 045
         055 055 054 053 053 052 051 051 050 050 059 059 059 058 058 057 057 057 056 056 063 063 063 063)
        (036 034 032 030 028 026 025 025 025 044 044 045 045 046 046 046 047 047 047 047 047 046 046 046 045 045 044 044 043
         003 003 003 002 002 002 023 021 019 017 015 013 011 009 007 006 026 028 030 032 034 036 038 040 042 043 042 040 038
         007 009 011 013 015 017 019 021 023 024 002 002 003 003 003 004 004 005 005 006 005 005 004 004)
        (045 046 046 047 048 048 049 100 029 035 036 036 036 036 037 037 038 038 039 039 040 040 040 041 041 042 042 042 043
         063 062 062 061 061 060 070 070 069 069 068 067 067 066 065 064 029 029 030 030 031 032 032 033 034 035 043 044 044
         056 055 055 054 053 053 052 051 051 050 060 059 059 059 058 058 057 057 057 056 064 063 063 063)
        (038 036 034 032 030 028 026 025 025 043 044 044 045 045 046 046 046 047 047 047 047 047 046 046 046 045 045 044 044
         004 003 003 003 002 002 025 023 021 019 017 015 013 011 009 007 025 026 028 030 032 034 036 038 040 042 043 042 040
         006 007 009 011 013 015 017 019 021 023 002 002 002 003 003 003 004 004 005 005 006 005 005 004)
        (004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004
         004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004
         004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004)
      )
      ("10"
        (048 046 044 042 039 037 035 033 031 029 051 053 055 057 060 062 064 066 068 070 070 000)
        (023 021 020 019 019 019 020 021 023 024 026 028 029 030 030 030 029 028 026 025 025 025)
        (050 048 046 044 042 039 037 035 033 031 050 051 053 055 057 060 062 064 066 068 100 029)
        (025 023 021 020 019 019 019 020 021 023 024 026 028 029 030 030 030 029 028 026 025 025)
        (006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006)
      )
      ("11"
        (052 051 049 047 045 042 040 038 037 037 047 048 062 062 059 062 052 040 000 057 050 052 054 059 061)
        (010 008 006 005 005 005 006 008 010 013 039 041 039 036 025 036 013 025 025 044 043 044 044 043 041)
        (052 052 051 049 047 045 042 040 038 037 047 047 061 062 100 059 047 037 040 054 048 050 052 057 059)
        (013 010 008 006 005 005 005 006 008 010 036 039 041 039 025 025 036 013 025 044 041 043 044 044 043)
        (001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001)
      )
      ("12"
        (062 062 050 050 037 037 000)
        (025 039 039 010 010 025 025)
        (100 062 062 050 049 037 037)
        (025 025 039 039 010 010 025)
        (002 002 002 002 002 002 002)
      )
      ("13"
        (059 058 056 053 050 046 043 041 040 039 060 000)
        (021 018 016 015 014 015 016 018 021 025 025 025)
        (060 059 058 056 053 050 046 043 041 040 100 039)
        (025 021 018 016 015 014 015 016 018 021 025 025)
        (003 003 003 003 003 003 003 003 003 003 003 003)
      )
      ("14"
        (059 058 056 053 050 046 043 041 040 039 040 041 043 046 050 053 056 058 059 060 060 000)
        (021 018 016 015 014 015 016 018 021 025 028 031 033 034 035 034 033 031 028 024 025 025)
        (060 059 058 056 053 050 046 043 041 040 039 040 041 043 046 050 053 056 058 059 100 039)
        (025 021 018 016 015 014 015 016 018 021 025 028 031 033 034 035 034 033 031 028 025 025)
        (004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004)
      )
      ("15"
        (055 058 061 063 066 069 072 075 078 080 082 084 086 088 090 092 093 095 097 097 098 098 098 099 099 099 099 099 100
         099 099 099 099 099 098 098 097 095 093 092 090 088 086 084 082 080 078 075 072 069 066 063 061 058 055 052 050 052
         038 036 033 030 027 024 021 052 055 058 061 063 066 069 072 075 078 080 082 084 086 088 090 092 093 095 097 098 098
         002 001 001 001 000 000 000 000 000 000 019 017 015 013 011 009 007 006 004 002 047 044 041)
        (027 028 029 030 031 032 032 033 033 034 034 034 033 033 033 032 031 030 030 029 029 028 028 027 027 026 026 025 025
         027 026 025 023 022 021 020 019 019 018 017 016 016 016 015 015 015 016 016 017 017 018 019 020 021 022 023 025 026
         020 019 018 017 017 016 016 026 027 028 029 030 031 032 032 033 033 034 034 034 033 033 033 032 031 030 030 029 028
         020 020 021 021 022 022 023 023 024 025 015 015 015 016 016 016 017 018 019 019 023 022 021)
        (052 055 058 061 063 066 069 072 075 078 080 082 084 086 088 090 092 093 095 097 097 098 098 098 099 099 099 099 099
         098 099 099 099 099 099 098 098 097 095 093 092 090 088 086 084 082 080 078 075 072 069 066 063 061 058 055 052 049
         041 038 036 033 030 027 024 049 052 055 058 061 063 066 069 072 075 078 080 082 084 086 088 090 092 093 095 097 098
         002 002 001 001 001 000 000 000 000 000 021 019 017 015 013 011 009 007 006 004 049 047 044)
        (026 027 028 029 030 031 032 032 033 033 034 034 034 033 033 033 032 031 030 030 029 029 028 028 027 027 026 026 025
         028 027 026 025 023 022 021 020 019 019 018 017 016 016 016 015 015 015 016 016 017 017 018 019 020 021 022 023 025
         021 020 019 018 017 017 016 025 026 027 028 029 030 031 032 032 033 033 034 034 034 033 033 033 032 031 030 030 029
         019 020 020 021 021 022 022 023 023 024 016 015 015 015 016 016 016 017 018 019 025 023 022)
        (006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006
         006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006
         006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006
         006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006)
      )
      ("16"
        (003 008 013 019 024 030 036 041 046 050 096 091 086 080 075 069 063 058 053 050)
        (029 032 035 036 037 036 035 032 029 025 020 017 014 013 012 013 014 017 020 025)
        (000 003 008 013 019 024 030 036 041 046 100 096 091 086 080 075 069 063 058 053)
        (024 029 032 035 036 037 036 035 032 029 025 020 017 014 013 012 013 014 017 020)
        (001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001)
      )
      ("17"
        (080 100 080 080 019 019 000 019 019)
        (016 025 033 025 025 016 025 033 025)
        (080 080 100 080 080 019 019 000 019)
        (025 016 025 033 025 025 016 025 033)
        (002 002 002 002 002 002 002 002 002)
      )
      ("18"
        (081 082 083 083 083 083 083 082 081 080 016 016 017 018 019 018 017 016 016 016 100 080 016 000 019)
        (031 030 028 026 025 023 021 019 018 016 026 028 030 031 033 018 019 021 023 024 025 016 025 025 016)
        (080 081 082 083 083 083 083 083 082 081 016 016 016 017 018 019 018 017 016 016 080 100 083 019 000)
        (033 031 030 028 026 024 023 021 019 018 025 026 028 030 031 016 018 019 021 023 033 025 025 033 025)
        (003 003 003 003 003 003 003 003 003 003 003 003 003 003 003 003 003 003 003 003 003 003 003 003 003)
      )
      ("19"
        (050 054 050 050 050 045 050 000)
        (025 014 014 001 014 014 025 025)
        (100 050 054 050 050 050 045 050)
        (025 025 014 014 001 014 014 025)
        (004 004 004 004 004 004 004 004)
      )
      ("20"
        (050 045 050 050 050 054 050 054 050 050 050 045 050 000)
        (025 034 034 047 034 034 025 015 015 002 015 015 025 025)
        (100 050 045 050 050 050 054 050 054 050 050 050 045 050)
        (025 025 034 034 047 034 034 025 015 015 002 015 015 025)
        (006 006 006 006 006 006 006 006 006 006 006 006 006 006)
      )
      ("21"
        (054 054 054 055 056 056 058 059 060 060 061 061 061 060 060 059 058 044 044 045 046 046 046 045 044 044 042 100 000
         039 039 040 040 041 042 041 040 040 039 039 039 040 040 041 042 056 056 055 054 054 054 055 056 056 058 056 056 055
         041 040 040 039)
        (017 018 020 021 023 024 025 037 036 035 033 032 030 029 027 026 025 037 036 035 033 032 030 029 027 026 025 025 025
         018 020 021 023 024 025 013 014 015 017 018 020 021 023 024 025 013 014 015 017 018 020 021 023 024 025 013 014 015
         013 014 015 017)
        (055 054 054 054 055 056 056 058 059 060 060 061 061 061 060 060 059 042 044 044 045 046 046 046 045 044 044 058 042
         039 039 039 040 040 041 042 041 040 040 039 039 039 040 040 041 058 056 056 055 054 054 054 055 056 056 058 056 056
         042 041 040 040)
        (015 017 018 020 021 023 024 038 037 036 035 033 032 030 029 027 026 038 037 036 035 033 032 030 029 027 026 025 025
         017 018 020 021 023 024 012 013 014 015 017 018 020 021 023 024 012 013 014 015 017 018 020 021 023 024 012 013 014
         012 013 014 015)
        (001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001
         001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001 001
         001 001 001 001)
      )
      ("22"
        (061 055 046 036 000 062 055 045 035 000)
        (031 016 047 031 031 019 002 035 019 019)
        (100 061 055 046 036 100 062 055 045 035)
        (031 031 016 047 031 019 019 002 035 019)
        (002 002 002 002 002 002 002 002 002 002)
      )
      ("23"
        (041 049 041 049 049 056 049 056 056 049)
        (033 025 017 008 033 025 017 008 025 025)
        (049 041 049 041 056 049 056 049 100 000)
        (042 033 025 017 042 033 025 017 025 025)
        (003 003 003 003 003 003 003 003 003 003)
      )
      ("24"
        (100 000 058 052)
        (025 025 012 012)
        (053 047 048 042)
        (025 025 038 038)
        (004 004 004 004)
      )
      ("25"
        (000 000 046 056 049 054 057)
        (029 021 046 005 018 029 021)
        (042 047 052 046 056 100 100)
        (029 021 031 046 005 029 021)
        (006 006 006 006 006 006 006)
      )
      ("26"
        (000 000) (025 048) (100 050) (041 033) (001 001)
      )
      ("27"
        (000 026 030 036 041 059 064 070 074)
        (025 025 039 011 025 025 039 011 025)
        (026 030 036 041 059 064 070 074 100)
        (025 039 011 025 025 039 011 025 025)
        (002 002 002 002 002 002 002 002 002)
      )
      ("28"
        (000 018 022 028 032 043 047 053 057 068 072 078 082)
        (025 025 039 011 025 025 039 011 025 025 039 011 025)
        (018 022 028 032 043 047 053 057 068 072 078 082 100)
        (025 039 011 025 025 039 011 025 025 039 011 025 025)
        (003 003 003 003 003 003 003 003 003 003 003 003 003)
      )
      ("29"
        (000 060 060 059 057 055 049 049 049 049 050 050 050 050 050 050 050 050 051 051 051 051 040 041 043 045 047 047 048 049
         049 049 049 050 050 050 050 051 051 051 051 049 049 049 050 050 049 051 050 050 051 051 051 053 053 052)
        (025 025 025 029 033 038 042 040 038 035 033 031 029 027 025 023 021 019 008 010 012 015 025 021 017 012 008 007 006 005
         004 004 004 004 004 004 005 005 006 006 007 044 044 043 046 045 045 046 046 046 045 046 046 042 043 044)
        (040 100 059 057 055 053 049 049 049 050 050 050 050 050 050 050 050 050 051 051 051 050 041 043 045 047 047 048 049 049
         049 049 050 050 050 050 051 051 051 051 051 049 049 049 050 049 049 050 050 050 051 051 051 053 052 051)
        (025 025 029 033 038 042 040 038 035 033 031 029 027 025 023 021 019 017 010 012 015 017 021 017 012 008 007 006 005 004
         004 004 004 004 004 005 005 006 006 007 008 044 043 042 045 045 044 046 046 046 046 046 046 043 044 045)
        (004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004
         004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004 004)
      )
      ("30"
        (005 005 001 099 095 095 001 001 002 002 002 003 003 004 004 004 005 006 007 008 009 009 010 011 012 014 015 017 019 022
         025 027 030 033 036 039 042 043 045 047 048 050 052 053 054 056 057 057 058 058 059 059 060 060 060 061 061 062 061 059
         057 053 050 046 043 040 099 099 098 098 098 097 097 096 096 096 095 094 093 092 091 091 090 089 088 086 085 083 081 078
         075 073 070 067 064 061 058 057 055 053 051 049 048 046 045 043 042 042 041 041 040 040 040 039 039 039 039 038 039 041
         043 047 050 054 057 059)
        (020 020 025 025 030 030 025 025 024 024 023 023 022 022 021 021 020 019 018 017 016 015 014 014 013 012 011 009 008 007
         006 005 004 004 004 004 004 004 005 005 006 006 007 008 009 011 012 013 014 015 016 017 018 019 019 020 021 025 029 032
         034 036 036 036 034 031 025 025 026 026 027 027 028 028 029 029 030 031 032 033 034 035 036 036 037 038 039 041 042 043
         044 045 046 046 046 046 046 046 045 045 044 043 042 041 040 039 037 036 035 035 034 033 032 031 030 029 028 024 021 018
         015 014 013 014 016 018)
        (005 001 005 095 095 099 001 002 002 002 003 003 004 004 004 005 006 007 008 009 009 010 011 012 014 015 017 019 022 025
         027 030 033 036 039 042 043 045 047 048 050 052 053 054 056 057 057 058 058 059 059 060 060 060 061 061 062 061 059 057
         053 050 046 043 040 039 099 098 098 098 097 097 096 096 096 095 094 093 092 091 091 090 089 088 086 085 083 081 078 075
         073 070 067 064 061 058 057 055 053 051 049 048 046 045 043 042 042 041 041 040 040 040 039 039 039 039 038 039 041 043
         047 050 054 057 059 061)
        (020 025 020 030 030 025 025 024 024 023 023 022 022 021 021 020 019 018 017 016 015 014 014 013 012 011 009 008 007 006
         005 004 004 004 004 004 004 005 005 006 006 007 008 009 011 012 013 014 015 016 017 018 019 019 020 021 025 029 032 034
         036 036 036 034 031 028 025 026 026 027 027 028 028 029 029 030 031 032 033 034 035 036 036 037 038 039 041 042 043 044
         045 046 046 046 046 046 046 045 045 044 043 042 041 040 039 037 036 035 035 034 033 032 031 030 029 028 024 021 018 015
         014 013 014 016 018 021)
        (006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006
         006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006
         006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006
         006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006 006
         006 006 006 006 006 006)
      )
    )    
  )

  ;;------------------------------------------------------------;;

  (defun _GetSavePath ( / tmp )
    (cond      
      ( (setq tmp (getvar 'ROAMABLEROOTPREFIX))

        (strcat (vl-string-right-trim "\\" (vl-string-translate "/" "\\" tmp)) "\\Support")
      )
      ( (setq tmp (findfile "ACAD.pat"))

        (vl-string-right-trim "\\" (vl-string-translate "/" "\\" (vl-filename-directory tmp)))
      )
      ( (vl-string-right-trim "\\" (vl-filename-directory (vl-filename-mktemp))) )
    )
  )

  ;;------------------------------------------------------------;;

  (defun _WriteDCL ( fname / ofile )
    
    (if (not (findfile fname))

      (if (setq ofile (open fname "w"))
        (progn
          (foreach str
           '(
             "img : image_button { width = 16.67; height = 3.85; fixed_width = true; fixed_height = true; color = 0; alignment = centered; }"
             ""
             "breaksym : dialog { key = \"dcltitle\"; spacer;"
             "  : row {"
             "    : img { key = \"01\"; }"
             "    : img { key = \"02\"; }"
             "    : img { key = \"03\"; }"
             "    : img { key = \"04\"; }"
             "    : img { key = \"05\"; }"
             "  }"
             "  : row {"
             "    : img { key = \"06\"; }"
             "    : img { key = \"07\"; }"
             "    : img { key = \"08\"; }"
             "    : img { key = \"09\"; }"
             "    : img { key = \"10\"; }"
             "  }"
             "  : row {"
             "    : img { key = \"11\"; }"
             "    : img { key = \"12\"; }"
             "    : img { key = \"13\"; }"
             "    : img { key = \"14\"; }"
             "    : img { key = \"15\"; }"
             "  }"
             "  : row {"
             "    : img { key = \"16\"; }"
             "    : img { key = \"17\"; }"
             "    : img { key = \"18\"; }"
             "    : img { key = \"19\"; }"
             "    : img { key = \"20\"; }"
             "  }"
             "  : row {"
             "    : img { key = \"21\"; }"
             "    : img { key = \"22\"; }"
             "    : img { key = \"23\"; }"
             "    : img { key = \"24\"; }"
             "    : img { key = \"25\"; }"
             "  }"
             "  : row {"
             "    : img { key = \"26\"; }"
             "    : img { key = \"27\"; }"
             "    : img { key = \"28\"; }"
             "    : img { key = \"29\"; }"
             "    : img { key = \"30\"; }"
             "  }"
             "  spacer;"
             "  : row { spacer_1;"
             "  : button { width = 12; fixed_width = true; key = \"accept\"; label = \"&Cancel\"; is_default = true; is_cancel = true; }"
             "  : button { width = 12; fixed_width = true; key = \"help\"; label = \"&Help\"; }"
             "   spacer_1; }"
             "}"
            )
            (write-line str ofile)
          )
          (setq ofile (close ofile))
          (while (not (findfile fname)))
          t)
      )
      t
    )
  )

  ;;------------------------------------------------------------;;

  (defun BrkHelp ()
    (alert
      (strcat
        "BrkSym.lsp                                          (c) 2011 Charles Alan Butler\n"
        "BrkSym Lisp adds Break Symbols (LWplines) to the drawing"
        "\nThe user selects the type of symbol from the dialog the picks end points."
        "\nThe symbol is drawn at  predetermined size & then the use can move the "
        "\ncursor to adjust the scale of the shape."
        "\nMoving the cursor past the mid point should reverse the symbol."
        "\nMoving the cursor in the other direction should flip the symbol."
        "\nNote that some shapes may not be scaled, only reverse."
        "\nWhile scaling the shape you may press + or - keys to extend or shrink "
        "\nthe extension of the line on some shapes."
        "\nPick a point to exit with the shape as shown. Enter Key will draw shape at "
        "\nscale 1 with filp/mirror & exit. Any other key will quit without a shape."
        "Please report any problems you may have.\n"
        )
    )
  )

  ;;------------------------------------------------------------;;

  (cond
    ( (not (vl-file-directory-p (setq Path (_GetSavePath))))
      (princ "\n--> Save Path not Valid.")
    )
    ( (not (_WriteDCL (setq dclfname (strcat Path "\\LMAC_BreakSym_V" (vl-string-translate "." "-" VersionNumber) ".dcl"))))
      (princ "\n--> DCL Could not be Written.")
    )
    ( (<= (setq dch (load_dialog dclfname)) 0)
      (princ "\n--> DCL File not Found.")
    )
    ( (not (new_dialog "breaksym" dch))
      (setq dch (unload_dialog dch))
      (princ "\n--> Dialog Could not be Loaded.")
    )
    (t
      (set_tile "dcltitle" (strcat "Break Line Symbol V" VersionNumber " by CAB, DCL by Lee Mac"))
     
      (foreach x imgdata
        (start_image (car x))
        (apply 'mapcar (cons 'vector_image (cdr x)))
        (end_image)
        (action_tile (car x) (strcat "(done_dialog " (car x) ")"))
      )
      (action_tile "accept" "(done_dialog 0)")
      (action_tile "help" "(BrkHelp)")
     
      (setq result (start_dialog) dch (unload_dialog dch))
    )
  )
  (if (< 0 result) (BrkSym result) (princ))
)

  


  
  ;;------------------------------------------------------------;;

