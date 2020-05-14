;;  PolylineJoin.lsp [command name: PJ]
;;  Joins viable objects [Lines/Arcs/LWPolylines/not-splined-or-fitted 2DPolylines]
;;    into connected LWPolylines.
;;  Rejects splined/fitted 2DPolylines, [joining them to other objects removes their
;;    spline/fit curvature] and 3DPolylines [can't be joined].
;;  If one viable object is selected, joins all other possible objects [contiguous at ends to
;;    it and to each other] to it, into one LWPolyline.
;;  If multiple viable objects are selected, joins them into as many LWPolylines as
;;    appropriate, not including anything contiguous at ends that was not selected.
;;  Leaves selected Lines/Arcs that are not contiguous to anything else as Lines/Arcs,
;;    not converted into one-segment Polylines.
;;  Concept from c:pljoin by beaufordt on AutoCAD Customization Discussion Group
;;  Streamlined by Kent Cooper, June 2011; expanded capabilities July 2012
;;  Last edited 16 July 2012

(defun C:PJ ; = Polyline Join
  (/ *error* pjss cmde peac nextent pjinit inc edata pjent)

  (defun *error* (errmsg)
    (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
      (princ (strcat "\nError: " errmsg))
    ); if
    (setvar 'peditaccept peac)
    (command "_.undo" "_end")
    (setvar 'cmdecho cmde)
    (princ)
  ); defun - *error*

  (princ "\nTo join objects into Polyline(s) [pick 1 to join all possible to it],")
  (setq
    pjss (ssget '((0 . "LINE,ARC,*POLYLINE")))
    cmde (getvar 'cmdecho)
    peac (getvar 'peditaccept)
    nextent (entlast); starting point for checking new entities
  ); setq
  (repeat (setq pjinit (sslength pjss) inc pjinit); PJ INITial-selection quantity & incrementer
    (if
      (and
        (=
          (cdr (assoc 0 (setq edata (entget (setq pjent (ssname pjss (setq inc (1- inc))))))))
          "POLYLINE" ; 2D "heavy" or 3D Polyline
        ); =
        (or
          (= (cdr (assoc 100 (reverse edata))) "AcDb3dPolyline"); 3D
          (member (boole 1 6 (cdr (assoc 70 edata))) '(2 4)); splined or fitted 2D
        ); or
      ); and
      (ssdel pjent pjss); remove 3D, splined/fitted 2D from set
    ); if
  ); repeat
  (setvar 'cmdecho 0)
  (command "_.undo" "_begin")
  (setvar 'peditaccept 1)
  (setvar 'plinetype 2); [just in case; assumes no desire to save and set back if different]
  (if pjss ; selected qualifying object(s)
    (cond ; then
      ( (= pjinit (sslength pjss) 1); selected only one, and it qualifies
        (command "_.pedit" pjss "_join" "_all" "" ""); join everything possible to it
      ); single-selection condition
      ( (> (sslength pjss) 1); more than one qualifying object
        (command "_.pedit" "_multiple" pjss "" "_join" "0.0" "")
      ); multiple qualifying condition
      ((prompt "\nSingle object not viable, or <= 1 of multiple selection viable."))
    ); cond
    (prompt "\nNothing viable selected.")
  ); outer if
  (while (setq nextent (entnext nextent)); start with first newly-created Pline, if any
    (if ; revert any un-joined Lines/Arcs back from Pline conversion
      (and ; newly-created single-segment Pline from unconnected Line/Arc
        (= (cdr (assoc 90 (entget nextent))) 2)
        (not (vlax-curve-isClosed nextent))
      ); and
      (command "_.explode" nextent)
    ); if
  ); while
  (setvar 'peditaccept peac)
  (command "_.undo" "_end")
  (setvar 'cmdecho cmde)
  (princ)
); defun

(prompt "\nType PJ for Polyline-Join command.")