(vl-load-com)

(setq *ysrw-geom-tol* 1.0
      *ysrw-side-dim-offset* 900.0
      *ysrw-total-dim-gap* 700.0)

(defun ysrw:3dpt (pt)
  (list
    (float (car pt))
    (float (cadr pt))
    (float (if (caddr pt) (caddr pt) 0.0))))

(defun ysrw:array->list (val)
  (if (= 'VARIANT (type val))
    (setq val (vlax-variant-value val)))
  (vlax-safearray->list val))

(defun ysrw:ss-curves-or-implied (/ filter ss)
  (setq filter
        '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC,CIRCLE,ELLIPSE,SPLINE")))
  (cond
    ((setq ss (ssget "_I" filter)) ss)
    ((setq ss (ssget filter)) ss)))

(defun ysrw:ss->enames (ss / idx result)
  (setq idx 0)
  (while (< idx (sslength ss))
    (setq result (cons (ssname ss idx) result)
          idx (1+ idx)))
  (reverse result))

(defun ysrw:entity-bbox (ename / obj pmin pmax)
  (if (and ename (setq obj (vlax-ename->vla-object ename)))
    (progn
      (vla-GetBoundingBox obj 'pmin 'pmax)
      (list
        (ysrw:array->list pmin)
        (ysrw:array->list pmax)))))

(defun ysrw:bbox-union (bbox1 bbox2)
  (list
    (list
      (min (car (car bbox1)) (car (car bbox2)))
      (min (cadr (car bbox1)) (cadr (car bbox2)))
      (min (caddr (car bbox1)) (caddr (car bbox2))))
    (list
      (max (car (cadr bbox1)) (car (cadr bbox2)))
      (max (cadr (cadr bbox1)) (cadr (cadr bbox2)))
      (max (caddr (cadr bbox1)) (caddr (cadr bbox2))))))

(defun ysrw:selection-bbox (ss / enames ename bbox result)
  (setq enames (ysrw:ss->enames ss))
  (foreach ename enames
    (if (setq bbox (ysrw:entity-bbox ename))
      (setq result
            (if result
              (ysrw:bbox-union result bbox)
              bbox))))
  result)

(defun ysrw:current-space (doc)
  (if (= 1 (getvar "CVPORT"))
    (vla-get-PaperSpace doc)
    (vla-get-ModelSpace doc)))

(defun ysrw:add-rotated-dim (space p1 p2 dimpt angle)
  (vla-AddDimRotated
    space
    (vlax-3d-point (ysrw:3dpt p1))
    (vlax-3d-point (ysrw:3dpt p2))
    (vlax-3d-point (ysrw:3dpt dimpt))
    angle))

(defun ysrw:sort-unique-values (vals / sorted result)
  (setq sorted (vl-sort vals '<))
  (foreach val sorted
    (if (or (not result)
            (not (equal val (car result) *ysrw-geom-tol*)))
      (setq result (cons val result))))
  (reverse result))

(defun ysrw:last-item (vals)
  (car (reverse vals)))

(defun ysrw:collect-side-points (/ idx pt pts)
  (setq idx 1)
  (while
    (setq pt
          (getpoint
            (strcat
              "\nPick side point "
              (itoa idx)
              " <Enter to finish this side>: ")))
    (setq pts (cons (ysrw:3dpt pt) pts)
          idx (1+ idx)))
  (reverse pts))

(defun ysrw:point-ranges (pts / minx maxx miny maxy pt)
  (foreach pt pts
    (setq minx (if minx (min minx (car pt)) (car pt))
          maxx (if maxx (max maxx (car pt)) (car pt))
          miny (if miny (min miny (cadr pt)) (cadr pt))
          maxy (if maxy (max maxy (cadr pt)) (cadr pt))))
  (list minx maxx miny maxy))

(defun ysrw:points-side (pts bbox / ranges xspan yspan bbox-min bbox-max left-gap right-gap bottom-gap top-gap)
  (setq ranges (ysrw:point-ranges pts)
        xspan (- (cadr ranges) (car ranges))
        yspan (- (cadddr ranges) (caddr ranges))
        bbox-min (car bbox)
        bbox-max (cadr bbox)
        left-gap (abs (- (car ranges) (car bbox-min)))
        right-gap (abs (- (car bbox-max) (cadr ranges)))
        bottom-gap (abs (- (caddr ranges) (cadr bbox-min)))
        top-gap (abs (- (cadr bbox-max) (cadddr ranges))))
  (if (>= xspan yspan)
    (if (<= bottom-gap top-gap) 'bottom 'top)
    (if (<= left-gap right-gap) 'left 'right)))

(defun ysrw:side-name (side)
  (cond
    ((eq side 'top) "top")
    ((eq side 'bottom) "bottom")
    ((eq side 'left) "left")
    ((eq side 'right) "right")
    (T "unknown")))

(defun ysrw:point-axis-values (pts side / pt vals)
  (foreach pt pts
    (setq vals
          (cons
            (if (member side '(top bottom))
              (car pt)
              (cadr pt))
            vals)))
  (ysrw:sort-unique-values vals))

(defun ysrw:outer-base-from-bbox (bbox side)
  (cond
    ((eq side 'top) (cadr (cadr bbox)))
    ((eq side 'bottom) (cadr (car bbox)))
    ((eq side 'left) (car (car bbox)))
    (T (car (cadr bbox)))))

(defun ysrw:add-horizontal-chain (space xvals basey dimy / idx x1 x2)
  (setq idx 0)
  (while (< idx (1- (length xvals)))
    (setq x1 (nth idx xvals)
          x2 (nth (1+ idx) xvals))
    (if (> (abs (- x2 x1)) *ysrw-geom-tol*)
      (ysrw:add-rotated-dim
        space
        (list x1 basey 0.0)
        (list x2 basey 0.0)
        (list (/ (+ x1 x2) 2.0) dimy 0.0)
        0.0))
    (setq idx (1+ idx))))

(defun ysrw:add-vertical-chain (space yvals basex dimx / idx y1 y2)
  (setq idx 0)
  (while (< idx (1- (length yvals)))
    (setq y1 (nth idx yvals)
          y2 (nth (1+ idx) yvals))
    (if (> (abs (- y2 y1)) *ysrw-geom-tol*)
      (ysrw:add-rotated-dim
        space
        (list basex y1 0.0)
        (list basex y2 0.0)
        (list dimx (/ (+ y1 y2) 2.0) 0.0)
        (/ pi 2.0)))
    (setq idx (1+ idx))))

(defun ysrw:add-overall-horizontal (space minx maxx basey dimy)
  (ysrw:add-rotated-dim
    space
    (list minx basey 0.0)
    (list maxx basey 0.0)
    (list (/ (+ minx maxx) 2.0) dimy 0.0)
    0.0))

(defun ysrw:add-overall-vertical (space miny maxy basex dimx)
  (ysrw:add-rotated-dim
    space
    (list basex miny 0.0)
    (list basex maxy 0.0)
    (list dimx (/ (+ miny maxy) 2.0) 0.0)
    (/ pi 2.0)))

(defun ysrw:add-side-dims (space pts side bbox / coords base row total first lastv created)
  (setq coords (ysrw:point-axis-values pts side))
  (if (>= (length coords) 2)
    (progn
      (setq base (ysrw:outer-base-from-bbox bbox side)
            first (car coords)
            lastv (ysrw:last-item coords)
            row (cond
                  ((eq side 'top) (+ base *ysrw-side-dim-offset*))
                  ((eq side 'bottom) (- base *ysrw-side-dim-offset*))
                  ((eq side 'left) (- base *ysrw-side-dim-offset*))
                  (T (+ base *ysrw-side-dim-offset*)))
            total (cond
                    ((eq side 'top) (+ row *ysrw-total-dim-gap*))
                    ((eq side 'bottom) (- row *ysrw-total-dim-gap*))
                    ((eq side 'left) (- row *ysrw-total-dim-gap*))
                    (T (+ row *ysrw-total-dim-gap*))))
      (cond
        ((member side '(top bottom))
         (ysrw:add-horizontal-chain space coords base row)
         (ysrw:add-overall-horizontal space first lastv base total))
        (T
         (ysrw:add-vertical-chain space coords base row)
         (ysrw:add-overall-vertical space first lastv base total)))
      (setq created (length coords))
      created)))

(defun ysrw:end-undo (doc)
  (if doc
    (vl-catch-all-apply 'vla-EndUndoMark (list doc))))

(defun ysrw:run-manual (/ *error* olderr doc space ss bbox pts side created total-created side-count)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
        space (ysrw:current-space doc)
        olderr *error*)
  (defun *error* (msg)
    (ysrw:end-undo doc)
    (setq *error* olderr)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg)))
    (princ))
  (setq ss (ysrw:ss-curves-or-implied))
  (cond
    ((not ss)
     (princ "\nSelect the current suite geometry first."))
    ((not (setq bbox (ysrw:selection-bbox ss)))
     (princ "\nUnable to build a virtual frame from the selected geometry."))
    (T
     (vla-StartUndoMark doc)
     (while
       (progn
         (princ "\nPick one side's chain points. Press Enter immediately to finish the command.")
         (setq pts (ysrw:collect-side-points))
         pts)
       (cond
         ((< (length pts) 2)
          (princ "\nNeed at least two points for this side."))
         (T
          (setq side (ysrw:points-side pts bbox)
                created (ysrw:add-side-dims space pts side bbox))
          (if created
            (progn
              (setq total-created (+ (if total-created total-created 0) created)
                    side-count (1+ (if side-count side-count 0)))
              (princ
                (strcat
                  "\nCreated side dims. Side: "
                  (ysrw:side-name side)
                  ", chain points: "
                  (itoa (length (ysrw:point-axis-values pts side)))
                  ", dimensions created: "
                  (itoa created)
                  ".")))
            (princ "\nNeed at least two distinct chain positions.")))))
     (if total-created
       (princ
         (strcat
           "\nManual wall dimensions created. Sides: "
           (itoa side-count)
           ", dimensions created: "
           (itoa total-created)
           ", first offset: "
           (rtos *ysrw-side-dim-offset* 2 0)
           ", total gap: "
           (rtos *ysrw-total-dim-gap* 2 0)
           "."))
       (princ "\nNo side dimensions were created."))))
  (ysrw:end-undo doc)
  (setq *error* olderr)
  (princ))

(defun c:YJK ()
  (ysrw:run-manual))

(defun c:YKJ ()
  (ysrw:run-manual))

(defun c:YROOMW ()
  (ysrw:run-manual))

(princ "\nYS wall dimension loaded. Commands: YJK, YKJ, YROOMW.")
(princ)
