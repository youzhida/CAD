(vl-load-com)

(setq *ysd-geom-tol* 1.0
      *ysd-helper-layer* "YDOOR_HELP"
      *ysd-helper-color* 8)

(defun ysd:3dpt (pt)
  (list
    (float (car pt))
    (float (cadr pt))
    (float (if (caddr pt) (caddr pt) 0.0))))

(defun ysd:ss-or-prompt (msg / ss)
  (cond
    ((setq ss (ssget "_I")) ss)
    ((setq ss (ssget msg)) ss)))

(defun ysd:ss->enames (ss / idx result)
  (setq idx 0)
  (while (< idx (sslength ss))
    (setq result (cons (ssname ss idx) result)
          idx (1+ idx)))
  (reverse result))

(defun ysd:ss->object-variant (ss / idx arr)
  (setq arr (vlax-make-safearray vlax-vbObject (cons 0 (1- (sslength ss))))
        idx 0)
  (while (< idx (sslength ss))
    (vlax-safearray-put-element
      arr
      idx
      (vlax-ename->vla-object (ssname ss idx)))
    (setq idx (1+ idx)))
  (vlax-make-variant arr))

(defun ysd:delete-ss (ss / idx)
  (setq idx 0)
  (while (< idx (sslength ss))
    (entdel (ssname ss idx))
    (setq idx (1+ idx))))

(defun ysd:current-space (doc)
  (if (= 1 (getvar "CVPORT"))
    (vla-get-PaperSpace doc)
    (vla-get-ModelSpace doc)))

(defun ysd:timestamp-string (/ raw out ch)
  (setq raw (rtos (getvar "CDATE") 2 6)
        out "")
  (foreach ch (vl-string->list raw)
    (if (/= ch 46)
      (setq out (strcat out (chr ch)))))
  out)

(defun ysd:unique-block-name (/ name idx)
  (setq name (strcat "YDOOR_" (ysd:timestamp-string))
        idx 1)
  (while (tblsearch "BLOCK" name)
    (setq name (strcat "YDOOR_" (ysd:timestamp-string) "_" (itoa idx))
          idx (1+ idx)))
  name)

(defun ysd:entity-type (ename / data)
  (setq data (entget ename))
  (if data
    (cdr (assoc 0 data))))

(defun ysd:entity-bbox (ename / obj pmin pmax)
  (if (and ename (setq obj (vlax-ename->vla-object ename)))
    (progn
      (vla-GetBoundingBox obj 'pmin 'pmax)
      (list
        (vlax-safearray->list pmin)
        (vlax-safearray->list pmax)))))

(defun ysd:ss-bbox (ss / idx ename bbox minpt maxpt)
  (setq idx 0)
  (while (< idx (sslength ss))
    (setq ename (ssname ss idx)
          bbox (ysd:entity-bbox ename))
    (if bbox
      (if minpt
        (progn
          (setq minpt
                 (list
                   (min (car minpt) (car (car bbox)))
                   (min (cadr minpt) (cadr (car bbox)))
                   (min (caddr minpt) (caddr (car bbox)))))
          (setq maxpt
                 (list
                   (max (car maxpt) (car (cadr bbox)))
                   (max (cadr maxpt) (cadr (cadr bbox)))
                   (max (caddr maxpt) (caddr (cadr bbox))))))
        (setq minpt (car bbox)
              maxpt (cadr bbox))))
    (setq idx (1+ idx)))
  (if minpt
    (list minpt maxpt)))

(defun ysd:bbox-width (bbox)
  (- (car (cadr bbox)) (car (car bbox))))

(defun ysd:bbox-height (bbox)
  (- (cadr (cadr bbox)) (cadr (car bbox))))

(defun ysd:bbox-union (bbox1 bbox2)
  (list
    (list
      (min (car (car bbox1)) (car (car bbox2)))
      (min (cadr (car bbox1)) (cadr (car bbox2)))
      (min (caddr (car bbox1)) (caddr (car bbox2))))
    (list
      (max (car (cadr bbox1)) (car (cadr bbox2)))
      (max (cadr (cadr bbox1)) (cadr (cadr bbox2)))
      (max (caddr (cadr bbox1)) (caddr (cadr bbox2))))))

(defun ysd:get-line-points (ename / data)
  (setq data (entget ename))
  (if (= "LINE" (cdr (assoc 0 data)))
    (list
      (ysd:3dpt (cdr (assoc 10 data)))
      (ysd:3dpt (cdr (assoc 11 data))))))

(defun ysd:horizontal-info (ename / bbox p1 p2 x1 x2 y1 y2)
  (setq bbox (ysd:entity-bbox ename))
  (if (and bbox
           (> (ysd:bbox-width bbox) (* 2.0 *ysd-geom-tol*))
           (<= (ysd:bbox-height bbox) *ysd-geom-tol*)
           (setq p1 (car (ysd:get-line-points ename)))
           (setq p2 (cadr (ysd:get-line-points ename))))
    (progn
      (setq x1 (car p1)
            x2 (car p2)
            y1 (cadr p1)
            y2 (cadr p2))
      (if (<= (abs (- y1 y2)) *ysd-geom-tol*)
        (list
          (min x1 x2)
          (max x1 x2)
          (/ (+ y1 y2) 2.0))))))

(defun ysd:door-helper-data (enames / bbox ebbox minpt maxpt width height inner-left inner-right line-info result)
  (foreach ename enames
    (setq ebbox (ysd:entity-bbox ename))
    (if ebbox
      (if bbox
        (setq bbox (ysd:bbox-union bbox ebbox))
        (setq bbox ebbox)))
    (setq line-info (ysd:horizontal-info ename))
    (if line-info
      (progn
        (setq inner-left
               (if inner-left
                 (min inner-left (car line-info))
                 (car line-info)))
        (setq inner-right
               (if inner-right
                 (max inner-right (cadr line-info))
                 (cadr line-info))))))
  (if bbox
    (progn
      (setq minpt (car bbox)
            maxpt (cadr bbox)
            width (ysd:bbox-width bbox)
            height (ysd:bbox-height bbox))
      (if (or (not inner-left)
              (not inner-right)
              (<= inner-left (+ (car minpt) *ysd-geom-tol*))
              (>= inner-right (- (car maxpt) *ysd-geom-tol*))
              (>= inner-left inner-right))
        (setq inner-left (+ (car minpt) (* width 0.08))
              inner-right (- (car maxpt) (* width 0.08))))
      (setq result
             (list
               (cons 'bbox bbox)
               (cons 'minx (car minpt))
               (cons 'maxx (car maxpt))
               (cons 'miny (cadr minpt))
               (cons 'maxy (cadr maxpt))
               (cons 'midy (/ (+ (cadr minpt) (cadr maxpt)) 2.0))
               (cons 'base
                     (list
                       (/ (+ (car minpt) (car maxpt)) 2.0)
                       (/ (+ (cadr minpt) (cadr maxpt)) 2.0)
                       0.0))
               (cons 'inner-left inner-left)
               (cons 'inner-right inner-right)
               (cons 'margin (max 2.0 (min 20.0 (* width 0.02))))))
      result)))

(defun ysd:ensure-layer (doc name color / layers result layer)
  (setq layers (vla-get-Layers doc)
        result (vl-catch-all-apply 'vla-Item (list layers name)))
  (setq layer
         (if (vl-catch-all-error-p result)
           (vla-Add layers name)
           result))
  (if color
    (vla-put-Color layer color))
  (if (vlax-property-available-p layer 'Plottable)
    (vla-put-Plottable layer :vlax-false))
  layer)

(defun ysd:add-line (space layer p1 p2 / obj)
  (setq obj (vla-AddLine space (vlax-3d-point (ysd:3dpt p1)) (vlax-3d-point (ysd:3dpt p2))))
  (vla-put-Layer obj layer)
  obj)

(defun ysd:add-rect (space layer x1 y1 x2 y2 / arr obj)
  (setq arr (vlax-make-safearray vlax-vbDouble '(0 . 9)))
  (vlax-safearray-fill
    arr
    (list
      x1 y1
      x2 y1
      x2 y2
      x1 y2
      x1 y1))
  (setq obj (vla-AddLightWeightPolyline space arr))
  (vla-put-Layer obj layer)
  obj)

(defun ysd:add-helper-geometry (doc block data / layer minx maxx miny maxy midy inner-left inner-right margin)
  (ysd:ensure-layer doc *ysd-helper-layer* *ysd-helper-color*)
  (setq minx (cdr (assoc 'minx data))
        maxx (cdr (assoc 'maxx data))
        miny (cdr (assoc 'miny data))
        maxy (cdr (assoc 'maxy data))
        midy (cdr (assoc 'midy data))
        inner-left (cdr (assoc 'inner-left data))
        inner-right (cdr (assoc 'inner-right data))
        margin (cdr (assoc 'margin data)))
  (ysd:add-line block *ysd-helper-layer* (list minx midy 0.0) (list maxx midy 0.0))
  (ysd:add-line block *ysd-helper-layer* (list inner-left miny 0.0) (list inner-left maxy 0.0))
  (ysd:add-line block *ysd-helper-layer* (list inner-right miny 0.0) (list inner-right maxy 0.0))
  (ysd:add-rect block *ysd-helper-layer* (- minx margin) (- miny margin) (+ inner-left margin) (+ maxy margin))
  (ysd:add-rect block *ysd-helper-layer* (- inner-right margin) (- miny margin) (+ maxx margin) (+ maxy margin)))

(defun ysd:end-undo (doc)
  (if doc
    (vl-catch-all-apply 'vla-EndUndoMark (list doc))))

(defun ysd:block-name-prompt (default / value)
  (setq value (getstring T (strcat "\nDoor block name <" default ">: ")))
  (if (= "" value)
    default
    value))

(defun ysd:open-bedit (blkname)
  (vl-catch-all-apply 'vl-cmdf (list "_.BEDIT" blkname)))

(defun ysd:prep-door-block (/ *error* olderr doc ss enames data basept blkname blkdef block-ref first-obj obj-var open-bedit-name)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
        olderr *error*)
  (defun *error* (msg)
    (ysd:end-undo doc)
    (setq *error* olderr)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg)))
    (princ))
  (vla-StartUndoMark doc)
  (setq ss (ysd:ss-or-prompt "\nSelect sliding door geometry: "))
  (cond
    ((not ss)
     (princ "\nNothing selected."))
    (T
     (setq enames (ysd:ss->enames ss)
           data (ysd:door-helper-data enames))
     (cond
       ((not data)
        (princ "\nUnable to analyze selected geometry."))
       (T
        (setq blkname (ysd:block-name-prompt (ysd:unique-block-name))
              basept (cdr (assoc 'base data))
              blkdef (vla-Add (vla-get-Blocks doc) (vlax-3d-point basept) blkname)
              first-obj (vlax-ename->vla-object (car enames))
              obj-var (ysd:ss->object-variant ss))
        (vla-CopyObjects doc obj-var blkdef)
        (ysd:add-helper-geometry doc blkdef data)
        (setq block-ref
               (vla-InsertBlock
                 (ysd:current-space doc)
                 (vlax-3d-point basept)
                 blkname
                 1.0
                 1.0
                 1.0
                 0.0))
        (if first-obj
          (vla-put-Layer block-ref (vla-get-Layer first-obj)))
        (ysd:delete-ss ss)
        (princ
          (strcat
            "\nDoor block created: "
            blkname
            ". Opening BEDIT. Use the center guide line for Linear Parameter, then use the two helper rectangles for left/right Stretch actions."))
        (setq open-bedit-name blkname)))))
  (ysd:end-undo doc)
  (setq *error* olderr)
  (if open-bedit-name
    (ysd:open-bedit open-bedit-name))
  (princ))

(defun c:YDOORPREP ()
  (ysd:prep-door-block))

(defun c:YDP ()
  (ysd:prep-door-block))

(princ "\nYS door prep loaded. Commands: YDOORPREP, YDP.")
(princ)
