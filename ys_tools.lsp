(vl-load-com)

(setq *ys-cabinet-target-width* 600.0
      *ys-cabinet-max-panels* 8
      *ys-cabinet-inset* 30.0
      *ys-geom-tol* 1.0
      *ys-furniture-outer-color* 2
      *ys-furniture-inner-color* 1
      *ys-furniture-inner-linetype* "__DASH"
      *ys-furniture-inner-ltscale* 1.0
      *ys-global-ltscale* 400.0
      *ys-custom-linetype-file* "F:/ys/cad_plugin/ys_tools.lin")

(defun ys:3dpt (pt)
  (list
    (float (car pt))
    (float (cadr pt))
    (float (if (caddr pt) (caddr pt) 0.0))))

(defun ys:vector (p1 p2)
  (mapcar '- (ys:3dpt p2) (ys:3dpt p1)))

(defun ys:length (v)
  (distance '(0.0 0.0 0.0) v))

(defun ys:dot (a b)
  (+ (* (car a) (car b))
     (* (cadr a) (cadr b))
     (* (caddr a) (caddr b))))

(defun ys:cross2d (a b)
  (- (* (car a) (cadr b))
     (* (cadr a) (car b))))

(defun ys:lerp (p1 p2 ratio)
  (mapcar
    '(lambda (a b) (+ a (* (- b a) ratio)))
    (ys:3dpt p1)
    (ys:3dpt p2)))

(defun ys:unit2d (v / len)
  (setq len (distance '(0.0 0.0 0.0) (list (car v) (cadr v) 0.0)))
  (if (> len 1e-9)
    (list (/ (car v) len) (/ (cadr v) len) 0.0)
    '(0.0 0.0 0.0)))

(defun ys:left-normal (v)
  (list (- (cadr v)) (car v) 0.0))

(defun ys:right-normal (v)
  (list (cadr v) (- (car v)) 0.0))

(defun ys:offset-point (pt normal dist)
  (list
    (+ (car pt) (* (car normal) dist))
    (+ (cadr pt) (* (cadr normal) dist))
    (float (if (caddr pt) (caddr pt) 0.0))))

(defun ys:polygon-area2 (pts / total idx p1 p2)
  (setq total 0.0
        idx 0)
  (while (< idx (length pts))
    (setq p1 (nth idx pts)
          p2 (nth (rem (1+ idx) (length pts)) pts)
          total (+ total
                   (- (* (car p1) (cadr p2))
                      (* (cadr p1) (car p2)))))
    (setq idx (1+ idx)))
  total)

(defun ys:line-intersection-2d (p1 p2 p3 p4 / r s denom tval)
  (setq r (ys:vector p1 p2)
        s (ys:vector p3 p4)
        denom (ys:cross2d r s))
  (if (> (abs denom) 1e-9)
    (progn
      (setq tval (/ (ys:cross2d (ys:vector p1 p3) s) denom))
      (ys:lerp p1 p2 tval))))

(defun ys:axis-aligned-segment-p (p1 p2)
  (or (<= (abs (- (car p1) (car p2))) *ys-geom-tol*)
      (<= (abs (- (cadr p1) (cadr p2))) *ys-geom-tol*)))

(defun ys:collinear-3pt-p (p1 p2 p3 / v1 v2)
  (setq v1 (ys:vector p1 p2)
        v2 (ys:vector p2 p3))
  (and (<= (abs (ys:cross2d v1 v2))
           (max 1e-6 (* *ys-geom-tol* (max (ys:length v1) (ys:length v2) 1.0))))
       (ys:axis-aligned-segment-p p1 p2)
       (ys:axis-aligned-segment-p p2 p3)))

(defun ys:simplify-closed-pts-once (pts / len idx prev curr next result)
  (setq len (length pts)
        idx 0)
  (while (< idx len)
    (setq prev (nth (if (= idx 0) (1- len) (1- idx)) pts)
          curr (nth idx pts)
          next (nth (rem (1+ idx) len) pts))
    (if (not (ys:collinear-3pt-p prev curr next))
      (setq result (cons curr result)))
    (setq idx (1+ idx)))
  (reverse result))

(defun ys:simplify-closed-pts (pts / prev cur)
  (setq prev nil
        cur pts)
  (while (not (equal prev cur 1e-6))
    (setq prev cur
          cur (ys:simplify-closed-pts-once cur)))
  cur)

(defun ys:ss-or-prompt (msg / ss)
  (cond
    ((setq ss (ssget "_I")) ss)
    ((setq ss (ssget msg)) ss)))

(defun ys:entity-or-prompt (msg / ss)
  (cond
    ((and (setq ss (ssget "_I"))
          (= 1 (sslength ss)))
     (ssname ss 0))
    ((car (entsel msg)))))

(defun ys:entity-type (ename / data)
  (if (and ename (setq data (entget ename)))
    (cdr (assoc 0 data))))

(defun ys:polyline-entity-p (ename / kind)
  (setq kind (ys:entity-type ename))
  (member kind '("LWPOLYLINE" "POLYLINE")))

(defun ys:lwpoly-vertices (ename / data pts)
  (setq data (entget ename))
  (foreach item data
    (if (= 10 (car item))
      (setq pts (cons (ys:3dpt (cdr item)) pts))))
  (reverse pts))

(defun ys:oldpoly-vertices (ename / next data kind pts done)
  (setq next (entnext ename)
        done nil)
  (while (and next (not done))
    (setq data (entget next)
          kind (cdr (assoc 0 data)))
    (cond
      ((= "VERTEX" kind)
       (setq pts (cons (ys:3dpt (cdr (assoc 10 data))) pts)))
      ((= "SEQEND" kind)
       (setq done T)))
    (if (not done)
      (setq next (entnext next))))
  (reverse pts))

(defun ys:polyline-vertices (ename / kind)
  (setq kind (ys:entity-type ename))
  (cond
    ((= "LWPOLYLINE" kind) (ys:lwpoly-vertices ename))
    ((= "POLYLINE" kind) (ys:oldpoly-vertices ename))))

(defun ys:polyline-closed-p (ename / data)
  (setq data (entget ename))
  (= 1 (logand 1 (if (assoc 70 data) (cdr (assoc 70 data)) 0))))

(defun ys:polyline-layer (ename / data)
  (setq data (entget ename))
  (cdr (assoc 8 data)))

(defun ys:polyline-has-bulge-p (ename / kind data next found)
  (setq kind (ys:entity-type ename))
  (cond
    ((= "LWPOLYLINE" kind)
     (setq data (entget ename))
     (foreach item data
       (if (and (= 42 (car item))
                (> (abs (float (cdr item))) 1e-9))
         (setq found T))))
    ((= "POLYLINE" kind)
     (setq next (entnext ename))
     (while (and next (not found))
       (setq data (entget next))
       (cond
         ((= "VERTEX" (cdr (assoc 0 data)))
          (if (and (assoc 42 data)
                   (> (abs (float (cdr (assoc 42 data)))) 1e-9))
            (setq found T)))
         ((= "SEQEND" (cdr (assoc 0 data)))
          (setq next nil)))
       (if next
         (setq next (entnext next))))))
  found)

(defun ys:entity-bbox (ename / obj pmin pmax)
  (if (and ename (setq obj (vlax-ename->vla-object ename)))
    (progn
      (vla-GetBoundingBox obj 'pmin 'pmax)
      (list
        (vlax-safearray->list pmin)
        (vlax-safearray->list pmax)))))
(defun ys:bbox-width (bbox)
  (- (car (cadr bbox)) (car (car bbox))))

(defun ys:bbox-height (bbox)
  (- (cadr (cadr bbox)) (cadr (car bbox))))

(defun ys:bbox-maxdim (bbox)
  (max (ys:bbox-width bbox) (ys:bbox-height bbox)))

(defun ys:bbox-from-points (pts / minpt maxpt)
  (foreach pt pts
    (if minpt
      (progn
        (setq minpt
               (list
                 (min (car minpt) (car pt))
                 (min (cadr minpt) (cadr pt))
                 (min (caddr minpt) (if (caddr pt) (caddr pt) 0.0))))
        (setq maxpt
               (list
                 (max (car maxpt) (car pt))
                 (max (cadr maxpt) (cadr pt))
                 (max (caddr maxpt) (if (caddr pt) (caddr pt) 0.0)))))
      (setq minpt (ys:3dpt pt)
            maxpt (ys:3dpt pt))))
  (if minpt
    (list minpt maxpt)))

(defun ys:range-overlap-size (a1 a2 b1 b2)
  (max 0.0
       (- (min a2 b2)
          (max a1 b1))))

(defun ys:bbox-rect-points (bbox / pmin pmax)
  (setq pmin (car bbox)
        pmax (cadr bbox))
  (list
    (list (car pmin) (cadr pmin) 0.0)
    (list (car pmax) (cadr pmin) 0.0)
    (list (car pmax) (cadr pmax) 0.0)
    (list (car pmin) (cadr pmax) 0.0)))

(defun ys:vla-object-name (obj)
  (if obj
    (vla-get-ObjectName obj)))

(defun ys:vla-curve-object-p (obj / kind)
  (setq kind (ys:vla-object-name obj))
  (member kind
          '("AcDbLine"
            "AcDbPolyline"
            "AcDb2dPolyline"
            "AcDb3dPolyline"
            "AcDbArc"
            "AcDbCircle"
            "AcDbEllipse"
            "AcDbSpline")))

(defun ys:vla-blockref-p (obj)
  (= "AcDbBlockReference" (ys:vla-object-name obj)))

(defun ys:vla-bbox (obj / pmin pmax)
  (if obj
    (progn
      (vla-GetBoundingBox obj 'pmin 'pmax)
      (list
        (vlax-safearray->list pmin)
        (vlax-safearray->list pmax)))))

(defun ys:set-vla-style (obj color linetype)
  (if obj
    (progn
      (if color (vla-put-Color obj color))
      (if linetype (vla-put-Linetype obj linetype)))))

(defun ys:inner-linetype-scale (bbox)
  *ys-furniture-inner-ltscale*)

(defun ys:style-inner-vla-object (obj bbox / inner-ltype)
  (setq inner-ltype (ys:resolve-inner-linetype))
  (ys:set-vla-style obj *ys-furniture-inner-color* inner-ltype)
  (if (and obj (vlax-property-available-p obj 'LinetypeScale))
    (vla-put-LinetypeScale obj (ys:inner-linetype-scale bbox)))
  (if (and obj (vlax-property-available-p obj 'LinetypeGeneration))
    (vla-put-LinetypeGeneration obj :vlax-true)))

(defun ys:style-outer-vla-object (obj)
  (ys:set-vla-style obj *ys-furniture-outer-color* "Continuous")
  (if (and obj (vlax-property-available-p obj 'LinetypeScale))
    (vla-put-LinetypeScale obj 1.0)))

(defun ys:style-inner-entity (ename bbox / obj)
  (if (and ename (setq obj (vlax-ename->vla-object ename)))
    (ys:style-inner-vla-object obj bbox)))

(defun ys:style-outer-entity (ename / obj)
  (if (and ename (setq obj (vlax-ename->vla-object ename)))
    (ys:style-outer-vla-object obj)))

(defun ys:set-entity-style (ename color linetype / obj)
  (if (and ename (setq obj (vlax-ename->vla-object ename)))
    (ys:set-vla-style obj color linetype)))

(defun ys:ensure-linetype (name / file)
  (if (tblsearch "LTYPE" name)
    name
    (progn
      (foreach file
               (list
                 (findfile "ys_tools.lin")
                 *ys-custom-linetype-file*
                 "acad.lin"
                 "acadiso.lin")
        (if (not (tblsearch "LTYPE" name))
          (vl-catch-all-apply 'vl-cmdf (list "._-LINETYPE" "_Load" name file))))
      (if (tblsearch "LTYPE" name)
        name))))

(defun ys:resolve-inner-linetype (/ name)
  (cond
    ((tblsearch "LTYPE" *ys-furniture-inner-linetype*)
     *ys-furniture-inner-linetype*)
    ((setq name (ys:ensure-linetype *ys-furniture-inner-linetype*))
     name)
    ((setq name (ys:ensure-linetype "__DASH"))
     name)
    ((setq name (ys:ensure-linetype "DASHED"))
     name)
    ((setq name (ys:ensure-linetype "HIDDEN"))
     name)
    (T "Continuous")))

(defun ys:apply-linetype-standards ()
  (setvar "LTSCALE" *ys-global-ltscale*)
  (ys:resolve-inner-linetype))

(defun ys:make-line (layer p1 p2 color linetype / ename)
  (setq ename
         (entmakex
           (list
             '(0 . "LINE")
             '(100 . "AcDbEntity")
             (cons 8 layer)
             '(100 . "AcDbLine")
             (cons 10 (ys:3dpt p1))
             (cons 11 (ys:3dpt p2)))))
  (ys:set-entity-style ename color linetype)
  ename)

(defun ys:make-lwpolyline (layer pts color linetype / data pt ename)
  (setq data
         (list
           '(0 . "LWPOLYLINE")
           '(100 . "AcDbEntity")
           (cons 8 layer)
           '(100 . "AcDbPolyline")
           (cons 90 (length pts))
           (cons 70 1)))
  (foreach pt pts
    (setq data
           (append data
                   (list (cons 10 (list (car pt) (cadr pt)))))))
  (setq ename (entmakex data))
  (ys:set-entity-style ename color linetype)
  ename)

(defun ys:ss-bbox (ss / idx obj pmin pmax minpt maxpt)
  (setq idx 0)
  (while (< idx (sslength ss))
    (setq obj (vlax-ename->vla-object (ssname ss idx)))
    (vla-GetBoundingBox obj 'pmin 'pmax)
    (setq pmin (vlax-safearray->list pmin)
          pmax (vlax-safearray->list pmax))
    (if minpt
      (progn
        (setq minpt
               (list
                 (min (car minpt) (car pmin))
                 (min (cadr minpt) (cadr pmin))
                 (min (caddr minpt) (caddr pmin))))
        (setq maxpt
               (list
                 (max (car maxpt) (car pmax))
                 (max (cadr maxpt) (cadr pmax))
                 (max (caddr maxpt) (caddr pmax)))))
      (setq minpt pmin
            maxpt pmax))
    (setq idx (1+ idx)))
  (if minpt
    (list minpt maxpt)))

(defun ys:ss->object-variant (ss / idx arr)
  (setq arr (vlax-make-safearray vlax-vbObject (cons 0 (1- (sslength ss))))
        idx 0)
  (while (< idx (sslength ss))
    (vlax-safearray-put-element
      arr
      idx
      (vlax-ename->vla-object (ssname ss idx)))
    (setq idx (1+ idx)))
  (vlax-make-variant arr))

(defun ys:ss->enames (ss / idx result)
  (setq idx 0)
  (while (< idx (sslength ss))
    (setq result (cons (ssname ss idx) result)
          idx (1+ idx)))
  (reverse result))

(defun ys:delete-ss (ss / idx)
  (setq idx 0)
  (while (< idx (sslength ss))
    (entdel (ssname ss idx))
    (setq idx (1+ idx))))

(defun ys:variant-object-list (value / arr lb ub idx result)
  (setq arr (vlax-variant-value value)
        lb (vlax-safearray-get-l-bound arr 1)
        ub (vlax-safearray-get-u-bound arr 1)
        idx lb)
  (while (<= idx ub)
    (setq result (cons (vlax-safearray-get-element arr idx) result)
          idx (1+ idx)))
  (reverse result))

(defun ys:current-space (doc)
  (if (= 1 (getvar "CVPORT"))
    (vla-get-PaperSpace doc)
    (vla-get-ModelSpace doc)))

(defun ys:timestamp-string (/ raw out ch)
  (setq raw (rtos (getvar "CDATE") 2 6)
        out "")
  (foreach ch (vl-string->list raw)
    (if (/= ch 46)
      (setq out (strcat out (chr ch)))))
  out)

(defun ys:unique-block-name (/ base name idx)
  (setq base (strcat "YS_BLK_" (ys:timestamp-string))
        name base
        idx 1)
  (while (tblsearch "BLOCK" name)
    (setq name (strcat base "_" (itoa idx))
          idx (1+ idx)))
  name)

(defun ys:end-undo (doc)
  (if doc
    (vl-catch-all-apply 'vla-EndUndoMark (list doc))))

(defun ys:regen (doc)
  (if doc
    (vl-catch-all-apply 'vla-Regen (list doc 1))))

(defun ys:rectangle-data-from-points (pts layer / v1 v2 v3 v4 l1 l2 l3 l4 tol edge-a0 edge-a1 edge-b0 edge-b1)
  (if (/= 4 (length pts))
    nil
    (progn
      (setq v1 (ys:vector (nth 0 pts) (nth 1 pts))
            v2 (ys:vector (nth 1 pts) (nth 2 pts))
            v3 (ys:vector (nth 2 pts) (nth 3 pts))
            v4 (ys:vector (nth 3 pts) (nth 0 pts))
            l1 (ys:length v1)
            l2 (ys:length v2)
            l3 (ys:length v3)
            l4 (ys:length v4)
            tol 1e-6)
      (if (and (> l1 tol)
               (> l2 tol)
               (> l3 tol)
               (> l4 tol)
               (<= (abs (ys:dot v1 v2)) (* tol l1 l2))
               (<= (abs (ys:dot v2 v3)) (* tol l2 l3))
               (<= (abs (- l1 l3)) (* tol (max l1 l3)))
               (<= (abs (- l2 l4)) (* tol (max l2 l4)))
               (<= (abs (ys:cross2d v1 v3)) (* tol l1 l3))
               (<= (abs (ys:cross2d v2 v4)) (* tol l2 l4)))
        (progn
          (if (>= l1 l2)
            (setq edge-a0 (nth 0 pts)
                  edge-a1 (nth 1 pts)
                  edge-b0 (nth 3 pts)
                  edge-b1 (nth 2 pts))
            (setq edge-a0 (nth 1 pts)
                  edge-a1 (nth 2 pts)
                  edge-b0 (nth 0 pts)
                  edge-b1 (nth 3 pts)))
          (list
            (cons 'layer layer)
            (cons 'pts pts)
            (cons 'span (max l1 l2))
            (cons 'depth (min l1 l2))
            (cons 'edge-a0 edge-a0)
            (cons 'edge-a1 edge-a1)
            (cons 'edge-b0 edge-b0)
            (cons 'edge-b1 edge-b1)))))))

(defun ys:rectangle-data (ename / pts)
  (if (and (ys:polyline-entity-p ename)
           (ys:polyline-closed-p ename)
           (not (ys:polyline-has-bulge-p ename)))
    (progn
      (setq pts (ys:simplify-closed-pts (ys:polyline-vertices ename)))
      (ys:rectangle-data-from-points pts (ys:polyline-layer ename)))))

(defun ys:inset-rectangle-points (pts inset / area normals idx p1 p2 shifted intersections)
  (setq area (ys:polygon-area2 pts))
  (if (and (= 4 (length pts))
           (> inset 0.0)
           (> (abs area) 1e-9))
    (progn
      (setq idx 0)
      (while (< idx 4)
        (setq p1 (nth idx pts)
              p2 (nth (rem (1+ idx) 4) pts)
              normals
                (append
                  normals
                  (list
                    (if (> area 0.0)
                      (ys:left-normal (ys:unit2d (ys:vector p1 p2)))
                      (ys:right-normal (ys:unit2d (ys:vector p1 p2)))))))
        (setq shifted
               (append
                 shifted
                 (list
                   (list
                     (ys:offset-point p1 (nth idx normals) inset)
                     (ys:offset-point p2 (nth idx normals) inset)))))
        (setq idx (1+ idx)))
      (setq idx 0)
      (while (< idx 4)
        (setq intersections
               (append
                 intersections
                 (list
                   (ys:line-intersection-2d
                     (car (nth idx shifted))
                     (cadr (nth idx shifted))
                     (car (nth (rem (1+ idx) 4) shifted))
                     (cadr (nth (rem (1+ idx) 4) shifted))))))
        (setq idx (1+ idx)))
      (if (not (member nil intersections))
        intersections))))

(defun ys:step-boundaries (span step / result panel-count idx actual-step)
  (setq span (abs span)
        step (max 1.0 step))
  (cond
    ((<= span 1e-6)
     (list 0.0))
    (T
     (setq panel-count (max 1 (fix (+ (/ span step) 0.5))))
     (if (> panel-count *ys-cabinet-max-panels*)
       (setq panel-count *ys-cabinet-max-panels*))
     (setq idx 0
           actual-step (/ span panel-count))
     (while (<= idx panel-count)
       (setq result (append result (list (* idx actual-step)))
             idx (1+ idx)))
     result)))

(defun ys:offset-inward-ename (ename inset / obj original-area result objs best best-area item area dist)
  (if (and ename (> inset 0.0))
    (progn
      (setq obj (vlax-ename->vla-object ename)
            original-area (if (vlax-property-available-p obj 'Area)
                            (abs (vla-get-Area obj))))
      (if original-area
        (progn
          (foreach dist (list (- inset) inset)
            (setq result (vl-catch-all-apply 'vla-Offset (list obj dist)))
            (if (not (vl-catch-all-error-p result))
              (progn
                (setq objs (ys:variant-object-list result))
                (foreach item objs
                  (if (vlax-property-available-p item 'Area)
                    (progn
                      (setq area (abs (vla-get-Area item)))
                      (if (and (< area original-area)
                               (or (not best-area) (> area best-area)))
                        (setq best item
                              best-area area)))))
                (foreach item objs
                  (if (or (not best)
                          (/= (vla-get-Handle item) (vla-get-Handle best)))
                    (vla-Delete item))))))
          (if best
            (vlax-vla-object->ename best)))))))

(defun ys:straight-cabinet-data (ename / bbox width height span depth layer rect kind)
  (if (and (ys:polyline-entity-p ename)
           (ys:polyline-closed-p ename)
           (setq bbox (ys:entity-bbox ename)))
    (progn
      (setq width (ys:bbox-width bbox)
            height (ys:bbox-height bbox)
            span (max width height)
            depth (min width height)
            layer (ys:polyline-layer ename)
            rect (ys:rectangle-data ename)
            kind (cond
                   (rect 'rect)
                   ((ys:polyline-has-bulge-p ename) 'rounded)))
      (if (and kind
               (> span 1.0)
               (> depth 1.0)
               (> span (+ depth (max 10.0 (* depth 0.1)))))
        (list
          (cons 'kind kind)
          (cons 'layer layer)
          (cons 'bbox bbox)
          (cons 'rect rect)
          (cons 'orientation (if (>= width height) 'horizontal 'vertical))
          (cons 'span span)
          (cons 'depth depth))))))

(defun ys:draw-straight-cabinet (ename data / kind layer rect orientation inset inner-ltype inner-pts inner-bbox inner-ename inner-outline-ename pmin pmax span boundaries count idx d0 d1 x0 x1 y0 y1 segment-ename)
  (setq kind (cdr (assoc 'kind data))
        layer (cdr (assoc 'layer data))
        rect (cdr (assoc 'rect data))
        orientation (cdr (assoc 'orientation data))
        inner-ltype (ys:resolve-inner-linetype)
        inset (min *ys-cabinet-inset*
                   (max 0.0 (- (/ (cdr (assoc 'depth data)) 2.0) 1.0))))
  (cond
    ((eq kind 'rect)
     (setq inner-pts
            (if (> inset 0.0)
              (ys:inset-rectangle-points (cdr (assoc 'pts rect)) inset)
              (cdr (assoc 'pts rect))))
      (if inner-pts
        (progn
          (setq inner-outline-ename
                 (ys:make-lwpolyline layer inner-pts *ys-furniture-inner-color* inner-ltype)
                inner-bbox (ys:bbox-from-points inner-pts))
          (ys:style-inner-entity inner-outline-ename inner-bbox))))
    ((eq kind 'rounded)
      (if (> inset 0.0)
        (setq inner-ename (ys:offset-inward-ename ename inset))
        (setq inner-bbox (cdr (assoc 'bbox data))))
      (cond
        (inner-ename
         (setq inner-bbox (ys:entity-bbox inner-ename))
         (ys:style-inner-entity inner-ename inner-bbox))
        ((<= inset 0.0)
         inner-bbox))))
  (if inner-bbox
    (progn
      (setq pmin (car inner-bbox)
            pmax (cadr inner-bbox)
            span (if (eq 'horizontal orientation)
                   (- (car pmax) (car pmin))
                   (- (cadr pmax) (cadr pmin)))
            boundaries (ys:step-boundaries span *ys-cabinet-target-width*)
            count (1- (length boundaries))
            x0 (car pmin)
            x1 (car pmax)
            y0 (cadr pmin)
            y1 (cadr pmax))
      (setq idx 1)
      (while (< idx count)
        (if (eq 'horizontal orientation)
          (setq segment-ename
                 (ys:make-line
                   layer
                   (list (+ x0 (nth idx boundaries)) y0 0.0)
                   (list (+ x0 (nth idx boundaries)) y1 0.0)
                   *ys-furniture-inner-color*
                   inner-ltype))
          (setq segment-ename
                 (ys:make-line
                   layer
                   (list x0 (+ y0 (nth idx boundaries)) 0.0)
                   (list x1 (+ y0 (nth idx boundaries)) 0.0)
                   *ys-furniture-inner-color*
                   inner-ltype)))
        (ys:style-inner-entity segment-ename inner-bbox)
        (setq idx (1+ idx)))
      (setq idx 0)
      (while (< idx count)
        (setq d0 (nth idx boundaries)
              d1 (nth (1+ idx) boundaries))
        (if (eq 'horizontal orientation)
          (progn
            (setq segment-ename
                   (ys:make-line
                     layer
                     (list (+ x0 d0) y0 0.0)
                     (list (+ x0 d1) y1 0.0)
                     *ys-furniture-inner-color*
                     inner-ltype))
            (ys:style-inner-entity segment-ename inner-bbox)
            (setq segment-ename
                   (ys:make-line
                     layer
                     (list (+ x0 d0) y1 0.0)
                     (list (+ x0 d1) y0 0.0)
                     *ys-furniture-inner-color*
                     inner-ltype))
            (ys:style-inner-entity segment-ename inner-bbox))
          (progn
            (setq segment-ename
                   (ys:make-line
                     layer
                     (list x0 (+ y0 d0) 0.0)
                     (list x1 (+ y0 d1) 0.0)
                     *ys-furniture-inner-color*
                     inner-ltype))
            (ys:style-inner-entity segment-ename inner-bbox)
            (setq segment-ename
                   (ys:make-line
                     layer
                     (list x1 (+ y0 d0) 0.0)
                     (list x0 (+ y0 d1) 0.0)
                     *ys-furniture-inner-color*
                     inner-ltype))
            (ys:style-inner-entity segment-ename inner-bbox)))
        (setq idx (1+ idx)))
      count)))
(defun ys:append-unique-string (vals item / key exists)
  (if (and item (/= item ""))
    (progn
      (setq key (strcase item)
            exists nil)
      (foreach val vals
        (if (= key (strcase val))
          (setq exists T)))
      (if exists
        vals
        (append vals (list item))))
    vals))

(defun ys:blockref-names (obj / names)
  (if obj
    (progn
      (if (vlax-property-available-p obj 'Name)
        (setq names (ys:append-unique-string names (vla-get-Name obj))))
      (if (vlax-property-available-p obj 'EffectiveName)
        (setq names (ys:append-unique-string names (vla-get-EffectiveName obj))))
      names)))

(defun ys:get-block-definition (doc block-name / blocks result)
  (setq blocks (vla-get-Blocks doc)
        result (vl-catch-all-apply 'vla-Item (list blocks block-name)))
  (if (vl-catch-all-error-p result)
    nil
    result))

(defun ys:block-definition-editable-p (block-obj)
  (and block-obj
       (or (not (vlax-property-available-p block-obj 'IsXRef))
           (= :vlax-false (vla-get-IsXRef block-obj)))
       (or (not (vlax-property-available-p block-obj 'IsLayout))
           (= :vlax-false (vla-get-IsLayout block-obj)))))

(defun ys:bbox-union (bbox1 bbox2)
  (list
    (list
      (min (car (car bbox1)) (car (car bbox2)))
      (min (cadr (car bbox1)) (cadr (car bbox2)))
      (min (caddr (car bbox1)) (caddr (car bbox2))))
    (list
      (max (car (cadr bbox1)) (car (cadr bbox2)))
      (max (cadr (cadr bbox1)) (cadr (cadr bbox2)))
      (max (caddr (cadr bbox1)) (caddr (cadr bbox2))))))

(defun ys:bbox-overlap-p (bbox1 bbox2 tol / a1 a2 b1 b2)
  (setq a1 (car bbox1)
        a2 (cadr bbox1)
        b1 (car bbox2)
        b2 (cadr bbox2))
  (and (>= (+ (car a2) tol) (car b1))
       (>= (+ (car b2) tol) (car a1))
       (>= (+ (cadr a2) tol) (cadr b1))
       (>= (+ (cadr b2) tol) (cadr a1))))

(defun ys:outer-bbox-p (bbox total-bbox tol / minpt maxpt total-min total-max)
  (setq minpt (car bbox)
        maxpt (cadr bbox)
        total-min (car total-bbox)
        total-max (cadr total-bbox))
  (or
    (<= (abs (- (car minpt) (car total-min))) tol)
    (<= (abs (- (cadr minpt) (cadr total-min))) tol)
    (<= (abs (- (car maxpt) (car total-max))) tol)
    (<= (abs (- (cadr maxpt) (cadr total-max))) tol)))

(defun ys:point-on-bbox-border-p (pt bbox tol / x y minpt maxpt)
  (if (and pt bbox)
    (progn
      (setq x (car pt)
            y (cadr pt)
            minpt (car bbox)
            maxpt (cadr bbox))
      (or
        (and (<= (- (cadr minpt) tol) y)
             (<= y (+ (cadr maxpt) tol))
             (or (<= (abs (- x (car minpt))) tol)
                 (<= (abs (- x (car maxpt))) tol)))
        (and (<= (- (car minpt) tol) x)
             (<= x (+ (car maxpt) tol))
             (or (<= (abs (- y (cadr minpt))) tol)
                 (<= (abs (- y (cadr maxpt))) tol)))))))

(defun ys:line-outer-by-exposure-p (obj bbox bbox-list total-bbox tol / minpt maxpt x y x1 x2 y1 y2 left-seen right-seen above-seen below-seen item other-obj other-bbox overlap startpt endpt)
  (setq minpt (car bbox)
        maxpt (cadr bbox))
  (cond
    ((<= (ys:bbox-width bbox) tol)
     (setq x (/ (+ (car minpt) (car maxpt)) 2.0)
           y1 (cadr minpt)
           y2 (cadr maxpt))
     (foreach item bbox-list
       (setq other-obj (car item)
             other-bbox (cdr item))
       (if (/= (vla-get-Handle other-obj) (vla-get-Handle obj))
         (progn
           (setq overlap
                  (ys:range-overlap-size
                    y1
                    y2
                    (cadr (car other-bbox))
                    (cadr (cadr other-bbox))))
           (if (> overlap tol)
             (progn
               (if (< (car (car other-bbox)) (- x tol))
                 (setq left-seen T))
               (if (> (car (cadr other-bbox)) (+ x tol))
                 (setq right-seen T)))))))
     (or (not left-seen) (not right-seen)))
    ((<= (ys:bbox-height bbox) tol)
     (setq y (/ (+ (cadr minpt) (cadr maxpt)) 2.0)
           x1 (car minpt)
           x2 (car maxpt))
     (foreach item bbox-list
       (setq other-obj (car item)
             other-bbox (cdr item))
       (if (/= (vla-get-Handle other-obj) (vla-get-Handle obj))
         (progn
           (setq overlap
                  (ys:range-overlap-size
                    x1
                    x2
                    (car (car other-bbox))
                    (car (cadr other-bbox))))
           (if (> overlap tol)
             (progn
               (if (< (cadr (car other-bbox)) (- y tol))
                 (setq below-seen T))
               (if (> (cadr (cadr other-bbox)) (+ y tol))
                 (setq above-seen T)))))))
     (or (not below-seen) (not above-seen)))
    (T
     (setq startpt (vlax-curve-getStartPoint obj)
           endpt (vlax-curve-getEndPoint obj))
     (and (ys:point-on-bbox-border-p startpt total-bbox tol)
          (ys:point-on-bbox-border-p endpt total-bbox tol)))))

(defun ys:outer-vla-object-p (obj bbox bbox-list total-bbox tol / kind minpt maxpt total-min total-max bw bh)
  (setq kind (ys:vla-object-name obj)
        minpt (car bbox)
        maxpt (cadr bbox)
        total-min (car total-bbox)
        total-max (cadr total-bbox)
        bw (ys:bbox-width bbox)
        bh (ys:bbox-height bbox))
  (cond
    ((= kind "AcDbLine")
     (or (ys:line-outer-by-exposure-p obj bbox bbox-list total-bbox tol)
         (ys:outer-bbox-p bbox total-bbox tol)))
    ((<= bw tol)
     (or (<= (abs (- (car minpt) (car total-min))) tol)
         (<= (abs (- (car minpt) (car total-max))) tol)))
    ((<= bh tol)
     (or (<= (abs (- (cadr minpt) (cadr total-min))) tol)
         (<= (abs (- (cadr minpt) (cadr total-max))) tol)))
    (T
     (ys:outer-bbox-p bbox total-bbox tol))))

(defun ys:group-objs-by-bbox (objs / pairs bbox groups seed group groupbbox added item next tol)
  (foreach obj objs
    (if (setq bbox (ys:vla-bbox obj))
      (setq pairs (append pairs (list (cons obj bbox))))))
  (setq tol (max 1.0 *ys-geom-tol*))
  (while pairs
    (setq seed (car pairs)
          pairs (cdr pairs)
          group (list (car seed))
          groupbbox (cdr seed)
          added T)
    (while added
      (setq added nil
            next nil)
      (foreach item pairs
        (if (ys:bbox-overlap-p groupbbox (cdr item) tol)
          (progn
            (setq group (append group (list (car item)))
                  groupbbox (ys:bbox-union groupbbox (cdr item))
                  added T))
          (setq next (append next (list item)))))
      (setq pairs next))
    (setq groups (append groups (list group))))
  groups)

(defun ys:style-vla-objects-by-bbox (objs / bbox-list item obj bbox total-bbox minpt maxpt total-min total-max tol changed)
  (foreach obj objs
    (if (and (ys:vla-curve-object-p obj)
             (setq bbox (ys:vla-bbox obj)))
      (setq bbox-list (cons (cons obj bbox) bbox-list))))
  (setq bbox-list (reverse bbox-list))
  (if bbox-list
    (progn
      (setq total-min (car (cdar bbox-list))
            total-max (cadr (cdar bbox-list)))
      (foreach item (cdr bbox-list)
        (setq bbox (cdr item)
              minpt (car bbox)
              maxpt (cadr bbox)
              total-min
                (list
                  (min (car total-min) (car minpt))
                  (min (cadr total-min) (cadr minpt))
                  (min (caddr total-min) (caddr minpt)))
              total-max
                (list
                  (max (car total-max) (car maxpt))
                  (max (cadr total-max) (cadr maxpt))
                  (max (caddr total-max) (caddr maxpt)))))
      (setq total-bbox (list total-min total-max)
            tol (max 1.0 *ys-geom-tol*))
      (foreach item bbox-list
        (setq obj (car item)
              bbox (cdr item))
        (if (ys:outer-vla-object-p obj bbox bbox-list total-bbox tol)
          (ys:style-outer-vla-object obj)
          (ys:style-inner-vla-object obj total-bbox))
        (setq changed (1+ (if changed changed 0))))
      changed)
    0))

(defun ys:style-vla-objects-as-furniture (objs / groups group changed)
  (setq groups (ys:group-objs-by-bbox objs))
  (foreach group groups
    (setq changed (+ (if changed changed 0)
                     (ys:style-vla-objects-by-bbox group))))
  (if changed changed 0))

(defun ys:style-block-definition-recursive (doc block-name visited / key block-obj direct-curves nested-blocks item child-name child-names result changed skipped)
  (setq key (strcase block-name))
  (if (member key visited)
    (list 0 0 visited)
    (progn
      (setq visited (cons key visited)
            block-obj (ys:get-block-definition doc block-name))
      (if (not (ys:block-definition-editable-p block-obj))
        (list 0 1 visited)
        (progn
          (vlax-for item block-obj
            (cond
              ((ys:vla-curve-object-p item)
               (setq direct-curves (cons item direct-curves)))
              ((ys:vla-blockref-p item)
               (setq child-names (ys:blockref-names item))
               (foreach child-name child-names
                 (setq nested-blocks (ys:append-unique-string nested-blocks child-name))))))
          (setq changed (ys:style-vla-objects-as-furniture (reverse direct-curves))
                skipped 0)
          (foreach child-name (reverse nested-blocks)
            (setq result (ys:style-block-definition-recursive doc child-name visited)
                  changed (+ changed (car result))
                  skipped (+ skipped (cadr result))
                  visited (caddr result)))
          (list changed skipped visited))))))

(defun ys:style-entities-as-furniture (doc enames / objs skipped ename obj result changed visited block-name block-names)
  (foreach ename enames
    (if (setq obj (vlax-ename->vla-object ename))
      (cond
        ((ys:vla-curve-object-p obj)
         (setq objs (cons obj objs)))
        ((ys:vla-blockref-p obj)
         (setq block-names (ys:blockref-names obj))
         (if block-names
           (foreach block-name block-names
             (setq result (ys:style-block-definition-recursive doc block-name visited)
                   changed (+ (if changed changed 0) (car result))
                   skipped (+ (if skipped skipped 0) (cadr result))
                   visited (caddr result)))
           (setq skipped (1+ (if skipped skipped 0)))))
        (T
         (setq skipped (1+ (if skipped skipped 0)))))
      (setq skipped (1+ (if skipped skipped 0)))))
  (list
    (+ (if changed changed 0)
       (ys:style-vla-objects-as-furniture (reverse objs)))
    (if skipped skipped 0)))

(defun ys:purge-document (doc / result pass)
  (setq pass 0)
  (while (< pass 4)
    (setq result (vl-catch-all-apply 'vla-PurgeAll (list doc))
          pass (1+ pass))
    (if (vl-catch-all-error-p result)
      (setq pass 4)))
  (if (vl-catch-all-error-p result)
    nil
    T))

(defun c:R (/ *error* olderr doc ename data count)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
        olderr *error*)
  (defun *error* (msg)
    (ys:end-undo doc)
    (setq *error* olderr)
     (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
       (princ (strcat "\nError: " msg)))
     (princ))
  (vla-StartUndoMark doc)
  (ys:apply-linetype-standards)
  (setq ename (ys:entity-or-prompt "\nSelect a closed straight cabinet polyline: "))
  (cond
    ((not ename)
     (princ "\nNothing selected."))
    ((not (setq data (ys:straight-cabinet-data ename)))
     (princ "\nPlease select a closed straight cabinet polyline (rectangle or rounded-end)."))
     ((not (setq count (ys:draw-straight-cabinet ename data)))
      (princ "\nFailed to generate cabinet layout."))
     (T
      (ys:style-outer-entity ename)
      (princ
        (strcat
          "\nCabinet layout created. Panels: "
         (itoa count)
         ". Step = "
         (rtos *ys-cabinet-target-width* 2 2)))))
  (ys:end-undo doc)
  (setq *error* olderr)
  (princ))

(defun c:Q (/ *error* olderr doc ss result changed skipped)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
        olderr *error*)
  (defun *error* (msg)
    (ys:end-undo doc)
    (setq *error* olderr)
     (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
       (princ (strcat "\nError: " msg)))
     (princ))
  (vla-StartUndoMark doc)
  (ys:apply-linetype-standards)
  (setq ss (ys:ss-or-prompt "\nSelect furniture geometry to restyle: "))
  (cond
    ((not ss)
     (princ "\nNothing selected."))
    (T
     (setq result (ys:style-entities-as-furniture doc (ys:ss->enames ss))
           changed (car result)
           skipped (cadr result))
     (ys:regen doc)
     (princ
       (strcat
         "\nFurniture style applied. Changed: "
         (itoa changed)
         (if (> skipped 0)
           (strcat ", skipped: " (itoa skipped) " (unsupported objects)")
           "")))))
  (ys:end-undo doc)
  (setq *error* olderr)
  (princ))

(defun c:P (/ *error* olderr doc removed-p purged-p)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
        olderr *error*)
  (defun *error* (msg)
    (ys:end-undo doc)
    (setq *error* olderr)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg)))
    (princ))
  (vla-StartUndoMark doc)
  (if (dictsearch (namedobjdict) "ACAD_DGNLINESTYLECOMP")
    (progn
      (dictremove (namedobjdict) "ACAD_DGNLINESTYLECOMP")
      (setq removed-p T))
    (setq removed-p nil))
  (setq purged-p (ys:purge-document doc))
  (ys:regen doc)
  (princ
    (strcat
      "\nCleanup finished. "
      (if removed-p
        "ACAD_DGNLINESTYLECOMP removed. "
        "ACAD_DGNLINESTYLECOMP not found. ")
      (if purged-p
        "PURGE ALL applied."
        "PURGE ALL unavailable in this CAD.")))
  (ys:end-undo doc)
  (setq *error* olderr)
  (princ))

(defun c:B (/ *error* olderr doc ss bbox basept blkname blkdef block-ref first-obj obj-var)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
        olderr *error*)
  (defun *error* (msg)
    (ys:end-undo doc)
    (setq *error* olderr)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg)))
    (princ))
  (vla-StartUndoMark doc)
  (setq ss (ys:ss-or-prompt "\nSelect objects to quick-block: "))
  (cond
    ((not ss)
     (princ "\nNothing selected."))
    ((not (setq bbox (ys:ss-bbox ss)))
     (princ "\nUnable to calculate bounds for selection."))
    (T
     (setq basept (car bbox)
           blkname (ys:unique-block-name)
           blkdef (vla-Add (vla-get-Blocks doc) (vlax-3d-point basept) blkname)
           first-obj (vlax-ename->vla-object (ssname ss 0))
           obj-var (ys:ss->object-variant ss))
     (vla-CopyObjects doc obj-var blkdef)
     (setq block-ref
            (vla-InsertBlock
              (ys:current-space doc)
              (vlax-3d-point basept)
              blkname
              1.0
              1.0
              1.0
              0.0))
     (vla-put-Layer block-ref (vla-get-Layer first-obj))
     (ys:delete-ss ss)
     (princ (strcat "\nQuick block created: " blkname))))
  (ys:end-undo doc)
  (setq *error* olderr)
  (princ))

(princ "\nYS CAD tools loaded. Commands: Q, P, B, R.")
(princ)
