(vl-load-com)

(setq *ysrw-geom-tol* 1.0
      *ysrw-min-room-width* 1200.0
      *ysrw-min-room-height* 1200.0
      *ysrw-min-room-area* 1000000.0
      *ysrw-max-room-area-ratio* 0.35
      *ysrw-dim-offset-min* 120.0
      *ysrw-dim-offset-max* 300.0
      *ysrw-bbox-key-tol* 5.0)

(defun ysrw:3dpt (pt)
  (list
    (float (car pt))
    (float (cadr pt))
    (float (if (caddr pt) (caddr pt) 0.0))))

(defun ysrw:vector (p1 p2)
  (mapcar '- (ysrw:3dpt p2) (ysrw:3dpt p1)))

(defun ysrw:length (v)
  (distance '(0.0 0.0 0.0) v))

(defun ysrw:cross2d (a b)
  (- (* (car a) (cadr b))
     (* (cadr a) (car b))))

(defun ysrw:axis-aligned-segment-p (p1 p2)
  (or (<= (abs (- (car p1) (car p2))) *ysrw-geom-tol*)
      (<= (abs (- (cadr p1) (cadr p2))) *ysrw-geom-tol*)))

(defun ysrw:collinear-3pt-p (p1 p2 p3 / v1 v2)
  (setq v1 (ysrw:vector p1 p2)
        v2 (ysrw:vector p2 p3))
  (and (<= (abs (ysrw:cross2d v1 v2))
           (max 1e-6 (* *ysrw-geom-tol* (max (ysrw:length v1) (ysrw:length v2) 1.0))))
       (ysrw:axis-aligned-segment-p p1 p2)
       (ysrw:axis-aligned-segment-p p2 p3)))

(defun ysrw:simplify-closed-pts-once (pts / len idx prev curr next result)
  (setq len (length pts)
        idx 0)
  (while (< idx len)
    (setq prev (nth (if (= idx 0) (1- len) (1- idx)) pts)
          curr (nth idx pts)
          next (nth (rem (1+ idx) len) pts))
    (if (not (ysrw:collinear-3pt-p prev curr next))
      (setq result (cons curr result)))
    (setq idx (1+ idx)))
  (reverse result))

(defun ysrw:simplify-closed-pts (pts / prev cur)
  (setq prev nil
        cur pts)
  (while (not (equal prev cur 1e-6))
    (setq prev cur
          cur (ysrw:simplify-closed-pts-once cur)))
  cur)

(defun ysrw:ss-all-or-implied (filter / ss)
  (cond
    ((setq ss (ssget "_I" filter)) ss)
    ((setq ss (ssget "_X" filter)) ss)))

(defun ysrw:ss->enames (ss / idx result)
  (setq idx 0)
  (while (< idx (sslength ss))
    (setq result (cons (ssname ss idx) result)
          idx (1+ idx)))
  (reverse result))

(defun ysrw:entity-type (ename / data)
  (if (and ename (setq data (entget ename)))
    (cdr (assoc 0 data))))

(defun ysrw:polyline-entity-p (ename / kind)
  (setq kind (ysrw:entity-type ename))
  (member kind '("LWPOLYLINE" "POLYLINE")))

(defun ysrw:lwpoly-vertices (ename / data pts)
  (setq data (entget ename))
  (foreach item data
    (if (= 10 (car item))
      (setq pts (cons (ysrw:3dpt (cdr item)) pts))))
  (reverse pts))

(defun ysrw:oldpoly-vertices (ename / next data kind pts done)
  (setq next (entnext ename)
        done nil)
  (while (and next (not done))
    (setq data (entget next)
          kind (cdr (assoc 0 data)))
    (cond
      ((= "VERTEX" kind)
       (setq pts (cons (ysrw:3dpt (cdr (assoc 10 data))) pts)))
      ((= "SEQEND" kind)
       (setq done T)))
    (if (not done)
      (setq next (entnext next))))
  (reverse pts))

(defun ysrw:polyline-vertices (ename / kind)
  (setq kind (ysrw:entity-type ename))
  (cond
    ((= "LWPOLYLINE" kind) (ysrw:lwpoly-vertices ename))
    ((= "POLYLINE" kind) (ysrw:oldpoly-vertices ename))))

(defun ysrw:polyline-closed-p (ename / data)
  (setq data (entget ename))
  (= 1 (logand 1 (if (assoc 70 data) (cdr (assoc 70 data)) 0))))

(defun ysrw:polyline-has-bulge-p (ename / kind data next found)
  (setq kind (ysrw:entity-type ename))
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

(defun ysrw:entity-bbox (ename / obj pmin pmax)
  (if (and ename (setq obj (vlax-ename->vla-object ename)))
    (progn
      (vla-GetBoundingBox obj 'pmin 'pmax)
      (list
        (vlax-safearray->list pmin)
        (vlax-safearray->list pmax)))))

(defun ysrw:bbox-width (bbox)
  (- (car (cadr bbox)) (car (car bbox))))

(defun ysrw:bbox-height (bbox)
  (- (cadr (cadr bbox)) (cadr (car bbox))))

(defun ysrw:bbox-area (bbox)
  (* (ysrw:bbox-width bbox) (ysrw:bbox-height bbox)))

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

(defun ysrw:polygon-area2 (pts / total idx p1 p2)
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

(defun ysrw:orthogonal-pts-p (pts / ok idx)
  (setq ok T
        idx 0)
  (while (and ok (< idx (length pts)))
    (if (not (ysrw:axis-aligned-segment-p
               (nth idx pts)
               (nth (rem (1+ idx) (length pts)) pts)))
      (setq ok nil))
    (setq idx (1+ idx)))
  ok)

(defun ysrw:round-key (value step)
  (* step (fix (+ 0.5 (/ value step)))))

(defun ysrw:bbox-key (bbox)
  (strcat
    (rtos (ysrw:round-key (car (car bbox)) *ysrw-bbox-key-tol*) 2 3) ","
    (rtos (ysrw:round-key (cadr (car bbox)) *ysrw-bbox-key-tol*) 2 3) ","
    (rtos (ysrw:round-key (car (cadr bbox)) *ysrw-bbox-key-tol*) 2 3) ","
    (rtos (ysrw:round-key (cadr (cadr bbox)) *ysrw-bbox-key-tol*) 2 3)))

(defun ysrw:append-unique-key (vals key)
  (if (member key vals)
    vals
    (cons key vals)))

(defun ysrw:room-data-from-ename (ename / pts bbox width height area key)
  (if (and (ysrw:polyline-entity-p ename)
           (ysrw:polyline-closed-p ename)
           (not (ysrw:polyline-has-bulge-p ename)))
    (progn
      (setq pts (ysrw:simplify-closed-pts (ysrw:polyline-vertices ename))
            bbox (ysrw:entity-bbox ename))
      (if (and pts
               bbox
               (>= (length pts) 4)
               (ysrw:orthogonal-pts-p pts))
        (progn
          (setq width (ysrw:bbox-width bbox)
                height (ysrw:bbox-height bbox)
                area (/ (abs (ysrw:polygon-area2 pts)) 2.0)
                key (ysrw:bbox-key bbox))
          (list
            (cons 'ename ename)
            (cons 'pts pts)
            (cons 'bbox bbox)
            (cons 'key key)
            (cons 'width width)
            (cons 'height height)
            (cons 'area area)))))))

(defun ysrw:bbox-strictly-contains-p (outer inner / omin omax imin imax)
  (setq omin (car outer)
        omax (cadr outer)
        imin (car inner)
        imax (cadr inner))
  (and (< (car omin) (- (car imin) *ysrw-geom-tol*))
       (< (cadr omin) (- (cadr imin) *ysrw-geom-tol*))
       (> (car omax) (+ (car imax) *ysrw-geom-tol*))
       (> (cadr omax) (+ (cadr imax) *ysrw-geom-tol*))))

(defun ysrw:room-sized-p (room total-area)
  (and room
       (>= (cdr (assoc 'width room)) *ysrw-min-room-width*)
       (>= (cdr (assoc 'height room)) *ysrw-min-room-height*)
       (>= (cdr (assoc 'area room)) *ysrw-min-room-area*)
       (or (not total-area)
           (<= (cdr (assoc 'area room)) (* total-area *ysrw-max-room-area-ratio*)))))

(defun ysrw:contains-large-subrooms-p (room rooms / count other)
  (setq count 0)
  (foreach other rooms
    (if (and (/= (cdr (assoc 'key room)) (cdr (assoc 'key other)))
             (ysrw:bbox-strictly-contains-p
               (cdr (assoc 'bbox room))
               (cdr (assoc 'bbox other)))
             (>= (cdr (assoc 'area other)) *ysrw-min-room-area*)
             (< (cdr (assoc 'area other)) (* (cdr (assoc 'area room)) 0.9)))
      (setq count (1+ count))))
  (>= count 2))

(defun ysrw:dedupe-rooms (rooms / seen result key)
  (foreach room rooms
    (setq key (cdr (assoc 'key room)))
    (if (not (member key seen))
      (progn
        (setq seen (cons key seen))
        (setq result (cons room result)))))
  (reverse result))

(defun ysrw:collect-room-candidates (/ ss enames rooms bbox total-bbox total-area room)
  (setq ss (ysrw:ss-all-or-implied '((0 . "LWPOLYLINE,POLYLINE"))))
  (if ss
    (progn
      (setq enames (ysrw:ss->enames ss))
      (foreach ename enames
        (if (setq room (ysrw:room-data-from-ename ename))
          (progn
            (setq rooms (cons room rooms))
            (if total-bbox
              (setq total-bbox (ysrw:bbox-union total-bbox (cdr (assoc 'bbox room))))
              (setq total-bbox (cdr (assoc 'bbox room)))))))
      (setq rooms (ysrw:dedupe-rooms (reverse rooms))
            total-area (if total-bbox (ysrw:bbox-area total-bbox)))
      (foreach room rooms
        (if (and (ysrw:room-sized-p room total-area)
                 (not (ysrw:contains-large-subrooms-p room rooms)))
          (setq bbox (cons room bbox))))
      (reverse bbox))))

(defun ysrw:current-space (doc)
  (if (= 1 (getvar "CVPORT"))
    (vla-get-PaperSpace doc)
    (vla-get-ModelSpace doc)))

(defun ysrw:room-dim-offset (room)
  (min *ysrw-dim-offset-max*
       (max *ysrw-dim-offset-min*
            (* 0.18 (cdr (assoc 'height room))))))

(defun ysrw:add-room-width-dim (space room / bbox minpt maxpt midx midy dimy obj)
  (setq bbox (cdr (assoc 'bbox room))
        minpt (car bbox)
        maxpt (cadr bbox)
        midx (/ (+ (car minpt) (car maxpt)) 2.0)
        midy (/ (+ (cadr minpt) (cadr maxpt)) 2.0)
        dimy (+ midy (ysrw:room-dim-offset room)))
  (setq obj
         (vla-AddDimRotated
           space
           (vlax-3d-point (list (car minpt) midy 0.0))
           (vlax-3d-point (list (car maxpt) midy 0.0))
            (vlax-3d-point (list midx dimy 0.0))
            0.0))
  obj)

(defun ysrw:room-minpt (room)
  (car (cdr (assoc 'bbox room))))

(defun ysrw:sort-rooms (rooms)
  (vl-sort
    rooms
    '(lambda (a b)
       (if (equal (cadr (ysrw:room-minpt a))
                  (cadr (ysrw:room-minpt b))
                  1e-6)
         (< (car (ysrw:room-minpt a))
            (car (ysrw:room-minpt b)))
         (> (cadr (ysrw:room-minpt a))
            (cadr (ysrw:room-minpt b)))))))

(defun ysrw:end-undo (doc)
  (if doc
    (vl-catch-all-apply 'vla-EndUndoMark (list doc))))

(defun ysrw:run (/ *error* olderr doc space rooms count)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
        space (ysrw:current-space doc)
        olderr *error*)
  (defun *error* (msg)
    (ysrw:end-undo doc)
    (setq *error* olderr)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg)))
    (princ))
  (vla-StartUndoMark doc)
  (setq rooms (ysrw:sort-rooms (ysrw:collect-room-candidates)))
  (cond
    ((not rooms)
     (princ "\nNo orthogonal closed room polylines found."))
    (T
     (foreach room rooms
       (if (ysrw:add-room-width-dim space room)
         (setq count (1+ (if count count 0)))))
     (princ
       (strcat
         "\nRoom width dimensions created: "
         (itoa (if count count 0))
         ". Uses current dimstyle and current layer."))))
  (ysrw:end-undo doc)
  (setq *error* olderr)
  (princ))

(defun c:YKJ ()
  (ysrw:run))

(defun c:YROOMW ()
  (ysrw:run))

(princ "\nYS room width loaded. Commands: YKJ, YROOMW.")
(princ)
