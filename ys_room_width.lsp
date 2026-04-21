(vl-load-com)

(setq *ysrw-geom-tol* 1.0
      *ysrw-row-tol* 5.0
      *ysrw-side-dim-offset* 700.0
      *ysrw-total-dim-gap* 700.0
      *ysrw-max-wall-thickness* 1200.0
      *ysrw-green-aci* 3
      *ysrw-white-aci* 7
      *ysrw-white-truecolor* 16777215)

(defun ysrw:3dpt (pt)
  (list
    (float (car pt))
    (float (cadr pt))
    (float (if (caddr pt) (caddr pt) 0.0))))

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

(defun ysrw:entity-layer (ename / data)
  (if (setq data (entget ename))
    (cdr (assoc 8 data))))

(defun ysrw:entity-truecolor (ename / data item)
  (setq data (entget ename)
        item (assoc 420 data))
  (if item
    (cdr item)))

(defun ysrw:layer-truecolor (layer / data item)
  (setq data (tblsearch "LAYER" layer)
        item (if data (assoc 420 data)))
  (if item
    (cdr item)))

(defun ysrw:layer-aci-color (layer / data item)
  (setq data (tblsearch "LAYER" layer)
        item (if data (assoc 62 data)))
  (if item
    (abs (cdr item))))

(defun ysrw:entity-aci-color (ename / data item layer)
  (setq data (entget ename)
        item (assoc 62 data)
        layer (ysrw:entity-layer ename))
  (cond
    ((and item (not (member (abs (cdr item)) '(0 256))))
     (abs (cdr item)))
    (layer
     (ysrw:layer-aci-color layer))))

(defun ysrw:white-wall-p (ename / layer truecolor aci)
  (setq layer (ysrw:entity-layer ename)
        truecolor (or (ysrw:entity-truecolor ename)
                      (if layer (ysrw:layer-truecolor layer)))
        aci (ysrw:entity-aci-color ename))
  (or (= truecolor *ysrw-white-truecolor*)
      (= aci *ysrw-white-aci*)))

(defun ysrw:wall-candidate-p (ename / aci)
  (setq aci (ysrw:entity-aci-color ename))
  (or (ysrw:white-wall-p ename)
      (= aci *ysrw-green-aci*)))

(defun ysrw:axis-aligned-segment-p (p1 p2)
  (or (<= (abs (- (car p1) (car p2))) *ysrw-geom-tol*)
      (<= (abs (- (cadr p1) (cadr p2))) *ysrw-geom-tol*)))

(defun ysrw:make-segment (p1 p2 / x y)
  (cond
    ((<= (abs (- (car p1) (car p2))) *ysrw-geom-tol*)
     (setq x (/ (+ (car p1) (car p2)) 2.0))
     (list 'vertical x (min (cadr p1) (cadr p2)) (max (cadr p1) (cadr p2))))
    ((<= (abs (- (cadr p1) (cadr p2))) *ysrw-geom-tol*)
     (setq y (/ (+ (cadr p1) (cadr p2)) 2.0))
     (list 'horizontal y (min (car p1) (car p2)) (max (car p1) (car p2))))))

(defun ysrw:line-segments (ename / data p1 p2 seg)
  (setq data (entget ename)
        p1 (ysrw:3dpt (cdr (assoc 10 data)))
        p2 (ysrw:3dpt (cdr (assoc 11 data)))
        seg (ysrw:make-segment p1 p2))
  (if seg
    (list seg)))

(defun ysrw:polyline-segments (ename / pts closed len idx p1 p2 seg result)
  (if (not (ysrw:polyline-has-bulge-p ename))
    (progn
      (setq pts (ysrw:polyline-vertices ename)
            closed (ysrw:polyline-closed-p ename)
            len (length pts)
            idx 0)
      (while (< idx (if closed len (1- len)))
        (setq p1 (nth idx pts)
              p2 (nth (rem (1+ idx) len) pts)
              seg (ysrw:make-segment p1 p2))
        (if seg
          (setq result (cons seg result)))
        (setq idx (1+ idx)))
      (reverse result))))

(defun ysrw:entity-segments (ename / kind)
  (setq kind (ysrw:entity-type ename))
  (cond
    ((= "LINE" kind) (ysrw:line-segments ename))
    ((ysrw:polyline-entity-p ename) (ysrw:polyline-segments ename))))

(defun ysrw:collect-wall-data (/ ss enames ename bbox total-bbox segments ref-segments entity-segments count ref-count)
  (setq ss (ysrw:ss-all-or-implied '((0 . "LINE,LWPOLYLINE,POLYLINE"))))
  (if ss
    (progn
      (setq enames (ysrw:ss->enames ss))
      (foreach ename enames
        (if (and (ysrw:wall-candidate-p ename)
                 (setq bbox (ysrw:entity-bbox ename))
                 (setq entity-segments (ysrw:entity-segments ename)))
          (progn
            (setq count (1+ (if count count 0))
                  total-bbox (if total-bbox
                               (ysrw:bbox-union total-bbox bbox)
                               bbox)
                  segments (append segments entity-segments))
            (if (ysrw:white-wall-p ename)
              (setq ref-count (1+ (if ref-count ref-count 0))
                    ref-segments (append ref-segments entity-segments))))))
      (if total-bbox
        (list
          (cons 'bbox total-bbox)
          (cons 'segments segments)
          (cons 'ref-segments ref-segments)
          (cons 'count count)
          (cons 'ref-count (if ref-count ref-count 0)))))))

(defun ysrw:coord-inside-span-p (val minv maxv)
  (and (> val (+ minv *ysrw-geom-tol*))
       (< val (- maxv *ysrw-geom-tol*))))

(defun ysrw:coord-inside-single-span-p (val span / minv maxv)
  (setq minv (car span)
        maxv (cadr span))
  (and (>= val (- minv *ysrw-row-tol*))
       (<= val (+ maxv *ysrw-row-tol*))))

(defun ysrw:coord-inside-spans-p (val spans / inside)
  (foreach span spans
    (if (ysrw:coord-inside-single-span-p val span)
      (setq inside T)))
  inside)

(defun ysrw:sort-unique-values (vals / sorted result)
  (setq sorted (vl-sort vals '<))
  (foreach val sorted
    (if (or (not result)
            (not (equal val (car result) *ysrw-geom-tol*)))
      (setq result (cons val result))))
  (reverse result))

(defun ysrw:row-key (val)
  (* *ysrw-row-tol* (fix (+ 0.5 (/ val *ysrw-row-tol*)))))

(defun ysrw:add-span-to-bucket (buckets key span / item)
  (if (setq item (assoc key buckets))
    (setq buckets
           (subst
             (cons key (cons span (cdr item)))
             item
             buckets))
    (setq buckets (cons (cons key (list span)) buckets)))
  buckets)

(defun ysrw:merge-spans (spans / sorted span current result)
  (setq sorted
         (vl-sort
           spans
           '(lambda (a b)
              (< (car a) (car b)))))
  (foreach span sorted
    (cond
      ((not current)
       (setq current span))
      ((<= (car span) (+ (cadr current) *ysrw-geom-tol*))
       (setq current
              (list
                (car current)
                (max (cadr current) (cadr span)))))
      (T
       (setq result (cons current result)
             current span))))
  (if current
    (setq result (cons current result)))
  (reverse result))

(defun ysrw:span-list-length (spans / total)
  (foreach span spans
    (setq total (+ (if total total 0.0)
                   (max 0.0 (- (cadr span) (car span))))))
  (if total total 0.0))

(defun ysrw:outer-horizontal-row (segments bbox side / minx maxx miny maxy side-len threshold buckets seg y span rows row coord spans merged cover dist best fallback)
  (setq minx (car (car bbox))
        maxx (car (cadr bbox))
        miny (cadr (car bbox))
        maxy (cadr (cadr bbox))
        side-len (- maxx minx)
        threshold (max 1000.0 (* side-len 0.15)))
  (foreach seg segments
    (if (= 'horizontal (car seg))
      (progn
        (setq y (cadr seg)
              span (list (caddr seg) (cadddr seg)))
        (if (> (- (cadr span) (car span)) *ysrw-geom-tol*)
          (setq buckets (ysrw:add-span-to-bucket buckets (ysrw:row-key y) span))))))
  (setq rows buckets)
  (foreach row rows
    (setq coord (car row)
          spans (cdr row)
          merged (ysrw:merge-spans spans)
          cover (ysrw:span-list-length merged)
          dist (cond
                 ((eq side 'top) (- maxy coord))
                 (T (- coord miny))))
    (if (or (not fallback)
            (< dist (car fallback))
            (and (equal dist (car fallback) *ysrw-row-tol*)
                 (> cover (cadr fallback))))
      (setq fallback (list dist cover coord merged)))
    (if (>= cover threshold)
      (if (or (not best)
              (< dist (car best))
              (and (equal dist (car best) *ysrw-row-tol*)
                   (> cover (cadr best))))
        (setq best (list dist cover coord merged)))))
  (if best
    (list (caddr best) (cadddr best))
    (if fallback
      (list (caddr fallback) (cadddr fallback)))))

(defun ysrw:outer-vertical-row (segments bbox side / minx maxx miny maxy side-len threshold buckets seg x span rows row coord spans merged cover dist best fallback)
  (setq minx (car (car bbox))
        maxx (car (cadr bbox))
        miny (cadr (car bbox))
        maxy (cadr (cadr bbox))
        side-len (- maxy miny)
        threshold (max 1000.0 (* side-len 0.15)))
  (foreach seg segments
    (if (= 'vertical (car seg))
      (progn
        (setq x (cadr seg)
              span (list (caddr seg) (cadddr seg)))
        (if (> (- (cadr span) (car span)) *ysrw-geom-tol*)
          (setq buckets (ysrw:add-span-to-bucket buckets (ysrw:row-key x) span))))))
  (setq rows buckets)
  (foreach row rows
    (setq coord (car row)
          spans (cdr row)
          merged (ysrw:merge-spans spans)
          cover (ysrw:span-list-length merged)
          dist (cond
                 ((eq side 'left) (- coord minx))
                 (T (- maxx coord))))
    (if (or (not fallback)
            (< dist (car fallback))
            (and (equal dist (car fallback) *ysrw-row-tol*)
                 (> cover (cadr fallback))))
      (setq fallback (list dist cover coord merged)))
    (if (>= cover threshold)
      (if (or (not best)
              (< dist (car best))
              (and (equal dist (car best) *ysrw-row-tol*)
                   (> cover (cadr best))))
        (setq best (list dist cover coord merged)))))
  (if best
    (list (caddr best) (cadddr best))
    (if fallback
      (list (caddr fallback) (cadddr fallback)))))

(defun ysrw:best-horizontal-row (segments bbox side / minx maxx miny maxy side-len threshold buckets seg y span key rows row coord spans merged cover dist best fallback)
  (setq minx (car (car bbox))
        maxx (car (cadr bbox))
        miny (cadr (car bbox))
        maxy (cadr (cadr bbox))
        side-len (- maxx minx)
        threshold (max 1000.0 (* side-len 0.15)))
  (foreach seg segments
    (if (= 'horizontal (car seg))
      (progn
        (setq y (cadr seg)
              span (list (caddr seg) (cadddr seg)))
        (if (and (> (- (cadr span) (car span)) *ysrw-geom-tol*)
                 (cond
                   ((eq side 'top) (< y (- maxy *ysrw-geom-tol*)))
                   ((eq side 'bottom) (> y (+ miny *ysrw-geom-tol*)))))
          (setq buckets (ysrw:add-span-to-bucket buckets (ysrw:row-key y) span))))))
  (setq rows buckets)
  (foreach row rows
    (setq coord (car row)
          spans (cdr row)
          merged (ysrw:merge-spans spans)
          cover (ysrw:span-list-length merged)
          dist (cond
                 ((eq side 'top) (- maxy coord))
                 (T (- coord miny))))
    (if (or (not fallback)
            (< dist (car fallback))
            (and (equal dist (car fallback) *ysrw-row-tol*)
                 (> cover (cadr fallback))))
      (setq fallback (list dist cover coord merged)))
    (if (>= cover threshold)
      (if (or (not best)
              (< dist (car best))
              (and (equal dist (car best) *ysrw-row-tol*)
                   (> cover (cadr best))))
        (setq best (list dist cover coord merged)))))
  (if best
    (list (caddr best) (cadddr best))
    (if fallback
      (list (caddr fallback) (cadddr fallback)))))

(defun ysrw:inner-horizontal-row (segments bbox side outercoord / minx maxx miny maxy side-len threshold buckets seg y span rows row coord spans merged cover dist best fallback)
  (setq minx (car (car bbox))
        maxx (car (cadr bbox))
        miny (cadr (car bbox))
        maxy (cadr (cadr bbox))
        side-len (- maxx minx)
        threshold (max 1000.0 (* side-len 0.15)))
  (foreach seg segments
    (if (= 'horizontal (car seg))
      (progn
        (setq y (cadr seg)
              span (list (caddr seg) (cadddr seg)))
        (if (and (> (- (cadr span) (car span)) *ysrw-geom-tol*)
                 (cond
                   ((eq side 'top)
                    (and (< y (- outercoord *ysrw-geom-tol*))
                         (<= (- outercoord y) *ysrw-max-wall-thickness*)))
                   ((eq side 'bottom)
                    (and (> y (+ outercoord *ysrw-geom-tol*))
                         (<= (- y outercoord) *ysrw-max-wall-thickness*)))))
          (setq buckets (ysrw:add-span-to-bucket buckets (ysrw:row-key y) span))))))
  (foreach row buckets
    (setq coord (car row)
          spans (cdr row)
          merged (ysrw:merge-spans spans)
          cover (ysrw:span-list-length merged)
          dist (abs (- outercoord coord)))
    (if (or (not fallback)
            (< dist (car fallback))
            (and (equal dist (car fallback) *ysrw-row-tol*)
                 (> cover (cadr fallback))))
      (setq fallback (list dist cover coord merged)))
    (if (>= cover threshold)
      (if (or (not best)
              (< dist (car best))
              (and (equal dist (car best) *ysrw-row-tol*)
                   (> cover (cadr best))))
        (setq best (list dist cover coord merged)))))
  (if best
    (list (caddr best) (cadddr best))
    (if fallback
      (list (caddr fallback) (cadddr fallback)))))

(defun ysrw:best-vertical-row (segments bbox side / minx maxx miny maxy side-len threshold buckets seg x span key rows row coord spans merged cover dist best fallback)
  (setq minx (car (car bbox))
        maxx (car (cadr bbox))
        miny (cadr (car bbox))
        maxy (cadr (cadr bbox))
        side-len (- maxy miny)
        threshold (max 1000.0 (* side-len 0.15)))
  (foreach seg segments
    (if (= 'vertical (car seg))
      (progn
        (setq x (cadr seg)
              span (list (caddr seg) (cadddr seg)))
        (if (and (> (- (cadr span) (car span)) *ysrw-geom-tol*)
                 (cond
                   ((eq side 'left) (> x (+ minx *ysrw-geom-tol*)))
                   ((eq side 'right) (< x (- maxx *ysrw-geom-tol*)))))
          (setq buckets (ysrw:add-span-to-bucket buckets (ysrw:row-key x) span))))))
  (setq rows buckets)
  (foreach row rows
    (setq coord (car row)
          spans (cdr row)
          merged (ysrw:merge-spans spans)
          cover (ysrw:span-list-length merged)
          dist (cond
                 ((eq side 'left) (- coord minx))
                 (T (- maxx coord))))
    (if (or (not fallback)
            (< dist (car fallback))
            (and (equal dist (car fallback) *ysrw-row-tol*)
                 (> cover (cadr fallback))))
      (setq fallback (list dist cover coord merged)))
    (if (>= cover threshold)
      (if (or (not best)
              (< dist (car best))
              (and (equal dist (car best) *ysrw-row-tol*)
                   (> cover (cadr best))))
        (setq best (list dist cover coord merged)))))
  (if best
    (list (caddr best) (cadddr best))
    (if fallback
      (list (caddr fallback) (cadddr fallback)))))

(defun ysrw:inner-vertical-row (segments bbox side outercoord / minx maxx miny maxy side-len threshold buckets seg x span rows row coord spans merged cover dist best fallback)
  (setq minx (car (car bbox))
        maxx (car (cadr bbox))
        miny (cadr (car bbox))
        maxy (cadr (cadr bbox))
        side-len (- maxy miny)
        threshold (max 1000.0 (* side-len 0.15)))
  (foreach seg segments
    (if (= 'vertical (car seg))
      (progn
        (setq x (cadr seg)
              span (list (caddr seg) (cadddr seg)))
        (if (and (> (- (cadr span) (car span)) *ysrw-geom-tol*)
                 (cond
                   ((eq side 'left)
                    (and (> x (+ outercoord *ysrw-geom-tol*))
                         (<= (- x outercoord) *ysrw-max-wall-thickness*)))
                   ((eq side 'right)
                    (and (< x (- outercoord *ysrw-geom-tol*))
                         (<= (- outercoord x) *ysrw-max-wall-thickness*)))))
          (setq buckets (ysrw:add-span-to-bucket buckets (ysrw:row-key x) span))))))
  (foreach row buckets
    (setq coord (car row)
          spans (cdr row)
          merged (ysrw:merge-spans spans)
          cover (ysrw:span-list-length merged)
          dist (abs (- outercoord coord)))
    (if (or (not fallback)
            (< dist (car fallback))
            (and (equal dist (car fallback) *ysrw-row-tol*)
                 (> cover (cadr fallback))))
      (setq fallback (list dist cover coord merged)))
    (if (>= cover threshold)
      (if (or (not best)
              (< dist (car best))
              (and (equal dist (car best) *ysrw-row-tol*)
                   (> cover (cadr best))))
        (setq best (list dist cover coord merged)))))
  (if best
    (list (caddr best) (cadddr best))
    (if fallback
      (list (caddr fallback) (cadddr fallback)))))

(defun ysrw:spans-min (spans / val)
  (foreach span spans
    (if (or (not val) (< (car span) val))
      (setq val (car span))))
  val)

(defun ysrw:spans-max (spans / val)
  (foreach span spans
    (if (or (not val) (> (cadr span) val))
      (setq val (cadr span))))
  val)

(defun ysrw:vertical-crosses-y-p (seg y)
  (and (= 'vertical (car seg))
       (<= (caddr seg) (+ y *ysrw-row-tol*))
       (>= (cadddr seg) (- y *ysrw-row-tol*))))

(defun ysrw:horizontal-crosses-x-p (seg x)
  (and (= 'horizontal (car seg))
       (<= (caddr seg) (+ x *ysrw-row-tol*))
       (>= (cadddr seg) (- x *ysrw-row-tol*))))

(defun ysrw:vertical-touches-horizontal-row-p (seg y side)
  (and (= 'vertical (car seg))
       (cond
         ((eq side 'top)
          (<= (abs (- (cadddr seg) y)) *ysrw-row-tol*))
         (T
          (<= (abs (- (caddr seg) y)) *ysrw-row-tol*)))))

(defun ysrw:horizontal-touches-vertical-row-p (seg x side)
  (and (= 'horizontal (car seg))
       (cond
         ((eq side 'right)
          (<= (abs (- (cadddr seg) x)) *ysrw-row-tol*))
         (T
          (<= (abs (- (caddr seg) x)) *ysrw-row-tol*)))))

(defun ysrw:vertical-connects-horizontal-row-p (seg y side)
  (and (= 'vertical (car seg))
       (<= (caddr seg) (+ y *ysrw-row-tol*))
       (>= (cadddr seg) (- y *ysrw-row-tol*))
       (cond
         ((eq side 'top)
          (< (caddr seg) (- y *ysrw-row-tol*)))
         (T
          (> (cadddr seg) (+ y *ysrw-row-tol*))))))

(defun ysrw:horizontal-connects-vertical-row-p (seg x side)
  (and (= 'horizontal (car seg))
       (<= (caddr seg) (+ x *ysrw-row-tol*))
       (>= (cadddr seg) (- x *ysrw-row-tol*))
       (cond
         ((eq side 'right)
         (< (caddr seg) (- x *ysrw-row-tol*)))
         (T
          (> (cadddr seg) (+ x *ysrw-row-tol*))))))

(defun ysrw:vertical-bridges-horizontal-band-p (seg outer inner)
  (and (= 'vertical (car seg))
       (<= (caddr seg) (+ inner *ysrw-row-tol*))
       (>= (cadddr seg) (- outer *ysrw-row-tol*))
       (<= (- (cadddr seg) (caddr seg))
           (+ (abs (- outer inner)) (* 2.0 *ysrw-row-tol*)))))

(defun ysrw:horizontal-bridges-vertical-band-p (seg outer inner)
  (and (= 'horizontal (car seg))
       (<= (caddr seg) (+ outer *ysrw-row-tol*))
       (>= (cadddr seg) (- inner *ysrw-row-tol*))
       (<= (- (cadddr seg) (caddr seg))
           (+ (abs (- inner outer)) (* 2.0 *ysrw-row-tol*)))))

(defun ysrw:pair-adjacent-values (vals / sorted idx x1 x2 gap result)
  (setq sorted (ysrw:sort-unique-values vals)
        idx 0)
  (while (< idx (1- (length sorted)))
    (setq x1 (nth idx sorted)
          x2 (nth (1+ idx) sorted)
          gap (- x2 x1))
    (cond
      ((and (> gap *ysrw-geom-tol*)
            (<= gap *ysrw-max-wall-thickness*))
       (setq result (cons (list x1 x2) result)
             idx (+ idx 2)))
      (T
       (setq idx (1+ idx)))))
  (reverse result))

(defun ysrw:pair-adjacent-values-in-spans (vals spans / span spanvals result)
  (foreach span spans
    (setq spanvals nil)
    (foreach val vals
      (if (ysrw:coord-inside-single-span-p val span)
        (setq spanvals (cons val spanvals))))
    (if spanvals
      (setq result (append result (ysrw:pair-adjacent-values spanvals)))))
  result)

(defun ysrw:top-wall-data (segments ref-segments bbox / ref-source outer-row inner-row outer-base coord spans vals seg x)
  (setq ref-source (if ref-segments ref-segments segments)
        outer-row (ysrw:outer-horizontal-row segments bbox 'top)
        inner-row (if outer-row
                    (ysrw:inner-horizontal-row ref-source bbox 'top (car outer-row))))
  (if outer-row
    (progn
      (setq outer-base (car outer-row))
      (if inner-row
        (progn
          (setq coord (car inner-row)
                spans (cadr inner-row))
          (foreach seg segments
            (if (and (ysrw:vertical-touches-horizontal-row-p seg coord 'top)
                     (setq x (cadr seg))
                     (ysrw:coord-inside-spans-p x spans))
              (setq vals (cons x vals))))))
      (list
        (cons 'base outer-base)
        (cons 'bodies (ysrw:pair-adjacent-values-in-spans vals spans))))))

(defun ysrw:bottom-wall-data (segments ref-segments bbox / ref-source outer-row inner-row outer-base coord spans vals seg x)
  (setq ref-source (if ref-segments ref-segments segments)
        outer-row (ysrw:outer-horizontal-row segments bbox 'bottom)
        inner-row (if outer-row
                    (ysrw:inner-horizontal-row ref-source bbox 'bottom (car outer-row))))
  (if outer-row
    (progn
      (setq outer-base (car outer-row))
      (if inner-row
        (progn
          (setq coord (car inner-row)
                spans (cadr inner-row))
          (foreach seg segments
            (if (and (ysrw:vertical-touches-horizontal-row-p seg coord 'bottom)
                     (setq x (cadr seg))
                     (ysrw:coord-inside-spans-p x spans))
              (setq vals (cons x vals))))))
      (list
        (cons 'base outer-base)
        (cons 'bodies (ysrw:pair-adjacent-values-in-spans vals spans))))))

(defun ysrw:left-wall-data (segments ref-segments bbox / ref-source outer-row inner-row outer-base coord spans vals seg y)
  (setq ref-source (if ref-segments ref-segments segments)
        outer-row (ysrw:outer-vertical-row segments bbox 'left)
        inner-row (if outer-row
                    (ysrw:inner-vertical-row ref-source bbox 'left (car outer-row))))
  (if outer-row
    (progn
      (setq outer-base (car outer-row))
      (if inner-row
        (progn
          (setq coord (car inner-row)
                spans (cadr inner-row))
          (foreach seg segments
            (if (and (ysrw:horizontal-touches-vertical-row-p seg coord 'left)
                     (setq y (cadr seg))
                     (ysrw:coord-inside-spans-p y spans))
              (setq vals (cons y vals))))))
      (list
        (cons 'base outer-base)
        (cons 'bodies (ysrw:pair-adjacent-values-in-spans vals spans))))))

(defun ysrw:right-wall-data (segments ref-segments bbox / ref-source outer-row inner-row outer-base coord spans vals seg y)
  (setq ref-source (if ref-segments ref-segments segments)
        outer-row (ysrw:outer-vertical-row segments bbox 'right)
        inner-row (if outer-row
                    (ysrw:inner-vertical-row ref-source bbox 'right (car outer-row))))
  (if outer-row
    (progn
      (setq outer-base (car outer-row))
      (if inner-row
        (progn
          (setq coord (car inner-row)
                spans (cadr inner-row))
          (foreach seg segments
            (if (and (ysrw:horizontal-touches-vertical-row-p seg coord 'right)
                     (setq y (cadr seg))
                     (ysrw:coord-inside-spans-p y spans))
              (setq vals (cons y vals))))))
      (list
        (cons 'base outer-base)
        (cons 'bodies (ysrw:pair-adjacent-values-in-spans vals spans))))))

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

(defun ysrw:add-horizontal-wall-bodies (space bodies base dimy / body)
  (foreach body bodies
    (ysrw:add-rotated-dim
      space
      (list (car body) base 0.0)
      (list (cadr body) base 0.0)
      (list (/ (+ (car body) (cadr body)) 2.0) dimy 0.0)
      0.0)))

(defun ysrw:add-vertical-wall-bodies (space bodies base dimx / body)
  (foreach body bodies
    (ysrw:add-rotated-dim
      space
      (list base (car body) 0.0)
      (list base (cadr body) 0.0)
      (list dimx (/ (+ (car body) (cadr body)) 2.0) 0.0)
      (/ pi 2.0))))

(defun ysrw:add-side-dims (space bbox segments ref-segments / minpt maxpt minx miny maxx maxy row total side-data bodies base created)
  (setq minpt (car bbox)
        maxpt (cadr bbox)
        minx (car minpt)
        miny (cadr minpt)
        maxx (car maxpt)
        maxy (cadr maxpt)
        row *ysrw-side-dim-offset*
        total (+ *ysrw-side-dim-offset* *ysrw-total-dim-gap*)
        created 0)

  (setq side-data (ysrw:top-wall-data segments ref-segments bbox)
        bodies (if side-data (cdr (assoc 'bodies side-data))))
  (if side-data
    (progn
      (if bodies
        (ysrw:add-horizontal-wall-bodies space bodies maxy (+ maxy row)))
      (ysrw:add-overall-horizontal space minx maxx maxy (+ maxy total))
      (setq created (+ created (length bodies) 1))))

  (setq side-data (ysrw:bottom-wall-data segments ref-segments bbox)
        bodies (if side-data (cdr (assoc 'bodies side-data))))
  (if side-data
    (progn
      (if bodies
        (ysrw:add-horizontal-wall-bodies space bodies miny (- miny row)))
      (ysrw:add-overall-horizontal space minx maxx miny (- miny total))
      (setq created (+ created (length bodies) 1))))

  (setq side-data (ysrw:left-wall-data segments ref-segments bbox)
        bodies (if side-data (cdr (assoc 'bodies side-data))))
  (if side-data
    (progn
      (if bodies
        (ysrw:add-vertical-wall-bodies space bodies minx (- minx row)))
      (ysrw:add-overall-vertical space miny maxy minx (- minx total))
      (setq created (+ created (length bodies) 1))))

  (setq side-data (ysrw:right-wall-data segments ref-segments bbox)
        bodies (if side-data (cdr (assoc 'bodies side-data))))
  (if side-data
    (progn
      (if bodies
        (ysrw:add-vertical-wall-bodies space bodies maxx (+ maxx row)))
      (ysrw:add-overall-vertical space miny maxy maxx (+ maxx total))
      (setq created (+ created (length bodies) 1))))

  created)

(defun ysrw:end-undo (doc)
  (if doc
    (vl-catch-all-apply 'vla-EndUndoMark (list doc))))

(defun ysrw:run (/ *error* olderr doc space wall-data bbox segments ref-segments created)
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
  (setq wall-data (ysrw:collect-wall-data))
  (cond
    ((not wall-data)
     (princ "\nNo white LINE/LWPOLYLINE/POLYLINE wall geometry found in the current selection or drawing."))
    (T
     (setq bbox (cdr (assoc 'bbox wall-data))
           segments (cdr (assoc 'segments wall-data))
           ref-segments (cdr (assoc 'ref-segments wall-data))
           created (ysrw:add-side-dims space bbox segments ref-segments))
     (princ
       (strcat
         "\nExterior opening dimensions created around 4 sides. Wall objects used: "
         (itoa (cdr (assoc 'count wall-data)))
         ", white reference walls: "
         (itoa (cdr (assoc 'ref-count wall-data)))
         ", dimensions created: "
         (itoa created)
         ". Offset: "
         (rtos *ysrw-side-dim-offset* 2 0)
         ", overall gap: "
         (rtos *ysrw-total-dim-gap* 2 0)
         "."))))
  (ysrw:end-undo doc)
  (setq *error* olderr)
  (princ))

(defun c:YKJ ()
  (ysrw:run))

(defun c:YROOMW ()
  (ysrw:run))

(princ "\nYS wall dimension loaded. Commands: YKJ, YROOMW.")
(princ)
