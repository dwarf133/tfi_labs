
;calculate direct form
(defun calc (lst) 
    (cond   
        ((null lst) nil)
            ((atom lst) lst)
            (t (list (cadr lst) (calc (car lst)) (calc (caddr lst))))
            ))


; count the depth
(defun depth (a &optional (lvl 1))
  (cond 
    ((null a) lvl)
        ((atom (car a)) (depth (cdr a) lvl))
        (t (max (depth (car a) (+ 1 lvl)) (depth (cdr a))))
        ))

;sum on the depth level
(defun depthCount (a lvl)
  (cond 
    ((null a) 0)
        ((> lvl 1) (cond 
                    ((atom (car a)) (depthCount (cdr a) lvl))
                        (t (+ (depthCount (car a) (- lvl 1))(depthCount (cdr a) lvl)))))
        (t (cond 
            ((numberp (car a)) (+ (car a) (depthCount (cdr a) lvl)))
                 (t (depthCount (cdr a) lvl))
                 ))))


; count sum on each level
(defun CountLevel (a &optional (lvl (depth a)))
  (cond
    ((< lvl 1) nil)
        (t (append (CountLevel a (- lvl 1)) (list (list lvl (depthCount a lvl)))
        ))))


; Ensure items is in set
(defun isinset (lst elem) 
(cond
    ((null lst) nil)
                      (t 
                        (cond 
                            ((= (car lst) elem) t) 
                            (t 
                                (isinset (cdr lst) elem))
                                ))))

;do some stuff bobabinations you know
;(intersection obviously (of list and elemet (if you dont understand (somehow))))
(defun bobabinator (biba boba) 
    (cond 
        ((null biba) nil)
                      (t (cond 
                            ((eq (car biba) boba) (cons (car biba) (bobabinator (cdr biba) boba)))
                            (t (bobabinator (cdr biba) boba))))
))

;intersects two sets
(defun intersect (x y)
    (cond
        ((null y)nil)
        (t (cond
            ((eq nil (bobabinator x (car y)))(intersect x (cdr y))) 
        (t
            (cons (car(bobabinator x (car y)))(intersect x (cdr y)))
            )))))

;unify two sets
(defun unify (a b)
    (cond 
    ((null a) b)
    ((null b) a)
    ((isinset b (car a)) (unify (cdr a) b) )
    (t (cons (car a) (unify (cdr a) b)))
    ))

