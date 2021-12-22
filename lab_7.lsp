(setq tr '(5 
	(3 
		(1 nil nil) 
		(4 nil nil)) 
	(7 
		(6 nil nil) nil)))


(defun root (tree) 
(cond
	((or (null tree) (atom tree)) nil)
	(t (car tree))
))

(defun left (tree) 
(cond
	((or (null tree) (atom tree)) nil)
	(t (cadr tree))
))

(defun right (tree) 
(cond
	((or (null tree) (atom tree)) nil)
	(t (caddr tree))
))
	
(defun find (x tree) 
(cond
	((or (null tree) (null (root tree)) (null x)) nil)
			((= x (root tree)) t)
			((< x (root tree)) (find x (left tree)))
			((> x (root tree)) (find x (right tree)))
))

(defun add (x tree) 
(cond 	
	((or (null tree) (null (root tree))) (list x nil nil))
			((= x (root tree)) tree)
			((< x (root tree)) (list (root tree) (add x (left tree)) (right tree)))
			((> x (root tree)) (list (root tree) (left tree) (add x (right tree))))
))

(defun prevTree (x tree) 
(cond	
	((null tree) nil)
			((<= x (root tree)) (prevTree x (left tree)))
			(t (list (root tree) (left tree) (prevTree x (right tree))))
))

(defun nextTree (x tree) 
(cond
	((null tree) nil)
			((>= x (root tree)) (nextTree x (right tree)))
			(t (list (root tree) (nextTree x (left tree)) (right tree)))
))

(defun join (p q) 
(cond 	
	((null p) q)
	((null q) p)
	(t (list(root p)
			(join (left p) (prevTree (root p) q))
			(join (right p) (nextTree (root p) q)) 
))))

(defun takeRight (tree) 
(cond 
	((null (right tree)) (root tree))
	(t (takeRight (right tree)))
))

(defun del (x tree) 
(cond
	((null tree) nil)
	((= x (root tree)) 
	(cond	
		((null (left tree)) (right tree))
		((null (right tree)) (left tree))
		(t (list	(takeRight (left tree))
								(del (takeRight (left tree)) (left tree))
								(right tree)))))
			((< x (root tree)) (list (root tree) (del x (left tree)) (right tree)))
			((> x (root tree)) (list (root tree) (left tree) (del x (right tree))))

))

