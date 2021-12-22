
(defun getStrsFromFile (fileName) (
prog (fi out)
(filOpen 'fi fileName _INPUT)
(setq out (toFlatList (insertEnters (mapcar (function split) (readToEnd 'fi)))))
(filClose 'fi)
(return out)
))

(defun toFlatList (lst &optional (out nil)) (
cond ((null lst) out)
((atom lst) (cons lst out))
(t 
    (toFlatList (car lst) (toFlatList(cdr lst) out)))
))

(defun insertEnters (lst) 
(cond 
    ((null lst) nil)	
    (t (cons (car lst)(insertEnters(cdr lst))))
    ))

(defun split (str) 
(cond 
    ((= (strind str ".") 0) (cons str nil))
    (t (cons (strdel str (strind str ".") (strlen str))
(split (strdel str 1 (strind str ".")))))
))

(defun readToEnd (fil) 
(cond
    ((fileof fil) nil)
    (t (cons (filGetLine fil) (readToEnd fil)))
))

(defun toNormalText (x) 
(cond 
    ((null x) nil)
    ((stringp (car x)) (cond 
                            ((not(equal x (strchr 0)))(cons (cons (strcat (strUcase (strmid (car x) 1 1))
                            (strmid (car x) 2))(list (strchr 46))) (toNormalText (cdr x)))))) 
    (t (cons (toNormalText (car x)) (toNormalText (cdr x))))))

(toNormalText (getStrsFromFile "C:\Users\mihai\Desktop\w.txt"))

(defun split (str) 
(cond 
    ((= (strind str " ") 0) (cons str nil))
    (t (cons (strdel str (strind str " ") (strlen str)) (split (strdel str 1 (strind str " ")))))
))

(defun counter (list sym)
  (cond 
    ((null list) 0)
    ((eq sym (car list)) (+ 1 (counter (cdr list) sym)))
    (t (counter (cdr list) sym)))) 

(defun deleteAll (list sym)
  (cond 
    ((null list) nil)
    ((eq (car list) sym) (deleteAll (cdr list) sym))
    (t (cons (car list) (deleteAll (cdr list) sym)))
    ))

(defun task (list)
  (cond 
    ((null list) nil)
    (t (cons (list (car list) (counter list (car list))) (task (deleteAll (cdr list) (car list))))))) 

(task (getStrsFromFile "C:\Users\mihai\Desktop\w.txt"))