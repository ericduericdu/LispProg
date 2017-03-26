;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Your solution goes in this file ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cycle (a b)
    (if(consp b) ;if list has content
            (dotimes(y a)  ;cycle it a times
                    (setq b (append (cdr b)(cons (car b) nil)))
            )
    )
    b
)

(defun split-list(a b)
    (setq big nil)
    (setq temp '(blank))
    (setq prev (car b))
    (setq hasList nil)
    
    (dolist (i b) ;traverse through list, i holds the current atom
        (cond 
            ((eql a i) ;if current is delimiter
                (cond
                    ((not(eql i prev))                              ;if list has something to append
                        (setq big (append big (list (cdr temp))))   ;append content
                        (setq temp '(blank))        
                        (setq hasList t)
                    )
                )
            )
            
            ((not(eql a i)) ;if current is not delimiter
                (setq temp (append temp (cons i nil)))  ;put into the list
            )
        )
        (setq prev i) ;keep track of previous one
    )   
    
    (if(not(null (cdr temp))) ;if there is something to append at the end of the prcedure
        (setq big (append big (list (cdr temp))))
    )
    
    (if(and (null big) (not(null hasList)));;if delimiter as not in list, just put b
            (setq big (list big b))
    )
    
    big ;;return big without holder
)

(defun range(a)
    (setq smallest (car a)) ;smallest is the first one, unless proven otherwise
    (setq largest (car a))  ;largest is the first one, unless proven otherwise
    (setq total 0)

    (cond 
        ((not(null a))
    
            (dolist(i a)
                (setq total (+ total i)) ;accumulate the total
                
                (cond
                    ((> i largest)          ;find a larger value
                        (setq largest i)
                    )
                    ((< i smallest)         ;find a smaller value
                        (setq smallest i)
                    )
                )
            )
            
            (setq avg ( / total (length a)))        ;calculate average
            
            (list smallest avg largest)
        )
        ((null a) nil) ;if nothing
    )
)

(defun search-count(a b)
    (setq count 0)
    
    (dolist(i b)
        (if(eql i a) (setq count (+ count 1)))  ;add to count if equal
    )
    count
)

(defun pivot(a b)
    (setq over nil)
    (setq under nil)
    
    (dolist(i b)
        (if(>= i a)
            (setq over (append over (cons i nil)))  ;add to over is >=
          (setq under (append under (cons i nil)))  ;else add to under
        )
    )
    (list under over)
)

(defun quicksort(a)
 
    (cond
        ((not(null a))
            
            (append (quicksort (car (pivot (car a) (cdr a))))
                    (cons (car a) nil)
                    (quicksort (car (cdr (pivot (car a) (cdr a)))))
            )
        )
    )
)

(defun match(a b)
    (cond
        ((or (null a)(null b)) 
            (equal a b)
        )
        ((eql (car a) '?)
            (match (cdr a) (cdr b))
        )
        ((and (eql (car a) '!) (match (cdr a) (cdr b)))
            t
        )
        ((eql (car a) '!)
            (match a (cdr b))
        )
        (t
            (and (eql (car a) (car b)) (match (cdr a) (cdr b)))))
        )

(defun matrix-add(a b)
    (setq summat nil)
    
    (do((x a(cdr x)) (y b(cdr y))) ((null x))
        (setq summat (append summat (list (mapcar #'+ (car x)(car y)))))    ;traverse through each row and add together, then append to overall list
    )     
    summat
)

(defun matrix-multiply(a b)
    (setq temp nil)
    (setq prod nil)

    (dolist(i a)
        (dolist(j (matrix-transpose b))
            
            (Setq count 0)
            
            (do((x i(cdr x)) (y j(cdr y))) ((null x))
                (setq count (+ count (* (car x)(car y))))   ;sum of product of row
            )
            (setq temp (append temp (list count)))          ;holds that particular row
        )
        (setq prod (append prod (list temp)))               ;puts row into overall matrix
    )
    prod
)

(defun matrix-transpose(a)
    (setq trans nil)
    (setq temp nil)
    
    (dotimes(y (length(car a)))                 ;initialize transposed matrix to null
        (setq trans (append trans (list nil))))
                    
    (mapcar #'(lambda (b)
                        (do((x trans(cdr x)) (y b(cdr y)))((null y))
                            (setq temp (append temp (list (append (car x) (list (car y)))))) ;steps through and add to new rows
                        )
                        (setq trans temp)   ;puts it into transposed matrix
                        (setq temp nil)
                        ) a)
    trans
)