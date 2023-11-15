;;;;
;;;; An implementation for Exercise #08 of the Exercism Common Lisp Track.
;;;; CConverts a given Arabic numeral (between 1 and 3000) to the respective Roman numeral.
;;;;

(defun romanize (num)
  "Transform Arabic Numerals in Roman Numerals"
  (labels
    ;;; A tail recursion support fn.
    ;;; The "num-str" must be a list of char objects of numbers. e.g. (#\1 #\2).  
    ;;; The "rom" argument must be an empty list.
    ;;; The inital numeral order must be 1. 
    ;;; The fn returns the Roman Numeral on "rom", as a string. 
    ((romanize-rec (num-str rom order)
       (cond
           ;; When all arabic digits was convert to Roman Numerals, transforms
           ;; the list to string and returns.
           ((null num-str) (coerce rom 'string))
           ;; Calls the recursion... 
           (t (romanize-rec 
                ;; ... removing the first item from "num-str";
                (cdr num-str)
                ;; ... converting and appending the first item from "num-str"
                ;; in the "rom" list; 
                (append rom (convert-digit (car num-str) order)) 
                ;; ... and incrementing the numeral order.
                (1- order)))))
  
     ;;; Given an arabic digit (string) and its numeral order (i.e. units, tens,
     ;;; hundreds or thousands), converts it to respective Roman Numeral.
     ;;; The fn returns a list char objects. 
     (convert-digit (digit-str order)
       (let*
         ;; A template to mapping Arabic Numerals to Roman Numerals structure.
         ;; "#\o" refers to Roman symbols that are multiple of one.
         ;; "#\f" refers to Roman symbols that are multiple of five.
         ;; "#\t" refers to Roman symbols that are multiple of ten.  
         ((roman-template '((#\1 . (#\o))
                            (#\2 . (#\o #\o))
                            (#\3 . (#\o #\o #\o))
                            (#\4 . (#\o #\f))
                            (#\5 . (#\f))
                            (#\6 . (#\f #\o))
                            (#\7 . (#\f #\o #\o))
                            (#\8 . (#\f #\o #\o #\o))
                            (#\9 . (#\o #\t))))
          ;; An alist to mapping Roman Numerals of the order of units. 
          (order-1  '((#\o . #\I)
                      (#\f . #\V)
                      (#\t . #\X)))
          ;; An alist to mapping Roman Numerals of the order of tens.
          (order-2 '((#\o . #\X)
                     (#\f . #\L)
                     (#\t . #\C)))
          ;; An alist to mapping Roman Numerals of the order of hundreds.
          (order-3 '((#\o . #\C)
                     (#\f . #\D)
                     (#\t . #\M)))
          ;; An alist to mapping Roman Numerals of the order of thousands.
          (order-4 '((#\o . #\M)))
          ;; Sets local var to respective numeral order.
          (ord (cond
                 ((= order 1) order-1)
                 ((= order 2) order-2)
                 ((= order 3) order-3)
                 ((= order 4) order-4))))
         ;; Selects the correspondent template and substitutes the char objects
         ;; to Roman Numerals, according the respective order alist.
         (sublis ord (cdr (assoc digit-str roman-template))))))

    (if (<= 1 num 3000)
      (let*
        ;; Converts "num" to a list of char objects.
        ;; e.g. the number 123 converts to (#\1 #\2 #\3).
        ((num-str (coerce (write-to-string num) 'list))
         ;; Sets the initial number order.
         ;; e.g. sets 4 to a number of the order of thousands (>= 1000). 
         (order (length num-str)))            
        (romanize-rec num-str nil order))   
      (format t "The number must be greater than 1 and less tha 3000!"))))


;;;