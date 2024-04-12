#lang scheme

; get the list of all textfiles in a directory
; (list-text-files-in-directory "C:\\Users\\Documents\\csi2520")
(define (list-text-files-in-directory directory-path)
  (filter (lambda (file)
            (string-suffix? file ".txt"))
          (map path->string
               (directory-list directory-path))))

; read a histogram textfile and returns the values in a list
; (read-hist-file "C:\\Users\\Documents\\csi2520\\q00.jpg.txt")
(define (read-hist-file filename) 
(cdr (call-with-input-file filename
  (lambda (p)
    (let f ((x (read p)))
      (if (eof-object? x) '() (cons x (f (read p)))))))))

;; fonction pour calculer l'intersection entre deux histogram
(define (comparaison-number h1 h2)
  (cond
   ((null? h1) 0)
   ((> (car h1) (car h2))
              (+ (car h2) (comparaison-number (cdr h1) (cdr h2))))
   ((< (car h1) (car h2))
              (+ (car h1) (comparaison-number (cdr h1) (cdr h2))))
   (else (+ (car h1) (comparaison-number (cdr h1) (cdr h2))))
  )  
)

;;fonction pour normaliser un histogram
(define (hist-normaliseur h)
  (let ((sum (apply + h)))
    (map (lambda (x) (/ x sum)) h)
   )
 )


(define (read-all-hist directory-path)
  (let ((files (map (lambda (text) (string-append directory-path text)) (map (lambda (text) (string-append "/" text)) (list-text-files-in-directory directory-path)))))
        (map (lambda (file) (cons file (read-hist-file file))) files)
    )
  )

;; similaritySearch: String String -> ListOfPairs
;; similaritySearch query-hist-filename directory-path
;; Given the filename of a query histogram file and the path to a directory containing
;; other histogram files, this function computes the similarity scores between the query
;; histogram and each histogram in the directory. It returns a list of pairs, where each
;; pair consists of the filename of a histogram file and its corresponding similarity score.
(define (similaritySearch2 query-hist-filename directory-path)
  (let* ((query-hist (hist-normaliseur (read-hist-file query-hist-filename)))
         (scores (map (lambda (image-hist-filename)
                        (cons image-hist-filename
                              (exact->inexact (comparaison-number query-hist (hist-normaliseur (read-hist-file (string-append directory-path "/" image-hist-filename)))))))
                      (list-text-files-in-directory directory-path)))
         (sorted-scores (take (sort scores (lambda (x y) (> (cdr x) (cdr y)))) 5)))
    sorted-scores))

;(display (read-hist-file "queryImages/q00.jpg.txt"))
;(newline)
;(display (list-text-files-in-directory "imageDataset2_15_20"))
;(newline)
(display (similaritySearch2 "queryImages/q00.jpg.txt" "imageDataset2_15_20"))

