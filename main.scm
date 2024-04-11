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

