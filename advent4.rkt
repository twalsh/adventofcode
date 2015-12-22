#lang racket

(require openssl/md5)

(define key "bgvyzdsv")

(let loop ((i 0))
  ;(display i)(newline)
  (let ((string (string-join  (list key (number->string i)) "")))
    (let ((hash (md5 (open-input-string string))))
      (if (string=? (substring hash 0 5) "00000")
          (cons i hash)
          (loop (+ i 1))))))
       
  