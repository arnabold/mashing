#lang racket

(require xml net/url)

(define (serve port-no)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    ;; Listener
    (define listener (tcp-listen port-no 5 #t))
    (define (loop)
      (accept-and-handle listener)
      (loop))
    ;; Main Server thread
    (thread loop))
  (lambda () (custodian-shutdown-all main-cust)))

(define (accept-and-handle listener)
  (define cust (make-custodian))
  ;; limit memory for execution of cust
  (custodian-limit-memory cust (* 50 1024 1024)) ;; 50 MB
  (parameterize ([current-custodian cust]) 
    (define-values (in out) (tcp-accept listener))  
    ;; Connection Thread
    (thread (lambda () (handle in out) (close-input-port in) (close-output-port out))))
  ;; Watcher Thread
  (thread (lambda () (sleep 10) (custodian-shutdown-all cust))))

(define (handle in out)
  (define req
    (regexp-match #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+" (read-line in)))
  
  (when req
    (regexp-match #rx"(\r\n|^)\r\n" in)
    ;; Dispatch:
    (let ([xexpr (dispatch (list-ref req 1))]) 
      ;; Send reply:
      (display "HTTP/1.0 200 Okay\r\n" out)
      (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
      (display (xexpr->string xexpr) out))))

(define (dispatch str-path)
  (define url (string->url str-path))
  (define path (map path/param-path (url-path url)))
  (define h (hash-ref dispatch-table (car path) #f))
  (if h
      (h (url-query url))
      `(html (head (title "Error"))
             (body (font ((color "red")) "Unknown page: " ,str-path)))))

(define dispatch-table (make-hash))

(hash-set! dispatch-table "hello"
           (lambda (query) `(html (body "Hello, World!"))))

(define (build-request-page label next-url hidden)
  `(html
    (head (title "Enter a Number to Add"))
    (body ([bgcolor "white"])
          (form ([action ,next-url] [method "get"])
                ,label
                (input ([type "text"] [name "number"] [value ""]))
                (input ([type "hidden"] [name "hidden"] [value ,hidden]))
                (input ([type "submit"] [name "enter"] [value "Enter"]))))))

;; With this "many" servlet a malicious client could request so many
;; “hello”s that the server runs out of memory.
;; The solution to this class of problems is to limit the memory use
;; of a connection. 
(define (many query)
  (build-request-page "Number of greetings:" "/reply" ""))
(hash-set! dispatch-table "many" many)

(define (reply query)
  (define n (string->number (cdr (assq 'number query))))
  `(html (body ,@(for/list ([i (in-range n)]) " hello"))))
(hash-set! dispatch-table "reply" reply) 

(module+ test
  
  (require rackunit)
  
  (define url (string->url "http://sky@www:801/cgi-bin/finger;xyz?name=shriram;host=nw#top"))
  (check-equal? (url-scheme url) "http")
  (check-equal? (url-user url) "sky")
  (check-equal? (url-host url) "www")
  (check-equal? (url-port url) 801)
  (check-equal? (path/param-path (first (url-path url))) "cgi-bin")
  (check-equal? (path/param-param (first (url-path url))) '())
  (check-equal? (path/param-path (second (url-path url))) "finger")
  (check-equal? (path/param-param (second (url-path url))) '("xyz"))   
  (check-equal? (url-query url) '((name . "shriram") (host . "nw")))
  (check-equal? (url-fragment url) "top")
) 
