#lang racket

(require xml 
         net/url)

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
  (lambda ()
    (custodian-shutdown-all main-cust)))

(define (accept-and-handle listener)
  (define cust (make-custodian)) 
  (parameterize ([current-custodian cust]) 
    (define-values (in out) (tcp-accept listener))  
    ;; Connection Thread
    (thread (lambda ()
              (handle in out)
              (close-input-port in)
              (close-output-port out))))
  ;; Watcher Thread
  (thread (lambda ()
            (sleep 10)
            (custodian-shutdown-all cust))))

(define (handle in out)
  (define req
    ;; Match the first line to extract the request:
    ;; First line should be like GET /ciao/miao?a=b&c=d HTTP/1.1
    ;; and the result of regexp-match
    ;; (GET /ciao/miao?a=b&c=d HTTP/1.1 /ciao/miao?a=b&c=d)
    (regexp-match #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+"
                  (read-line in)))
  (when req
    ;; Discard the other request header lines (up to blank line):
    (regexp-match #rx"(\r\n|^)\r\n" in)
    ;; Dispatch:
    (let ([xexpr (dispatch (list-ref req 1))]) ;; i.e. (dispatch "/ciao/miao?a=b&c=d HTTP/1.1")
      ;; Send reply:
      (display "HTTP/1.0 200 Okay\r\n" out)
      (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
      (display (xexpr->string xexpr) out))))

(define (dispatch str-path)
  (define url (string->url str-path))
  (define path (map path/param-path (url-path url)))
  (define h (hash-ref dispatch-table (car path) #f))
  (if h
      ;; Call a handler:
      (h (url-query url))
      ;; No handler found
      `(html (head (title "Error"))
             (body
              (font
               ((color "red"))
               "Unknown page: "
               ,str-path)))))

(define dispatch-table (make-hash))

(hash-set! dispatch-table "hello"
           (lambda (query)
             `(html (body "Hello, World!"))))

;; build-request-page
;; constructs and HTML form
;; label is a string to show the user
;; next-url is a destination for the form result
;; hidden is a value to propagate through the form as a hidden field
(define (build-request-page label next-url hidden)
  `(html
    (head (title "Enter a Number to Add"))
    (body ([bgcolor "white"])
          (form ([action ,next-url] [method "get"])
                ,label
                (input ([type "text"] [name "number"] [value ""]))
                (input ([type "hidden"] [name "hidden"] [value ,hidden]))
                (input ([type "submit"] [name "enter"] [value "Enter"]))))))

(define (many query)
  (build-request-page "Number of greetings:" "/reply" ""))

(define (reply query)
  ;;(display query)
  ;;(flush-output)
  (define n (string->number (cdr (assq 'number query))))
  `(html (body ,@(for/list ([i (in-range n)])
                   " hello"))))

(hash-set! dispatch-table "many" many) ;; i.e.: http://localhost:8080/many
(hash-set! dispatch-table "reply" reply) ;; i.e.: http://localhost:8080/reply?number=5&hidden=&enter=Enter

(module+ test
  
  (require rackunit)
  
  (check-equal?
   (build-request-page "Number of greetings:" "/reply" "")
   '(html
     (head (title "Enter a Number to Add"))
     (body ((bgcolor "white"))
           (form ((action "/reply") (method "get"))
                 "Number of greetings:"
                 (input ((type "text") (name "number") (value "")))
                 (input ((type "hidden") (name "hidden") (value "")))
                 (input ((type "submit") (name "enter") (value "Enter")))))))

  (check-equal?
   (many 'an-unused-query)
   '(html
     (head (title "Enter a Number to Add"))
     (body ((bgcolor "white"))
           (form ((action "/reply") (method "get"))
                 "Number of greetings:"
                 (input ((type "text") (name "number") (value "")))
                 (input ((type "hidden") (name "hidden") (value "")))
                 (input ((type "submit") (name "enter") (value "Enter")))))))

  (check-equal? (assq 3 (list (list 1 2) (list 3 4) (list 5 6)))
                '(3 4))
  
  (check-equal? (assq 'number '((number . "5") (hidden . nil) (enter . "Enter")))
                '(number . "5"))
  
  (check-equal? (cdr (assq 'number '((number . "5") (hidden . nil) (enter . "Enter")))) 
                "5")
  
  (check-equal? (string->number (cdr (assq 'number '((number . "5") (hidden . nil) (enter . "Enter"))))) 
                5)
  
  (check-equal?
   (for/list ([i (in-range 5)])
     "hello")
   '("hello" "hello" "hello" "hello" "hello"))

  (check-equal?
   `(html (body ,@(for/list ([i (in-range 5)])
                    " hello")))
   '(html (body " hello" " hello" " hello" " hello" " hello")))
  
  (check-equal?
   (reply '((number . "5") (hidden . nil) (enter . "Enter")))
   '(html (body " hello" " hello" " hello" " hello" " hello")))) 
