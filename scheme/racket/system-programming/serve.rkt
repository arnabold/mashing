#lang racket

(require xml ;; xexpt->string, to produce HTML
         net/url ;; string->url, url-path, path/param-path and url-query
         )

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
    (regexp-match #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+"
                  (read-line in)))
  (when req
    ;; Discard the request header (up to blank line):
    (regexp-match #rx"(\r\n|^)\r\n" in)
    ;; Dispatch:
    (let ([xexpr (dispatch (list-ref req 1))])
      ;; Send reply:
      (display "HTTP/1.0 200 Okay\r\n" out)
      (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
      (display (xexpr->string xexpr) out))))

;; dispatch:
;; takes a requested URL and produces a result value suitable to use
;; with xexpr->string to send back to the client
(define (dispatch str-path)
  ;; parse the request as a URL:
  (define url (string->url str-path))
  ;; Extract the path part:
  (define path (map path/param-path (url-path url)))
  ;; Find a handler based on the path's first element:
  (define h (hash-ref dispatch-table (car path) #f))
  (if h
      ;; Call a handler:
      (h (url-query url))
      ;; No handler found:
      `(html (head (title "Error"))
             (body
              (font
               ((color "red"))
               "Unknown page: "
               ,str-path)))))

(define dispatch-table (make-hash))

;; A simple dispatcher:
(hash-set! dispatch-table "hello"
           (lambda (query)
             `(html (body "Hello, World!"))))

(module+ test
  (require rackunit)

  (check-equal?
   (xexpr->string '(html (head (title "Hello")) (body "Hi!")))
   "<html><head><title>Hello</title></head><body>Hi!</body></html>")

  (define u (string->url "http://localhost:8080/foo/bar?x=bye"))
  
  ;; (url-path u) ;; => (#<path/param> #<path/param>)
  
  (check-equal? (map path/param-path (url-path u))
                '("foo" "bar"))
  (check-equal? (url-query u) '((x . "bye")))

  (check-equal? (regexp-match #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+"
                              "GET /foo/bar?x=bye HTTP/1.1")
                '("GET /foo/bar?x=bye HTTP/1.1" "/foo/bar?x=bye"))
  
  (check-equal? (regexp-match #rx"(\r\n|^)\r\n" "Host: localhost:8080\r\nConnection: keep-alive\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\nUser-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Ubuntu Chromium/28.0.1500.71 Chrome/28.0.1500.71 Safari/537.36\r\nAccept-Encoding: gzip,deflate,sdch\r\nAccept-Language: en-US,en;q=0.8\r\n\r\n")
                '("\r\n\r\n" "\r\n"))
  (check-equal?
   (list-ref (regexp-match #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+"
                           "GET /foo/bar?x=bye HTTP/1.1") 1)
   "/foo/bar?x=bye")

  (define str-path "/foo/bar?x=bye")
  
  (check-equal?
   (map path/param-path (url-path (string->url str-path)))
   '("foo" "bar"))

  (check-equal?
   (xexpr->string `(html (head (title "Error"))
                         (body
                          (font ((color "red"))
                                "Unknown page: "
                                ,str-path))))
   "<html><head><title>Error</title></head><body><font color=\"red\">Unknown page: /foo/bar?x=bye</font></body></html>"
   )
  
  (check-equal?
   (dispatch str-path)
   '(html (head (title "Error")) (body (font ((color "red")) "Unknown page: " "/foo/bar?x=bye"))))
  
  (check-equal?
   (xexpr->string (dispatch str-path))
   "<html><head><title>Error</title></head><body><font color=\"red\">Unknown page: /foo/bar?x=bye</font></body></html>")

  (check-equal?
   ((hash-ref dispatch-table "hello" #f) (url-query u))
   '(html (body "Hello, World!"))) 
  
  )

