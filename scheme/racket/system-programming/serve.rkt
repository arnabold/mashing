#lang racket

(define (serve port-no)
  (define listener (tcp-listen port-no 5 #t))
  (define (loop)
    (accept-and-handle listener)
    (loop))
  ;; Server Thread
  (define t (thread loop))
  (lambda ()
    (kill-thread t)
    (tcp-close listener)))

(define (accept-and-handle listener)
  (define-values (in out) (tcp-accept listener))
  ;; Connection Thread
  (define t (thread
             (lambda ()
               (sleep (random 10)) ;; just to show the effect of different threads
               (handle in out)
               (close-input-port in)
               (close-output-port out))))
  ;; Watcher Thread
  ;; A malicious client could connect to our web server and not send
  ;; the HTTP header, in which case a connection thread will idle
  ;; forever, waiting for the end of the header. To avoid this
  ;; possibility, we’d like to implement a timeout for each connection
  ;; thread.
  ;; One way to implement the timeout is to create a second thread
  ;; that waits for 10 seconds, and then kills the thread that calls
  ;; handle. Threads are lightweight enough in Racket that this
  ;; watcher-thread strategy works well:
  (thread (lambda ()
            (sleep 10)
            (kill-thread t))))
  
(define (handle in out)
  ;; Discard the request header (up to blank line):
  (regexp-match #rx"(\r\n|^)\r\n" in)
  ;; Send reply:
  (display "HTTP/1.0 200 Okay\r\n" out)
  (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
  (display "<html><body>Hello, world!</body></html>" out))


