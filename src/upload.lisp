(in-package #:lolisp)

(defconstant png-header #(137 80 78 71 13 10 26 10))
(defvar hunchenroot (merge-pathnames "flags/" (toot:config-item :document-root)))

(toot:handle (form :post "/upload" toot:@plain)
    ()
  (let ((thing (do-upload (tbnl:post-parameter "flag"))))
    (typecase thing
      (string thing)
      (t "Upload Success"))))

(defun do-upload (file)
  (let ((tmp-name (first file))
        (mime-type (third file)))
    (cond
      ((not (string= "image/png" mime-type))
       "Image should be a PNG")
      ((not (probe-file tmp-name))
       "What the fuck?")
      ((< 15360 (image-size tmp-name))
       "Image too large. Max size is 15KB")
      ((not (image-dimensions-p tmp-name))
       "Wrong image dimensions.")
      ((not (valid-header-p tmp-name))
       "Invalid PNG header")
      (t (upload-file tmp-name (second file))))))

(defun upload-file (tmp-name real-name)
  (real-move-file tmp-name (merge-pathnames real-name hunchenroot))
  t)

(defun image-size (img)
  (osicat-posix:stat-size (osicat-posix:stat img)))

(defun image-dimensions-p (img)
  "This is silly"
  (string= "1611" (inferior-shell:run/ss (str "/usr/bin/identify -format \"%w%h\" " img))))

(defun valid-header-p (img)
  (with-open-file (s img :element-type '(unsigned-byte 8))
    (loop for header-byte across png-header
          for file-byte = (read-byte s)
          always (= header-byte file-byte))))

(hunchentoot:start (make-instance 'toot:acceptor
                                  :port 4244
                                  :access-log-destination *standard-output*
                                  :message-log-destination *error-output*))
