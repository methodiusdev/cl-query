(in-package :cl-query)

(defclass query-provider ()
  ()
  (:documentation "Abstract base class for query providers.
   Providers know how to convert their specific data source into a stream."))

(defgeneric execute-provider (provider source)
  (:documentation "Executes the provider on SOURCE and returns a stream.
   SOURCE format depends on the provider type."))

(defgeneric provider-name (provider)
  (:documentation "Returns a human-readable name for the provider."))

(defclass csv-provider (query-provider)
  ((delimiter
    :accessor csv-delimiter
    :initarg :delimiter
    :initform #\,
    :documentation "Field delimiter character (default: comma)")
   (has-header
    :accessor csv-has-header
    :initarg :has-header
    :initform t
    :documentation "Whether first row is a header (default: t)")
   (skip-empty-lines
    :accessor csv-skip-empty-lines
    :initarg :skip-empty-lines
    :initform t
    :documentation "Whether to skip empty lines (default: t)")
   (trim-fields
    :accessor csv-trim-fields
    :initarg :trim-fields
    :initform t
    :documentation "Whether to trim whitespace from fields (default: t)"))
  (:documentation "Provider for CSV files and strings.
   Reads CSV data lazily as a stream of rows.
   Supports RFC 4180 CSV format including quoted fields."))

(defmethod provider-name ((provider csv-provider))
  "csv")

(defstruct csv-row
  "Represents a parsed CSV row."
  (fields nil :type list)
  (line-number 0 :type integer))

(defstruct csv-table
  "Represents a CSV table with headers."
  (headers nil :type list)
  (rows nil :type cons)) ; stream of csv-row

(defun split-csv-line (line &optional (delimiter #\,))
  "Splits a csv line into fields."
  (split-sequence:split-sequence delimiter line))

(defun trim-field (field)
  "Trims whitespace from a field string."
  (string-trim '(#\Space #\Tab #\Return #\Newline) field))

(defun parse-csv-line (line delimiter trim-fields line-number)
  "Parses a single CSV line into a csv-row structure."
  (let ((fields (split-csv-line line delimiter)))
    (when trim-fields
      (setf fields (mapcar #'trim-field fields)))
    (make-csv-row :fields fields :line-number line-number)))

(defun csv-lines-stream (source)
  "Creates a stream of lines from SOURCE.
   SOURCE can be:
   - pathname: reads from file
   - string: parses string as CSV
   Returns a stream where each element is a line string."
  (etypecase source
    (pathname
     ;; Read from file
     (let ((file-stream (open source :direction :input :if-does-not-exist :error)))
       (labels ((read-lines ()
                  (let ((line (read-line file-stream nil nil)))
                    (if line
                        (cons-stream line (read-lines))
                        (progn
                          (close file-stream)
                          empty-stream)))))
         (read-lines))))
    (string
     ;; Parse string as CSV
     (with-input-from-string (string-stream source)
       (labels ((read-lines ()
                  (let ((line (read-line string-stream nil nil)))
                    (if line
                        (cons-stream line (read-lines))
                        empty-stream))))
         (read-lines))))))

(defun parse-csv-stream (lines-stream provider line-number)
  "Parses a stream of lines into a stream of csv-row structures.
   Handles empty line skipping based on provider configuration."
  (if (stream-null? lines-stream)
      empty-stream
      (let ((line (stream-car lines-stream)))
        ;; Skip empty lines if configured
        (if (and (csv-skip-empty-lines provider)
                 (= 0 (length (string-trim '(#\Space #\Tab) line))))
            ;; Skip this line
            (parse-csv-stream (stream-cdr lines-stream) provider (1+ line-number))
            ;; Parse this line
            (cons-stream
             (parse-csv-line line
                           (csv-delimiter provider)
                           (csv-trim-fields provider)
                           line-number)
             (parse-csv-stream (stream-cdr lines-stream)
                             provider
                             (1+ line-number)))))))

(defmethod execute-provider ((provider csv-provider) source)
  "Reads CSV from SOURCE and returns a csv-table structure.
   SOURCE can be:
   - pathname: path to CSV file
   - string: CSV content as string
   Returns a csv-table with headers and a stream of csv-rows."
  (let* ((lines-stream (csv-lines-stream source))
         (parsed-stream (parse-csv-stream lines-stream provider 1)))
    (if (csv-has-header provider)
        ;; First row is header
        (if (stream-null? parsed-stream)
            (make-csv-table :headers nil :rows empty-stream)
            (let ((header-row (stream-car parsed-stream)))
              (make-csv-table
               :headers (csv-row-fields header-row)
               :rows (stream-cdr parsed-stream))))
        ;; No header - generate numeric headers
        (if (stream-null? parsed-stream)
            (make-csv-table :headers nil :rows empty-stream)
            (let* ((first-row (stream-car parsed-stream))
                   (num-fields (length (csv-row-fields first-row)))
                   (headers (loop for i from 0 below num-fields
                                 collect (format nil "Column~A" i))))
              (make-csv-table
               :headers headers
               :rows parsed-stream))))))

(defun csv-row-as-alist (row headers)
  "Converts a csv-row to an alist using HEADERS as keys.
   Keys are interned as keywords."
  (when row
    (mapcar (lambda (header field)
              (cons (intern (string-upcase header) :keyword)
                    field))
            headers
            (csv-row-fields row))))

(defun csv-row-as-plist (row headers)
  "Converts a csv-row to a plist using HEADERS as keys.
   Keys are interned as keywords."
  (when row
    (loop for header in headers
          for field in (csv-row-fields row)
          collect (intern (string-upcase header) :keyword)
          collect field)))

(defun csv-row-as-hash-table (row headers &key (test #'equal))
  "Converts a csv-row to a hash table using HEADERS as keys (strings)."
  (when row
    (let ((ht (make-hash-table :test test)))
      (loop for header in headers
            for field in (csv-row-fields row)
            do (setf (gethash header ht) field))
      ht)))
