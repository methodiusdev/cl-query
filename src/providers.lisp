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
  "Represents a parsed CSV row"
  (fields nil :type list)
  (line-number 0 :type integer))

(defstruct csv-table
  "Represents a CSV table with headers"
  (headers nil :type list)
  (rows nil :type cons)) ; stream of csv-row
