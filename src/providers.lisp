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
