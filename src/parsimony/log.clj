(ns parsimony.log
  (:require [potemkin :refer [import-vars]]
            [taoensso.timbre]))

(import-vars [taoensso.timbre debug info warn error fatal spy])
