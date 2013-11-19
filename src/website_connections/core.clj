(ns website-connections.core
  (:use seesaw.core)
  (:gen-class))

(def main-window (frame :title "Website connections"
                        :on-close :exit
                        :minimum-size [640 :by 480]
                        ))

(def links-to-scan (atom '()))
(def links-scanned (atom []))
(def hosts (atom '{}))

(defn link-scanned? [u]
  (find @links-scanned u))

(defn get-host-url [u] {:host (.getHost (java.net.URI. u)) :url u})

(defn get-links [u]
  (let [website-source (slurp u)]
    (re-seq #"(?im)https?:\/\/[^\s\"\']+" website-source)
    ))

(defn -main
  [& args]
  (-> main-window
      pack!
      show!))
