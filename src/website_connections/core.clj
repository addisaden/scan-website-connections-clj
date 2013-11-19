(ns website-connections.core
  (:use seesaw.core)
  (:gen-class))



(def main-window (frame :title "Website connections"
                        :on-close :exit
                        :minimum-size [640 :by 480]
                        ))

(def links-to-scan (atom '()))
(def links-scanned (atom []))
; { host {urls [contained-urls]} }
(def hosts (atom '{}))

(defn get-host-url [u] {:host (.getHost (java.net.URI. u)) :url u})

(defn create-host-if-not-exist [host]
  (dosync
    (if (not (some #(= % host) (keys @hosts)))
      (swap! hosts conj { host {} })
      )))

(defn create-url [u, contained-links]
  (let [umap (get-host-url u)]
    (dosync
      (create-host-if-not-exist (:host umap))
      (swap! hosts update-in [(:host umap)] conj {u (vec contained-links)})
      )))

(defn link-scanned? [u]
  (find @links-scanned u))

(defn get-links [u]
  (let [website-source (slurp u)]
    (try
      (re-seq #"(?im)https?:\/\/[^\s\"\']+" website-source)
      (catch Exception e (do (println "Error on url:" (.getMessage e))
                             '()
                             )))
    ))

(defn go-test [u]
  (let [l (get-links u)]
    (create-url u l)
    ))

(defn -main
  [& args]
  (-> main-window
      pack!
      show!))
