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

(defn new-host-created [] (do (println "New Host created!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-host-url [u] {:host (.getHost (java.net.URI. u)) :url u})

(defn create-host-if-not-exist [host]
  (dosync
    (if (not (some #(= % host) (keys @hosts)))
      (do
        (swap! hosts conj { host {} })
        (new-host-created)
        ))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(doseq [i (range 10)]
  (.start (Thread. (fn []
                     (loop []
                       (if (empty? @links-to-scan)
                         (Thread/sleep 1)
                         (let [c (atom nil)]
                           (dosync
                             (reset! c (first @links-to-scan))
                             (cond
                               (nil? @c)
                               (reset! c nil)
                               (link-scanned? @c)
                               (reset! c nil)
                               :else
                               (do
                                 (swap! links-scanned conj @c)
                                 (swap! links-to-scan rest)
                                 )))
                           (if (not (nil? @c))
                             (create-url @c (get-links @c))
                             )))
                       (recur)
                       )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn go-test [u]
  (swap! links-to-scan conj u))

(defn -main
  [& args]
  (-> main-window
      pack!
      show!))
