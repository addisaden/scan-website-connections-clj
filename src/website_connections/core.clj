(ns website-connections.core
  (:use seesaw.core)
  (:use seesaw.font)
  (:require clojure.string)
  (:gen-class))

(native!)

(def message-to-user (label ""))

(def label-count-links (label "Counted Links: "))

(def label-hosts-in-links (listbox :model '()))

(def label-unscanned-links (label "Counted unscanned links: "))

(def scan-unscanned-button (button :text "scan unscanned links"))

(config! message-to-user
         :background "#f80"
         :foreground "#333"
         :font (font :name :monospaced :size 18))

(config! [label-count-links label-hosts-in-links label-unscanned-links]
         :font (font :name :monospaced :size 18)
         :background "#333"
         :foreground "#f80")

(def listbox-hosts (listbox :model '("testhost")))

(def add-link-button (button :text "add link"))


(def host-panel (vertical-panel
                  :items [add-link-button (scrollable listbox-hosts)]
                  ))

(def stat-panel (vertical-panel
                  :items [message-to-user
                          label-count-links
                          label-unscanned-links
                          scan-unscanned-button
                          (scrollable label-hosts-in-links)]
                  :background "#333"
                  :foreground "#f80"
                  :font (font :name :monospaced :style #{:bold} :size 18)
                  ))

(def main-window (frame :title "Website connections"
                        :on-close :exit
                        :minimum-size [640 :by 480]
                        :content (left-right-split
                                   host-panel
                                   stat-panel
                                   :divider-location 1/4)
                        ))

(def links-to-scan (atom '()))
(def links-scanned (atom []))
; { host {urls [contained-urls]} }
(def hosts (atom '{}))

(defn new-host-created [] (do (config! listbox-hosts :model (keys @hosts))))

(defn links-of-host-element [helem]
  (let [result (atom '())]
    (do
      (swap! result #(apply (partial conj %) (keys helem)))
      (doseq [i (keys helem)]
        (if (not (empty? (get helem i)))
          (swap! result #(apply (partial conj %) (get helem i)))))
      (set @result))))

(defn hosts-of-host-element [helem]
  (let [all-links (links-of-host-element helem)
        hmap (map #(.getHost (java.net.URI. %)) all-links)]
    (vec (set hmap))
    ))

(defn unscanned-urls-of-host-element [helem]
  (filter #(some (fn [i] (not (= i %))) @links-scanned) (links-of-host-element helem)))

; label-count-links
; label-hosts-in-links
; label-unscanned-links
(defn create-stat-for-selected-host []
  (let [s (selection listbox-hosts)]
    (cond
      (nil? s)
      (config! message-to-user :text "  Bitte einen Host auswählen  ")
      (not (some #(= % (str s)) (keys @hosts)))
      (config! message-to-user :text "  Host existiert nicht.  ")
      :else
      (do
        (config! message-to-user :text (format "  %s  " (.toUpperCase (str s))))
        (let [helem (@hosts (str s))]
          (println helem)
          (if (empty? helem)
            (config! message-to-user :text "Host is empty!")
            (let [
                  all-l (count (links-of-host-element helem))
                  all-un-l (count (unscanned-urls-of-host-element helem))
                  s-hosts (hosts-of-host-element helem)
                  s-hosts (if (empty? s-hosts) [] s-hosts)]
              (config! label-count-links :text (format "Counted Links: %s" all-l))
              (config! label-unscanned-links :text (format "Counted unscanned links: %s" all-un-l))
              (config! label-hosts-in-links :model s-hosts)
              )))))))

;add a link
(listen add-link-button :action (fn [e] (do
                                          (swap! links-to-scan conj (input "insert an url to scan:"))
                                          (create-stat-for-selected-host)
                                          )))

(listen listbox-hosts :selection (fn [e] (create-stat-for-selected-host)))

; scan-unscanned-button

(listen scan-unscanned-button :action (fn [e] (let [s (selection listbox-hosts)]
                                                (cond
                                                  (nil? s)
                                                  (config! message-to-user :text "  Bitte einen Host auswählen  ")
                                                  (not (some #(= % (str s)) (keys @hosts)))
                                                  (config! message-to-user :text "  Host existiert nicht.  ")
                                                  (not (empty? (unscanned-urls-of-host-element (@hosts (str s)))))
                                                  (swap! links-to-scan
                                                         #(apply
                                                            (partial conj %)
                                                            (unscanned-urls-of-host-element (@hosts (str s)))
                                                            ))))))

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
      (swap! hosts update-in [(:host umap) u] set)
      (swap! hosts update-in [(:host umap) u] vec)
      )))

(defn link-scanned? [u]
  (find @links-scanned u))

(defn get-links [u]
  (try
    (re-seq #"(?im)https?:\/\/[a-z0-9\.\/]+" (slurp u))
    (catch Exception e (do (config! message-to-user :text (format "  error on %s  " u))
                           (swap! links-scanned conj u)
                           '()
                           ))))

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
                                 (config! message-to-user :text (format "  scanne %s  " @c))
                                 )))
                           (if (not (nil? @c))
                             (try
                               (create-url @c (get-links @c))
                               (catch Exception e (do
                                                    (create-url @c '())
                                                    (config! message-to-user :text (format"Error on %s" @c))))
                               ))))
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
