(ns oglo.core
    (:require [reagent.core :as reagent :refer [atom]]
              [cljs.core.async :refer [<! put! timeout chan] :as async])
    (:require-macros [cljs.core.async.macros :refer [go go-loop]]
                     [oglo.util :refer [pre-markdown]]))

;; -------------------------
;; Functions

(def m js/Math)
(def tau (* m.PI 2))
(def step 10)

(defn turn-fn [direction {:keys [angle] :as state} args]
  (let [a (or (get args 0) 1)]
    [(assoc state :angle (direction (state :angle) (* (/ m.PI 2) a))) nil]))

(defn vis-fn [what show state args]
  [(assoc state what show) nil])

(def fns
  {"f" (fn [{:keys [pos angle pen] :as state} args]
         (let [d (or (get args 0) 1)
               xn (m.round (+ (* (m.sin angle) d step) (get pos 0)))
               yn (m.round (+ (* (m.cos angle) d step) (get pos 1)))]
         [(assoc state :pos [xn yn]) [(if pen "L" "M") xn yn]]))
   "r" (partial turn-fn +)
   "l" (partial turn-fn -)
   "u" (partial vis-fn :pen false)
   "d" (partial vis-fn :pen true)
   "h" (partial vis-fn :turtle false)
   "s" (partial vis-fn :turtle true)
   nil (fn [state args] [state nil])})

(defn path-render [old-state code]
  (let [lines (clojure.string/split code #"\r?\n")]
    (loop [l lines
           path [["M" 0 0]]
           vm (old-state :vm)]
      ;(print "state:" state)
      (if (> (count l) 0)
        (let [form (clojure.string/split (first l) #"\ ")
              form-fn (get fns (first form))
              [new-vm-state new-path] (if form-fn (apply form-fn vm (rest form)) [vm nil])]
          ;(print "ran" form)
          (recur
            (rest l)
            (if new-path (conj path new-path) path)
            new-vm-state))
        (assoc old-state :path path :vm vm :code code)))))

(def default-style {:fill "none" :stroke "#41A4E6" :stroke-width "1px" :stroke-linecap "round"})

(def colors {:red "#41A4E6"
             :orange "#E6A441"
             :yellow "#41A4E6"
             :blue "#41A4E6"
             :green "#41A4E6"
             :purple "#41A4E6"})

;; -------------------------
;; Components

(defn component-svg-filter-glow []
  [:filter {:id "glowfilter" :x "-50%" :y "-50%" :width "200%" :height "200%"
            :dangerouslySetInnerHTML
            {:__html "<feGaussianBlur in='SourceGraphic' stdDeviation='5'/>
                      <feMerge>
                        <feMergeNode/><feMergeNode in='SourceGraphic'/>
                      </feMerge>"}}])

(defn component-svg-pattern-hatch []
  [:pattern {:id "hatch" :width 3 :height 3 :patternTransform "rotate(45 0 0)" :patternUnits "userSpaceOnUse"}
   [:line {:x1 0 :y1 0 :x2 0 :y2 3 :style {:stroke "#eee" :stroke-width 2}}]])

(defn component-svg-turtle [vm]
  (when (vm :turtle)
    [:g (if (> (count vm) 0) {:transform (str "translate(" (get (vm :pos) 0) " " (get (vm :pos) 1) ") rotate(" (* -1 (vm :angle) (/ 180 m.PI)) ")")})
     [:path {:fill "url(#hatch)" :stroke "#eee" :stroke-width "2" :d (js/roundPathCorners "M 0 0 L -20 0 L 0 40 L 20 0 Z" 5 false)}]]))

(defn component-svg-code-rendered [path]
  (let [path-rendered (clojure.string/join " " (map #(clojure.string/join " " %) path))]
    [:g
     [:path {:fill "none" :stroke "#eee" :stroke-width "2" :d path-rendered}]]))

(defn component-code-input [changes code]
  [:textarea {:id "code-input" :cols 10 :rows 5 :placeholder "f 1" :value code :on-change #(put! changes [:code (-> % .-target .-value)])}])

(defn component-svg-start-help [help oh ow]
  (if help
    [:text {:x 0 :y 80 :text-anchor "middle" :font-family "snap.seregular" :font-size "3em" :fill "#eee" :stroke "#eee" :stroke-width 2 :stroke-linejoin "round"} "oglo"]))

(defn component-start-help [help]
  (if help
    [:div#start-help "<- type code here"]))

(defn component-help []
  (let [v (atom false)]
    (fn []
      [:div
       (when @v [:div#help-text {:dangerouslySetInnerHTML  {:__html (pre-markdown "Help.md")}}])
       [:a {:id "help" :on-click #(swap! v not)} "?"]])))

(defn component-oglo [state changes]
  (let [{:keys [code path vm size]} @state
        [ow oh] (map #(/ % 2) size)
        help (or (not code) (= code ""))]
    [:div
     [:svg {:width "100%" :height "100%" :style {:top "0px" :left "0px" :position "absolute"}}
      [:defs
       (component-svg-filter-glow)
       (component-svg-pattern-hatch)]
      [:g {:transform (str "translate(" ow "," oh ")") :filter "url(#glowfilter)"}
       ;[:path {:fill "none" :stroke "#eee" :d "M 100 100 L 101 101"}]
       [component-svg-start-help help oh ow]
       [:g {:transform "scale(1,-1)"}
        [component-svg-turtle vm]
        [component-svg-code-rendered path]]]]
     [component-help]
     [component-code-input changes code]
     [component-start-help help]]))

;; -------------------------
;; Initialize app

(defn mount-root []
  (let [blank-vm {:pos [0 0] :angle 0 :pen true :turtle true}
        app-state (atom {:code ""
                         :path []
                         :vm blank-vm
                         :size [(.-innerWidth js/window) (.-innerHeight js/window)]})
        changes (chan)]
    (reagent/render [component-oglo app-state changes] (.getElementById js/document "app"))
    (.focus (.getElementById js/document "code-input"))
    (go-loop []
             (let [[c change] (<! changes)]
               ;(print "change:" c change)
               (case c
                 :code (let [updated-state (path-render (assoc @app-state :vm blank-vm) change)]
                         (swap! app-state assoc
                                :code change
                                :path (updated-state :path)
                                :vm (updated-state :vm)
                                :size [(.-innerWidth js/window) (.-innerHeight js/window)]))))
             (recur))))

(defn init! []
  (mount-root))

; off-white
; background-color: #EFF0DA;
; background-color: #D4D5C1;

