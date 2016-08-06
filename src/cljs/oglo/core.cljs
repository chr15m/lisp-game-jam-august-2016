(ns oglo.core
    (:require [reagent.core :as reagent :refer [atom]]
              [cljs.core.async :refer [<! put! timeout chan] :as async])
    (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

;; -------------------------
;; Functions

(def m js/Math)
(def tau (* m.PI 2))

(defn g-trans [x y]
  {:transform (str "translate(" x "," y ")")})

(defn render-path [old-path new-code]
  (let [lines (clojure.string/split new-code #"\r?\n")]
    (loop [l lines
           path ""]
      (if (> (count l) 0)
        (let [form (first l)]
          (print "form" (clojure.string/split form " "))
          (recur (rest l) ""))
        path))))

(def default-style {:fill "none" :stroke "#41A4E6" :stroke-width "1px" :stroke-linecap "round"})

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

(defn component-svg-turtle [[x y]]
  [:g {:transform (str "translate(" x "," y ")")}
   [:path {:fill "url(#hatch)" :stroke "#eee" :stroke-width "2" :d (js/roundPathCorners "M 0 0 L -20 0 L 0 -40 L 20 0 Z" 5 false)}]])

(defn component-svg-code-rendered [code]
  [:g
   [:path {:fill "url(#hatch)" :stroke "#eee" :stroke-width "2" :d ""}]])

(defn component-code-input [changes code]
  [:textarea {:id "code-input" :cols 10 :rows 5 :placeholder "f 1" :value @code :on-change #(put! changes [:code (-> % .-target .-value)])}])

(defn component-oglo [changes code rendered-path size]
  (let [[ow oh] (map #(/ % 2) @size)]
    [:div
     [:svg {:width "100%" :height "100%" :style {:top "0px" :left "0px" :position "absolute"}}
      [:defs
        (component-svg-filter-glow)
        (component-svg-pattern-hatch)]
      [:g {:transform (str "translate(" ow "," oh ")") :filter "url(#glowfilter)"}
       [component-svg-turtle [0 0]]
       [component-svg-code-rendered rendered-path]]]
     [component-code-input changes code]]))

;; -------------------------
;; Initialize app

(defn mount-root []
  (let [code (atom "")
        rendered-path (atom "")
        size (atom [(.-innerWidth js/window) (.-innerHeight js/window)])
        changes (chan)]
    (reagent/render [component-oglo changes code rendered-path size] (.getElementById js/document "app"))
    (go-loop []
             (let [[c change] (<! changes)]
               (print "change:" c change)
               (cond
                 (= c :code) (do (swap! rendered-path render-path change) (reset! code change))))
             (recur))))

(defn init! []
  (mount-root))

; off-white
; background-color: #EFF0DA;
; background-color: #D4D5C1;

