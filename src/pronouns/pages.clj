;; pronoun.is - a website for pronoun usage examples
;; Copyright (C) 2014 - 2018 Morgan Astra

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>

(ns pronouns.pages
  (:require [clojure.string :as s]
            [pronouns.config :refer [pronouns-table]]
            [pronouns.util :as u]
            [hiccup.core :refer :all]
            [hiccup.element :as e]
            [hiccup.util :refer [escape-html]]))

(defn prose-comma-list
  [items]
  (let [c (count items)]
    (cond
      (<= c 1) (or (first items) "")
      (= c 2) (s/join " and " items)
      :else (str (s/join ", " (butlast items)) ", and " (last items)))))

(defn href
  [url text]
  [:a {:href url} text])

;; FIXME morgan.astra <2018-11-14 Wed>
;; use a div for this instead of a plain bold tag
(defn wrap-pronoun
  [pronoun]
  [:b pronoun])

(defn render-sentence [& content]
  [:p [:span.sentence content]])

(defn subject-example
  [subject]
  (render-sentence (wrap-pronoun (s/capitalize subject)) " est allé.e au parc."))

(defn object-example
  [object]
  (render-sentence "Je suis allé.e avec " (wrap-pronoun object) "."))

(defn posessive-determiner-example
  [subject possessive-determiner]
  (render-sentence (wrap-pronoun (s/capitalize subject))
                   " porté "
                   (wrap-pronoun possessive-determiner)
                   " frisbee."))

(defn possessive-pronoun-example
  [possessive-pronoun]
  (render-sentence "Au moins, je pense que c'était le "
                   (wrap-pronoun possessive-pronoun)
                   "."))

(defn reflexive-example
  [subject reflexive]
  (render-sentence (wrap-pronoun (s/capitalize subject))
                   " a lancé.e le frisbee à "
                   (wrap-pronoun reflexive)
                   "."))

(defn header-block [header]
  [:div {:class "section title"}
   (href "/" [:h1 header])])

(defn examples-block
  [subject object possessive-determiner possessive-pronoun reflexive]
  (let [sub-obj (s/join "/" [subject object])
        header-str (str "Ici quelques exemples des phrases qui utilisent le pronom "
                        sub-obj
                        " pronoms:")]
    [:div {:class "section examples"}
     [:h2 header-str]
     [:p (subject-example subject)
         (object-example object)
         (posessive-determiner-example subject possessive-determiner)
         (possessive-pronoun-example possessive-pronoun)
         (reflexive-example subject reflexive)]]))

(defn usage-block []
  [:div {:class "section usage"}
   [:p "Usage complêt: "
    ;; FIXME morgan.astra <2018-11-14 Wed>
    ;; This looks really ugly in the browser
       [:tt "https://prono.ms/sujet/objet/posessifdeterminant/posessif/reflexif montre des exemples de vos pronoms."]
   [:p "Ceci est un peu lourd. Si nous connaissons votre pronom, il est permis d’entrer"
       " seulement la forme principale."]])

(defn contact-block []
  (let [twitter-name (fn [handle] (href (str "https://www.twitter.com/" handle)
                                       (str "@" handle)))]
    [:div {:class "section contact"}
     [:p "Écrit par  "
         (twitter-name "morganastra")
         ", qui usilise les "
         (href "https://prono.ms/elle" "prono.ms/elle")]
     [:p "prono.ms est un logiciel libre sous l’ "
         (href "https://www.gnu.org/licenses/agpl.html" "AGPLv3")
         "! rendez-vous visiter la projet sur "
         (href "https://github.com/witch-house/pronoun.is" "github")]
     [:p "&lt;3"]]))

(defn footer-block []
  [:footer (usage-block) (contact-block)])

(defn format-pronoun-examples
  [pronoun-declensions]
  (let [sub-objs (map #(s/join "/" (take 2 %)) pronoun-declensions)
        title (str "mes prono.ms sont: " (prose-comma-list sub-objs) " exemples")
        examples (map #(apply examples-block %) pronoun-declensions)]
    (html
     [:html
      [:head
       [:title title]
       [:meta {:name "viewport" :content "width=device-width"}]
       [:meta {:charset "utf-8"}]
       [:meta {:name "description" :content (u/strip-markup examples)}]
       [:meta {:name "twitter:card" :content "summary"}]
       [:meta {:name "twitter:title" :content title}]
       [:meta {:name "twitter:description" :content (u/strip-markup examples)}]
       [:link {:rel "stylesheet" :href "/pronouns.css"}]]
      [:body
       (header-block title)
       examples
       (footer-block)]])))

(defn table-lookup* [pronouns-string]
  (let [inputs (s/split pronouns-string #"/")
        n (count inputs)]
    (if (>= n 5)
      (take 5 inputs)
      (u/table-lookup inputs @pronouns-table))))

(defn lookup-pronouns
  "Given a seq of pronoun sets, look up each set in the pronouns table"
  [pronoun-sets]
  (->> pronoun-sets
       (map (comp table-lookup* escape-html))
       (filter some?)))

(defn make-link [path]
  (let [link (str "/" path)
        label path]
    [:li (href link label)]))

(defn front []
  (let [abbreviations (take 6 (u/abbreviate @pronouns-table))
        links (map make-link abbreviations)
        title "mes prono.ms sont:"
        description "prono.ms est un site web pour des exemples d’usage des pronoms personnels. "]
    (html
     [:html
      [:head
       [:title title]
       [:meta {:name "description" :content description}]
       [:meta {:name "twitter:card" :content "summary"}]
       [:meta {:name "twitter:title" :content title}]
       [:meta {:name "twitter:description" :content description}]
       [:meta {:name "viewport" :content "width=device-width"}]
       [:meta {:charset "utf-8"}]
       [:link {:rel "stylesheet" :href "/pronouns.css"}]]
      [:body
       (header-block title)
       [:div {:class "section table"}
        [:p "prono.ms est un site web pour des exemples d’usage des pronoms personnels "]
        [:p "ce site connais ces pronoms:"]
        [:ul links]
        [:p [:small (href "all-pronouns" "see all pronouns in the database")]]]]
      (footer-block)])))

(defn all-pronouns []
  (let [abbreviations (u/abbreviate @pronouns-table)
        links (map make-link abbreviations)
        title "mes prono.ms sont:"]
    (html
     [:html
      [:head
       [:title title]
       [:meta {:name "viewport" :content "width=device-width"}]
       [:meta {:charset "utf-8"}]
       [:link {:rel "stylesheet" :href "/pronouns.css"}]]
      [:body
       (header-block title)
       [:div {:class "section table"}
        [:p "All pronouns the site knows about:"]
        [:ul links]]]
      (footer-block)])))

(defn not-found [path]
  (let [title "Pronoun Island: English Language Examples"
        or-re #"/[oO][rR]/"]
    (html
     [:html
      [:head
       [:title title]
       [:meta {:name "viewport" :content "width=device-width"}]
       [:meta {:charset "utf-8"}]
       [:link {:rel "stylesheet" :href "/pronouns.css"}]]
      [:body
       (header-block title)
       [:div {:class "section examples"}
        [:p [:h2 "Nous n'avons pas pu trouver ces pronoms dans notre 
base de données" :("]
         "Si vous pensez qu'il faut les ajouter, contactez-nous!"]
        (when (re-find or-re path)
          (let [alts (s/split path or-re)
                new-path (str "/" (s/join "/:OR/" alts))]
            [:div
             "Est-ce que vous voulez dire: "
             (href new-path
                   (str "pronoun.is"
                        new-path))]))]
       (footer-block)]])))

(defn pronouns [params]
  (let [path (params :*)
        param-alts (u/vec-coerce (or (params "or") []))
        path-alts (s/split path #"/:[oO][rR]/")
        pronouns (lookup-pronouns (concat path-alts param-alts))]
    (if (seq pronouns)
      (format-pronoun-examples pronouns)
      (not-found path))))
