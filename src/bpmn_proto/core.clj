(ns bpmn-proto.core
  (:require [clojure.xml :as xml]
    [clojure.data.zip.xml :as zip-xml]))

(defn get-node-content [node] (get node :content))

(defn filter-xml-tree [node, criteria]
  (let [content (get-node-content node)
        service-tasks (filter criteria content) 
        non-service-tasks (filter (fn [e] (not (criteria e))) content)]
      (concat service-tasks (mapcat (fn [e] (filter-xml-tree e criteria)) non-service-tasks))))

(defn is-service-task [node] 
  (= (get node :tag) :bpmn:serviceTask))

(defn is-external [node] 
  (= (get (get node :attrs) :camunda:type) "external"))

(defn is-external-service-task [node]
  (and (is-service-task node) (is-external node)))

(defn filter-external-service-tasks [node]
  (filter-xml-tree node is-external-service-task))

(defn filter-external-tasks [nodes] (filter is-external nodes))

(defn is-input-parameter [node]
  (= (get node :tag) :camunda:inputParameter))

(defn bpmn-to-proto-param [field-number, input-param]
  (let [name (get (get input-param :attrs) :name)]
    (apply str "\tstring " name " = " (+ 1 field-number) ";\n")))

(defn task-to-message [task]
  (let [topic (get (get task :attrs) :camunda:topic)
        header (str "\nmessage " topic " {\n")
        input-params (apply str (map-indexed bpmn-to-proto-param (filter-xml-tree task is-input-parameter)))
        tail "}\n"] 
    (apply str header input-params tail)))

(defn parse-bpmn [filename] (xml/parse filename))
  
(defn -main [& args]
  (let [parsed-bpmn (parse-bpmn (first args))
        external-service-tasks (filter-external-service-tasks parsed-bpmn)
        package "hardcoded"
        header (apply str "syntax = \"proto3\";\npackage " package ";\noption go_package = \"ninjhah.co.za/bpmn\";")]
    (spit (first (rest args)) (apply str header (map task-to-message external-service-tasks)))))
  