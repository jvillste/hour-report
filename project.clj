(defproject hour-report "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [flow-gl/flow-gl "1.0.0-SNAPSHOT"]
                 [metosin/jsonista "0.2.5"]
                 [clj-time "0.15.2"]
                 [medley "1.3.0"]
                 [dk.ative/docjure "1.14.0"]]
  :repl-options {:init-ns hour-report.core})
