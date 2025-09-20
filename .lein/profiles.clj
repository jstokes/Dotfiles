{:user
 {:injections
  [(require 'hashp.core)
   (require 'pjstadig.humane-test-output)
   (let [pct-var (ns-resolve (doto 'clojure.stacktrace require) 'print-cause-trace)
         pst-var (ns-resolve (doto 'clj-stacktrace.repl require) 'pst+)]
     (alter-var-root pct-var (constantly (deref pst-var))))
   (require 'pjstadig.humane-test-output)
   (pjstadig.humane-test-output/activate!)]

  :local-repo #=(eval (System/getenv "LEIN_M2"))

  :jvm-opts ["-Djdk.attach.allowAttachSelf"]
  :plugins
  [[lein-monolith "1.10.3"]
   [lein-hiera "2.0.0"]
   [lein-environ "1.2.0"]
   [lein-cprint "1.3.3" :exclusions [mvxcvi/puget]]
   [lein-collisions "0.1.4"]
   [lein-cloverage "1.2.4"]
   [lein-codox "0.10.8"]
   [amperity-service/lein-template "MONOLITH-SNAPSHOT"]
   [mvxcvi/whidbey "2.2.1"]
   [com.jakemccrary/lein-test-refresh "0.25.0"]
   [io.aviso/pretty "1.4.4"]]

  :aliases
  {"refresh" ["do" "monolith" "each" ":refresh" "build" ":upstream" ":skip"
              :project/name ":parallel" "4" "install,"]}

  :dependencies
  [[nrepl "1.3.0"]
   [hashp "0.2.2"]
   [criterium "0.4.6"]
   [mvxcvi/puget "1.3.4"]
   [pjstadig/humane-test-output "0.11.0"]
   [io.aviso/pretty "1.4.4"]
   [clj-stacktrace "0.2.8"]]

  :deploy-repositories
  [["local-jars" "file:///Users/jeff/work/dev/local-jars/"]]

  :repl-options
  {:timeout 480000}

  :middleware [whidbey.plugin/repl-pprint
               io.aviso.lein-pretty/inject]}
 :whidbey
 {:width 150
  :map-delimiter ","
  :namespace-maps true
  :color-scheme {:nil [:blue]}
  :tag-types {java.lang.Class {'class #(symbol (.getName %))}
              java.time.Instant {'inst str}
              java.time.Duration {'time/duration str}
              java.time.LocalDate {'time/local-date str}
              java.time.LocalDateTime {'time/local-date-time str}
              java.time.LocalTime {'time/local-time str}
              java.time.Month {'time/month str}
              java.time.Period {'time/period str}
              java.time.Year {'time/year str}
              java.time.YearMonth {'time/year-month str}
              java.time.ZoneId {'time/zone-id str}
              java.time.ZoneRegion {'time/zone-region str}
              java.time.ZoneOffset {'time/zone-offset str}
              java.time.ZonedDateTime {'time/zoned-date-time str}
              'org.joda.time.DateTime {'joda/inst str}
              'org.joda.time.UTCDateTimeZone {'joda/zone str}}}

 :profiling
 {:dependencies [[com.clojure-goes-fast/clj-memory-meter "0.3.0"]
                 [com.clojure-goes-fast/clj-async-profiler "1.0.4"]
                 [com.clojure-goes-fast/clj-java-decompiler "0.3.4"]
                 [com.clojure-goes-fast/jvm-alloc-rate-meter "0.1.4"]]
  :jvm-opts ["-Djdk.attach.allowAttachSelf"]}

 :cider/repl ^:repl
 {:plugins [[lein-monolith "1.10.0" :exclusions [org.clojure/tools.logging]]
            [refactor-nrepl "3.10.0"]
            [cider/cider-nrepl "0.55.7"]]}}
