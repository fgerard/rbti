(defproject rbti "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.namespace "0.2.11"]
                 [com.cerner/clara-rules "0.16.0"]

                 [com.google.apis/google-api-services-oauth2 "v2-rev131-1.23.0"]
                 [com.google.apis/google-api-services-sheets "v4-rev488-1.23.0"]
                 [com.google.http-client/google-http-client-jackson2 "1.23.0"]
                 [com.google.oauth-client/google-oauth-client-jetty "1.23.0"]]
  :aot [rbti.core]

  :main rbti.core
)
