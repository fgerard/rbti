(ns rbti.sheet-access)

; en contexto poner :data-dir /Users/fgerard/git/rbti/resources/google


(defn write-to-drive[ctx]
  (let [{:keys [sheet-id data-dir]} ctx
        httpTransport (com.google.api.client.googleapis.javanet.GoogleNetHttpTransport/newTrustedTransport)
        jsonFactory   (com.google.api.client.json.jackson2.JacksonFactory/getDefaultInstance)]
    (letfn [(authorize []
              (let [_ (clojure.java.io/make-parents (str data-dir "/.store/oauth2"))
                    dataStoreDir     (java.io.File. (str data-dir "/.store/oauth2"))
                    dataStoreFactory (com.google.api.client.util.store.FileDataStoreFactory. dataStoreDir)

                    scopes           (java.util.Arrays/asList (into-array String ["https://www.googleapis.com/auth/spreadsheets"]))
                    clientSecrets    (com.google.api.client.googleapis.auth.oauth2.GoogleClientSecrets/load
                                      jsonFactory
                                      (java.io.InputStreamReader.
                                       (clojure.java.io/input-stream (str data-dir "/client_secret_rbtiV01.json"))))
                    flow             (-> (com.google.api.client.googleapis.auth.oauth2.GoogleAuthorizationCodeFlow$Builder.
                                          httpTransport jsonFactory clientSecrets scopes)
                                         (.setDataStoreFactory dataStoreFactory)
                                         (.setAccessType "offline")
                                         (.build))
                    credential       (-> (com.google.api.client.extensions.java6.auth.oauth2.AuthorizationCodeInstalledApp. flow (com.google.api.client.extensions.jetty.auth.oauth2.LocalServerReceiver.))
                                         (.authorize "user"))]
                credential))
            (sheet-service []
              (let [credential (authorize)]
                (-> (com.google.api.services.sheets.v4.Sheets$Builder. httpTransport jsonFactory credential)
                    (.setApplicationName "iwRobot")
                    (.build))))
            (write [spreadsheet-id]
              (let [service (sheet-service)
                    row  (-> (.spreadsheets service)
                             (.values)
                             (.get spreadsheet-id "Hoja 1!A:A")
                             (.execute)
                             (.getValues)
                             (count)
                             (inc))
                    body (-> (com.google.api.services.sheets.v4.model.BatchUpdateValuesRequest.)
                             (.setValueInputOption "USER_ENTERED")
                             (.setData [(-> (com.google.api.services.sheets.v4.model.ValueRange.)
                                            (.setRange (str "Hoja 1!" row ":" row))
                                            ;; Set new values here
                                            (.setValues [["test1" "test2"]]))]))]
                (-> (.spreadsheets service)
                    (.values)
                    (.batchUpdate spreadsheet-id body)
                    (.execute))))]
      (write sheet-id))))


(defn -main
 "I don't do a whole lot ... yet."
 [& args]
 (write-to-drive {:sheet-id "184Ajr0c0RXOyh5T43PSWYVO0OcyTKdlTLFFg96wt6cM" :data-dir "/Users/fgerard/git/rbti/resources/google"}))
