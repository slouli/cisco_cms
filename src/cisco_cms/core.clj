(ns cisco-cms.core
  (:require [clj-http.client :as client]
            [clojure.zip :as zip]
            [clojure.data.xml :as xml]
            [clojure.data.zip.xml :as zip-xml])
  (:gen-class))

(def coSpaceId "283fd0ff-1d88-40e7-87db-e03dffbb0ff5")
(def callLegProfileId "6a793e5f-1811-45b2-bb0c-b40d3713a659")
(def cms-url "https://cawl-cms1.opentext.net:445/api/v1")
(def callLegProfilesUrl (str cms-url "/callLegProfiles"))
(def calls (str cms-url "/calls"))

(defn coSpaceUrl
  [coSpaceId]
  (str cms-url "/coSpaces/" coSpaceId))

(defn callLegProfileUrl
  [callLegProfileId]
  (str cms-url "/callLegProfiles/" callLegProfileId))

(defn callLegsUrl
  [callId]
  (str cms-url "/calls/" callId "/callLegs"))

(defn callLegUrl
  [callLegId]
  (str cms-url "/callLegs/" callLegId))

(defn range2keyword
  [coll]
  (map keyword (map str (map inc coll))))

(defn participantUrl
  [pId]
  (str cms-url "/participants/" pId))

(defn participantsUrl
  [callId]
  (str cms-url "/calls/" callId "/participants"))

(defn participant2CallLegUrl
  [pId]
  (str cms-url "/participants/" pId "/callLegs"))

(defn str2zip
  [xml]
  (zip/xml-zip (xml/parse-str xml)))

(defn request
  [method url body]
  (let [destruct (fn [[key value]] (str (name key) "=" value))
        _auth {:basic-auth "<creds>"}
        _getUrl (reduce str (str url "?") (interpose "&" (map destruct body)))
        _getParams _auth
        _postParams (conj _auth {:form-params body})
        ops {:get {:method client/get :url _getUrl :params _getParams}
             :post {:method client/post :url url :params _postParams}
             :put {:method client/put :url url :params _postParams}}]
    ((:method (method ops)) (:url (method ops)) (:params (method ops)))))

(defn participantIds
  [callId]
  (let [xml (:body (request :get (participantsUrl callId) {}))
        zipped-xml (str2zip xml)
        locs  (zip-xml/xml-> zipped-xml :participants :participant)
        pIds (map zip-xml/attr locs (repeat :id))]
    pIds))

(defn pId2CallLeg
  [pId]
  (let [xml (:body (request :get (participant2CallLegUrl pId) {}))
        zipped-xml (str2zip xml)
        loc (zip-xml/xml1-> zipped-xml :callLegs :callLeg)
        callLegId (zip-xml/attr loc :id)]
    {(keyword callLegId) pId}))

(defn callMap
  [coSpaceId]
  (let [xml (:body (request :get calls {:coSpaceFilter coSpaceId}))
        zipped-xml (str2zip xml)
        loc  (zip-xml/xml1-> zipped-xml :calls :call)]
    {(keyword (zip-xml/xml1-> loc :name zip-xml/text)) (zip-xml/attr loc :id)}))

(defn muteStatus
  [callLegId]
  (let [xml (:body (request :get (callLegUrl callLegId) {}))
        zipped-xml (str2zip xml)
        isMuted? (zip-xml/xml1-> zipped-xml :callLeg :configuration :rxAudioMute zip-xml/text)]
    isMuted?
  ))

;;Maybe callLegMap function can be more elegant
(defn callLegMap
  [callId]
  (let [xml (:body (request :get (callLegsUrl callId) {}))
        zipped-xml (str2zip xml)
        locs (zip-xml/xml-> zipped-xml :callLegs :callLeg)
        participants (map zip-xml/xml1-> locs (repeat :remoteParty) (repeat zip-xml/text))
        callLegIds (map zip-xml/attr locs (repeat :id))
        isMuted? (map muteStatus callLegIds)
        enum (range2keyword (range))
        keyLists (repeat '(:id :name :muted))
        tuples (map list callLegIds participants isMuted?)
        gen-pMap (fn [keyList tuple] (zipmap keyList tuple))]
    (zipmap enum (map gen-pMap keyLists tuples))))

(defn isMuted?
  [callLegId]
  (let [xml (:body (request :get (callLegUrl callLegId) {}))
        zipped-xml (str2zip xml)
        muted? (zip-xml/xml1-> zipped-xml :callLeg :configuration :rxAudioMute zip-xml/text)]
    muted?))

(defn toggleMute
  [callLegId pId isMuted?]
  (let [toggleMap {:true "false" :false "true"}
        importanceMap {:true 10 :false 1}
        mute (keyword (isMuted? callLegId))]
    (request :put (callLegUrl callLegId) {:rxAudioMute (mute toggleMap)})
    (request :put (participantUrl pId) {:importance (mute importanceMap)})))

;;MAIN LOOP
(loop []

  (def callId (-> (callMap coSpaceId)
                  :OKO))

  (def pIds (participantIds callId))

  (def callLegId2pId
    (into {} (map pId2CallLeg pIds)))

  ;;(println callLegId2pId)

  (def callLegs (-> (callMap coSpaceId)
                    :OKO
                    callLegMap))

  (doall (for [[k v] callLegs] (println k v)))

  (def getUserInput
    (do
      (print "\nSelect Participant: ")
      (flush)
      (read-line)))

  (def selectedLeg
    (-> (keyword getUserInput)
        callLegs
        :id))

  (def selectedpId
    (cond
      (not (nil? selectedLeg)) ((keyword selectedLeg) callLegId2pId)
      :else "REFRESH"))

  ;;(println selectedLeg)
  (cond
    (nil? selectedLeg) (println "REFRESH")
    :else (toggleMute selectedLeg selectedpId isMuted?))

  (recur))



;;MODIFY callLegProfile ASSOCIATION WITH THE coSpace
;;(request :put (coSpaceUrl coSpaceId) {:callLegProfile callLegProfileId})
;;(request :put (callLegProfileUrl callLegProfileId) {:participantCounter "never"})

(comment
  Use this to update OKO callLegProfile
  (request :put callLegProfilesUrl {:name "OKO Profile"
                                    :rxAudioMute "true"
                                    :needsActivation "false"
                                    :defaultLayout "speakerOnly"
                                    :changeLayoutAllowed "true"
                                    :participantLabels "false"
                                    :joinToneParticipantThreshold "0"
                                    :leaveToneParticipantThreshold "0"
                                    })
  )


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
