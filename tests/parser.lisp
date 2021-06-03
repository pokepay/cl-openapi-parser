(in-package :openapi-parser-tests)

(deftest yaml
  (ok (gethash "200" (yaml:parse "{'200': 'ok'}")))
  (ok (gethash 200 (yaml:parse "{200: 'ok'}"))))

(deftest parse-file
  )
