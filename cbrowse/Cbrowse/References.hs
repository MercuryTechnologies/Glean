{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Cbrowse.References where

import Cbrowse.Api.Types

-- TODO: we have to figure out how to either:
-- - enumerate all entities in a file and give it to the client
--   - then the client needs to be able to actually *refer* to the entity in a
--     way the server can make any sense of, which I don't have any idea how to
--     do.
-- - ask the server for symbols with a file offset in their range on goto
--   reference operations. is that even possible???
--
{-

More specifically, we have to take this target.hs.name thing and allow it to be
recreated server side again (or obtain one anew), because that's what we need
to use to query references to an object under the cursor.

Definite problem that needs to be solved: how do you reference a
codemarkup.Entity from a client without just Encoding The Entire Thing in
thrift (... is that the way?)

The object under the cursor can either be a defn site or a usage site.

{
  "id": 9490341,
  "key": {
    "target": {
      "hs": {
        "name": {
          "id": 1291,
          "key": {
            "occ": { "id": 1290, "key": { "name": "Text", "namespace_": 3 } },
            "mod": { "id": 1289, "key": { "name": { "id": 1287, "key": "Data.Text.Internal" }, "unit": { "id": 1288, "key": "text-2.1.1-c86e" } } },
            "sort": { "external": { } }
          }
        }
      }
    },
    "file": { "id": 9600, "key": "src/Model/AgentAssist/Types.hs" },
    "range": { "span": { "start": 320, "length": 4 } }
  }
}
-}
