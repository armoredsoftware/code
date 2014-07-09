module Demo2 where

import Data.ByteString

data Mail = Actor Actor Message

data Actor = Appraiser Int
           | Attester Int
           | Measurer Int
           deriving (Show, Eq)

 
data Message = Request
             | Response
             | MeasurementRequest
             | MeasurementResponse
             | Encripted Message

--Request
type Request = (DesiredEvidence,Nonce)
type DesiredEvidence = [EvidenceDescriptor]
data EvidenceDescriptor = D0 | D1 | D2 | D3 | D4 | D5 | D6 --for now
type Nonce = Int

--Response
data Response = MetaResp (Hash, Nonce)
              | InfoResp (Evidence, Nonce)

type Hash = ByteString
data Evidence = [EvidencePiece]
data EvidencePiece = ByteString --for now 

--Measurement
type MeasurementRequest = (EvidenceDescriptor, Nonce)
type MeasurementResponse = (EvidencePiece, Nonce)

