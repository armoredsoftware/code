--Packet.hs
module Packet(Packet(..))where

import Data.Binary

data Packet = CommRequest {src::String, dest::String} |
              Chat {src::String, dest::String, msg::String} |
              AttestationRequest {src::String, dest::String, attestation::String}
  deriving Read
instance Show Packet where
 show (CommRequest src dest) = "CommRequest Src: "++src++" Dest: "++dest 
 show (Chat src dest msg) = "Chat Src: "++src++" Dest: "++dest++" Msg: "++msg 
 show (AttestationRequest src dest att) = "AttestationRequest Src: "++src++" Dest: "++dest++" Att: "++att 
    




instance Binary Packet where
 put (CommRequest src dest)            = do put (0 ::Word8) 
                                            put src
                                            put dest
 put (Chat src dest msg)               = do put (1 ::Word8)
                                            put src
                                            put dest
                                            put msg
 put (AttestationRequest src dest att) = do put (2 :: Word8)
                                            put src
                                            put dest
                                            put att
 get = do t<-  get :: Get Word8
          case t of 
               0 -> do src <- get
                       dest <- get
                       return (CommRequest src dest)
               1 -> do src <- get
                       dest <- get
                       msg <- get
                       return (Chat src dest msg)
               2 -> do src <- get
                       dest <- get
                       att <- get
                       return (AttestationRequest src dest att)



