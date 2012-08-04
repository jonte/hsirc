module Network.IRC.Commands (
    -- * Types
    Channel
  , Password

    -- * IRC Functions
  , nick
  , user
  , joinChan
  , part
  , quit
  , privmsg
  , kick
  , channelMode
  , userMode
  , topic
  ) where

import Network.IRC.Base
import Data.Maybe (maybeToList)

type Channel     = String
type Password    = String
type Reason      = String
type UserMask    = String
type ChannelMode = Char
type UserMode    = Char

mkMessage           :: String -> [Parameter] -> Message
mkMessage cmd params = Message Nothing cmd params

nick  :: UserName -> Message
nick u = mkMessage "NICK" [u]

user        :: UserName -> ServerName -> ServerName -> RealName -> Message
user u h s r = mkMessage "USER" [u,h,s,r]

joinChan  :: Channel -> Message
joinChan c = mkMessage "JOIN" [c]

kick :: Channel -> UserName -> Maybe Reason -> Message
kick c u (Just r) = mkMessage "KICK" [c,u,r]
kick c u Nothing  = mkMessage "KICK" [c,u]

part  :: Channel -> Message
part c = mkMessage "PART" [c]

quit :: Maybe String -> Message
quit (Just m) = mkMessage "QUIT" [m]
quit Nothing  = mkMessage "QUIT" []

privmsg    :: String -> String -> Message
privmsg c m = mkMessage "PRIVMSG" [c,m]

channelMode :: Channel -> [ChannelMode] -> Maybe Int -> 
                Maybe UserName -> Maybe UserMask -> Message
channelMode c cm l u um = mkMessage "MODE" params
    where params = [c, cm] ++ maybeToList (fmap show l) ++ maybeToList u ++ maybeToList um

userMode :: UserName -> [UserMode] -> Message
userMode n m = mkMessage "MODE" [n,m]

topic :: Channel -> String -> Message
topic c t = mkMessage "TOPIC" [c,t]
