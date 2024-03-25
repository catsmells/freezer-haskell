{-# LANGUAGE OverloadedStrings #-}

import Discord
import Discord.Types
import Discord.Requests
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T

handleNewUser :: Int -> User -> DiscordHandler ()
handleNewUser count user = do
    let username = "No." <> T.pack (show count)
    _ <- liftIO $ putStrLn $ "New user joined: " <> T.unpack (userUsername user) <> " - Assigning username: " <> T.unpack username
    _ <- restCall $ ModifyGuildMember (userId user) $ UpdateGuildMemberNick Nothing (Just username) Nothing
    pure ()

runBot :: IO ()
runBot = do
    token <- T.pack <$> readFile "token.txt"
    _ <- runDiscord $ def { discordToken = token
                           , discordOnEvent = eventHandler
                           }
    pure ()
  where
    eventHandler :: Event -> DiscordHandler ()
    eventHandler (EventGuildMemberAdd member) = do
        guild <- getCurrentUserGuild
        case guild of
            Nothing -> pure ()
            Just g -> do
                users <- getGuildMembers g
                let newUserCount = length users + 1
                handleNewUser newUserCount (memberUser member)
    eventHandler _ = pure ()

main :: IO ()
main = runBot
