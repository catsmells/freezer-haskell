{-# LANGUAGE OverloadedStrings #-}

import Discord
import Discord.Types
import Discord.Requests
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Char (isDigit)
import Text.Read (readMaybe)

handleNewUser :: Int -> User -> DiscordHandler ()
handleNewUser count user = do
    let username = "No." <> T.pack (show count)
    _ <- liftIO $ putStrLn $ "New user joined: " <> T.unpack (userUsername user) <> " - Assigning username: " <> T.unpack username
    _ <- restCall $ ModifyGuildMember (userId user) $ UpdateGuildMemberNick Nothing (Just username) Nothing
    pure ()


handleCommand :: Message -> DiscordHandler ()
handleCommand msg = do
    case parseCommand (messageContent msg) of
        Just ("~hello", _) -> sendMessage (messageChannel msg) "Hello!"
        Just ("~rename", args) -> handleRenameCommand (messageChannel msg) args
        _ -> pure ()

handleRenameCommand :: ChannelId -> [T.Text] -> DiscordHandler ()
handleRenameCommand channel args = do
    case args of
        [newName] -> do
            modifyNickname newName
            sendMessage channel $ "Your nickname has been changed to: " <> newName
        _ -> sendMessage channel "Invalid arguments. Usage: ~rename <new_name>"


parseCommand :: T.Text -> Maybe (T.Text, [T.Text])
parseCommand msgContent =
    case T.words msgContent of
        (cmd:args) | T.head cmd == '~' -> Just (cmd, args)
        _ -> Nothing

runBot :: IO ()
runBot = do
    token <- T.pack <$> readFile "token.txt"
    _ <- runDiscord $ def { discordToken = token
                           , discordOnEvent = eventHandler
                           }
    pure ()
  where
    eventHandler :: Event -> DiscordHandler ()
    eventHandler (EventMessageCreate msg) = do
        handleCommand msg
    eventHandler (EventGuildMemberAdd member) = do
        guild <- getCurrentUserGuild
        case guild of
            Nothing -> pure ()
            Just g -> do
                users <- getGuildMembers g
                let newUserCount = length users + 1
                handleNewUser newUserCount (memberUser member)
    eventHandler _ = pure ()

modifyNickname :: T.Text -> DiscordHandler ()
modifyNickname newName = do
    user <- getCurrentUser
    case user of
        Nothing -> pure ()
        Just u -> do
            _ <- restCall $ ModifyGuildMember (userId u) $ UpdateGuildMemberNick Nothing (Just newName) Nothing
            pure ()

main :: IO ()
main = runBot
