{-# LANGUAGE OverloadedStrings #-}

import Discord
import Discord.Types
import Discord.Requests
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Char (isDigit)
import Text.Read (readMaybe)

-- Function to handle new user joining
handleNewUser :: Int -> User -> DiscordHandler ()
handleNewUser count user = do
    let username = "No." <> T.pack (show count)
    _ <- liftIO $ putStrLn $ "New user joined: " <> T.unpack (userUsername user) <> " - Assigning username: " <> T.unpack username
    _ <- restCall $ ModifyGuildMember (userId user) $ UpdateGuildMemberNick Nothing (Just username) Nothing
    pure ()

-- Function to handle commands
handleCommand :: Message -> DiscordHandler ()
handleCommand msg = do
    case parseCommand (messageContent msg) of
        Just ("!hello", _) -> sendMessage (messageChannel msg) "Hello!"
        Just ("!rename", args) -> handleRenameCommand (messageChannel msg) args
        Just ("!createchannel", args) -> handleCreateChannelCommand (messageGuild msg) args
        Just ("!grantrole", args) -> handleGrantRoleCommand (messageGuild msg) (messageChannel msg) args
        _ -> pure ()

-- Function to handle rename command
handleRenameCommand :: ChannelId -> [T.Text] -> DiscordHandler ()
handleRenameCommand channel args = do
    case args of
        [newName] -> do
            modifyNickname newName
            sendMessage channel $ "Your nickname has been changed to: " <> newName
        _ -> sendMessage channel "Invalid arguments. Usage: !rename <new_name>"

-- Function to handle create channel command
handleCreateChannelCommand :: GuildId -> [T.Text] -> DiscordHandler ()
handleCreateChannelCommand guildId args = do
    case args of
        [channelName] -> do
            _ <- restCall $ CreateGuildChannel guildId channelName (ChannelText (def { channelName = channelName }))
            sendMessage channel $ "Channel created: " <> channelName
        _ -> sendMessage channel "Invalid arguments. Usage: !createchannel <channel_name>"

-- Function to handle grant role command
handleGrantRoleCommand :: GuildId -> ChannelId -> [T.Text] -> DiscordHandler ()
handleGrantRoleCommand guildId channel args = do
    case args of
        [mentionedUser] -> do
            users <- restCall $ GetGuildMembers guildId (GuildMembersTiming 1 1000)
            case findUserByName users mentionedUser of
                Just user -> do
                    role <- findOrCreateRole guildId "lq"
                    _ <- restCall $ AddGuildMemberRole guildId (userId user) role
                    sendMessage channel $ "Role 'lq' granted to user: " <> userUsername user
                Nothing -> sendMessage channel "User not found."
        _ -> sendMessage channel "Invalid arguments. Usage: !grantrole <user_mention>"

-- Function to parse commands
parseCommand :: T.Text -> Maybe (T.Text, [T.Text])
parseCommand msgContent =
    case T.words msgContent of
        (cmd:args) | T.head cmd == '!' -> Just (cmd, args)
        _ -> Nothing

-- Find user by name
findUserByName :: [GuildMember] -> T.Text -> Maybe GuildMember
findUserByName users name = find (\user -> userUsername (userUser user) == name) users

-- Find or create role by name
findOrCreateRole :: GuildId -> T.Text -> DiscordHandler Role
findOrCreateRole guildId roleName = do
    roles <- restCall $ GetGuildRoles guildId
    case find (\role -> roleName == role_name role) roles of
        Just role -> pure role
        Nothing -> restCall (CreateGuildRole guildId roleName)

-- Function to run the bot
runBot :: IO ()
runBot = do
    token <- T.pack <$> readFile "token.txt" -- Replace "token.txt" with the file containing your bot token
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

-- Function to modify user's nickname
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
